use std::collections::BTreeSet;

use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::Result;

use crate::parser::EPSILON;

// a map for predefined symbols
fn predefined_symbols(litstr: &syn::LitStr) -> Option<(&str, &str)> {
    match litstr.value().as_str() {
        "." => Some(("Dot", r"\.")),
        "+" => Some(("Plus", r"\+")),
        "-" => Some(("Minus", r"-")),
        "*" => Some(("Star", r"\*")),
        "%" => Some(("Modulo", r"%")),
        "/" => Some(("Slash", r"/")),
        ">>" => Some(("RShift", r">>")),
        "<<" => Some(("LShift", r"<<")),
        "==" => Some(("Equal", r"==")),
        "!=" => Some(("NotEqual", r"!=")),
        "<" => Some(("Less", r"<")),
        "<=" => Some(("LessEqual", r"<=")),
        ">" => Some(("Greater", r">")),
        ">=" => Some(("GreaterEqual", r">=")),
        "&&" => Some(("And", r"&&")),
        "||" => Some(("Or", r"||")),
        "!" => Some(("Not", r"!")),
        "=" => Some(("Assign", r"=")),
        "+=" => Some(("PlusAssign", r"\+=")),
        "-=" => Some(("MinusAssign", r"-=")),
        "*=" => Some(("StarAssign", r"\*=")),
        "/=" => Some(("SlashAssign", r"/=")),
        "%=" => Some(("ModuloAssign", r"%=")),
        "[" => Some(("LBracket", r"\[")),
        "]" => Some(("RBracket", r"\]")),
        "{" => Some(("LBrace", r"\{")),
        "}" => Some(("RBrace", r"\}")),
        "(" => Some(("LParen", r"\(")),
        ")" => Some(("RParen", r"\)")),
        "," => Some(("Comma", r",")),
        ";" => Some(("Semicolon", r";")),
        "if" => Some(("If", r"if")),
        "else" => Some(("Else", r"else")),
        "while" => Some(("While", r"while")),
        "for" => Some(("For", r"for")),
        "do" => Some(("Do", r"do")),
        "let" => Some(("Let", r"let")),
        "return" => Some(("Return", r"return")),
        "break" => Some(("Break", r"break")),
        "continue" => Some(("Continue", r"continue")),
        _ => None,
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct LexRule {
    pub name: String,
    pub regex: String,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct SyntaxRule {
    pub lhs: String,
    pub rhs: Vec<String>,
}

#[derive(Debug)]
pub(crate) struct GrammarDef {
    pub lex_rules: Vec<LexRule>,
    pub syntax_rules: Vec<SyntaxRule>,
    pub syntax_start: String,
}

mod kw {
    syn::custom_keyword!(lex);
    syn::custom_keyword!(syntax);
    syn::custom_keyword!(start);
}

fn parse_lex_rules(inner: &ParseStream) -> Result<Vec<LexRule>> {
    let mut rules = Vec::new();
    while !inner.is_empty() {
        while !inner.peek(syn::Token![;]) {
            let name_span = inner.span();
            let name = inner.parse::<syn::Ident>()?.to_string();
            inner.parse::<syn::Token![:]>()?;
            let regex_span = inner.span();
            let regex = inner.parse::<syn::LitStr>()?.value();
            let _ = regex::Regex::new(&regex)
                .map_err(|_| syn::Error::new(regex_span, format!("invalid regex: {}", regex)))?;
            if rules.iter().any(|rule: &LexRule| rule.name == name) {
                return Err(syn::Error::new(name_span, "duplicate lex rule"));
            }
            rules.push(LexRule { name, regex });
        }
        inner.parse::<syn::Token![;]>()?;
    }
    Ok(rules)
}

fn parse_syntax_rules(
    inner: &ParseStream,
    lex_rules: &mut Vec<LexRule>,
) -> Result<Vec<SyntaxRule>> {
    let mut syntax_rules = Vec::new();
    while !inner.is_empty() {
        let lhs = inner.parse::<syn::Ident>()?.to_string();
        inner.parse::<syn::Token![=>]>()?;
        let rhs_group;
        syn::braced!(rhs_group in inner);
        while !rhs_group.is_empty() {
            let mut rhs: Vec<String> = Vec::new();
            let start_span = rhs_group.span();
            while !rhs_group.peek(syn::Token![;]) {
                let symbol = if rhs_group.peek(syn::Ident) {
                    rhs_group.parse::<syn::Ident>()?.to_string()
                } else if rhs_group.peek(syn::LitStr) {
                    let litstr = rhs_group.parse::<syn::LitStr>()?;
                    let (symbol, re) = predefined_symbols(&litstr).expect(format!("invalid symbol: {}", litstr.value()).as_str());
                    if !lex_rules.iter().any(|rule: &LexRule| rule.name == symbol) {
                        lex_rules.push(LexRule {
                            name: symbol.into(),
                            regex: re.into(),
                        });
                    }
                    symbol.into()
                } else {
                    return Err(syn::Error::new(
                        rhs_group.span(),
                        "expect an Ident or LitStr",
                    ));
                };
                rhs.push(symbol);
            }
            let end_span = rhs_group.span();
            rhs_group.parse::<syn::Token![;]>()?;
            let curr_rule = SyntaxRule {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            };
            if syntax_rules.contains(&curr_rule) {
                return Err(syn::Error::new(
                    start_span.join(end_span).unwrap(),
                    format!("duplicate syntax rule: `{}` => {:?}", lhs, rhs),
                ));
            }
            syntax_rules.push(curr_rule);
        }
    }
    Ok(syntax_rules)
}

impl Parse for GrammarDef {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::lex>()?;
        let inner;
        syn::braced!(inner in input);
        let mut lex_rules = parse_lex_rules(&&inner)?;
        input.parse::<kw::syntax>()?;
        let inner;
        syn::braced!(inner in input);
        inner.parse::<kw::start>()?;
        inner.parse::<syn::Token![=>]>()?;
        let syntax_start = inner.parse::<syn::Ident>()?.to_string();
        inner.parse::<syn::Token![;]>()?;
        let syntax_rules = parse_syntax_rules(&&inner, &mut lex_rules)?;
        let grammar = Self {
            lex_rules,
            syntax_start,
            syntax_rules,
        };
        Ok(grammar)
    }
}

impl std::fmt::Display for GrammarDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "lex {{")?;
        for rule in &self.lex_rules {
            writeln!(f, "    {} : \"{}\";", rule.name, rule.regex)?;
        }
        writeln!(f, "}}")?;
        writeln!(f, "syntax {{")?;
        for rule in &self.syntax_rules {
            write!(f, "    {} => ", rule.lhs)?;
            for symbol in &rule.rhs {
                write!(f, " {}", symbol)?;
            }
            writeln!(f)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl GrammarDef {
    pub fn check_well_defined(&self) {
        let mut terminals = self
            .lex_rules
            .iter()
            .map(|rule| &rule.name)
            .collect::<BTreeSet<_>>();
        let nonterminals = self
            .syntax_rules
            .iter()
            .map(|rule| &rule.lhs)
            .collect::<BTreeSet<_>>();
        let epsilon = EPSILON.to_string();
        terminals.insert(&epsilon);
        // check terminals & nonterminals
        for common_symbol in terminals.intersection(&nonterminals) {
            panic!("{} is both a terminal and a nonterminal", common_symbol);
        }
        if !nonterminals.contains(&self.syntax_start) {
            panic!(
                "start symbol: {} is not defined in any lhs of syntax rules",
                self.syntax_start
            );
        }
        // check all symbol in syntax rules are defined
        for rule in &self.syntax_rules {
            for symbol in &rule.rhs {
                if !terminals.contains(symbol) && !nonterminals.contains(symbol) {
                    panic!("{} is not defined", symbol);
                }
            }
        }
    }
}
