use proc_macro2::TokenStream;
use std::collections::{HashMap, HashSet};

use crate::grammar::GrammarDef;

// For efficiency, we use the idex of the symbol in the grammar's symbol list
// to distinguish symbols.
// So it can be copy and moved effeciently in stack.
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub(crate) enum Symbol {
    Terminal(usize),
    NonTerminal(usize),
}

impl Symbol {
    fn index(&self) -> usize {
        match self {
            Symbol::Terminal(id) => *id,
            Symbol::NonTerminal(id) => *id,
        }
    }
    fn is_terminal(&self) -> bool {
        match self {
            Symbol::Terminal(_) => true,
            Symbol::NonTerminal(_) => false,
        }
    }
    fn is_nonterminal(&self) -> bool {
        match self {
            Symbol::Terminal(_) => false,
            Symbol::NonTerminal(_) => true,
        }
    }
}

const AUGMENTED_START: &str = r"Goal";
const EOF: &str = r"EOF";
pub const EPSILON: &str = r"EPSILON";

const AUGMENTED_START_SYMBOL: Symbol = Symbol::NonTerminal(0);
const EOF_SYMBOL: Symbol = Symbol::Terminal(1);
const EPSILON_SYMBOL: Symbol = Symbol::Terminal(2);

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub(crate) struct Production {
    lhs: Symbol,
    rhs: Vec<Symbol>,
}

#[derive(Debug)]
pub(crate) struct Grammar {
    pub start: Symbol,
    pub symbols: Vec<String>,
    pub name2symbol: HashMap<String, Symbol>,
    pub terminals: Vec<Symbol>,
    pub nonterminals: Vec<Symbol>,
    pub productions: Vec<Production>,
    pub follow_set: HashMap<Symbol, HashSet<Symbol>>,
    pub first_set: HashMap<Symbol, HashSet<Symbol>>,
}

impl Grammar {
    fn new() -> Self {
        Self {
            start: Symbol::NonTerminal(0),
            symbols: vec![
                AUGMENTED_START.to_owned(),
                EOF.to_owned(),
                EPSILON.to_owned(),
            ],
            name2symbol: HashMap::from_iter(vec![
                (AUGMENTED_START.to_owned(), AUGMENTED_START_SYMBOL),
                (EOF.to_owned(), EOF_SYMBOL),
                (EPSILON.to_owned(), EPSILON_SYMBOL),
            ]),
            terminals: vec![EOF_SYMBOL, EPSILON_SYMBOL],
            nonterminals: vec![AUGMENTED_START_SYMBOL],
            productions: vec![],
            follow_set: HashMap::new(),
            first_set: HashMap::new(),
        }
    }

    fn augmented_start(&self) -> Symbol {
        self.start
    }

    fn new_term(&mut self, name: &str) -> Symbol {
        if self.name2symbol.contains_key(name) {
            return *self.name2symbol.get(name).unwrap();
        }
        let id = self.symbols.len();
        self.symbols.push(name.to_owned());
        let symbol = Symbol::Terminal(id);
        self.terminals.push(symbol);
        self.name2symbol.insert(name.to_owned(), symbol);
        symbol
    }

    fn new_nonterm(&mut self, name: &str) -> Symbol {
        if self.name2symbol.contains_key(name) {
            return *self.name2symbol.get(name).unwrap();
        }
        let id = self.symbols.len();
        self.symbols.push(name.to_owned());
        let symbol = Symbol::NonTerminal(id);
        self.nonterminals.push(symbol);
        self.name2symbol.insert(name.to_owned(), symbol);
        symbol
    }

    fn new_production(&mut self, lhs: Symbol, rhs: &Vec<Symbol>) {
        let rhs = rhs.iter().filter(|s| s != &&EPSILON_SYMBOL).cloned().collect::<Vec<_>>();
        self.productions.push(Production { lhs, rhs });
    }

    fn init_first_set(&mut self) {
        for terminal in &self.terminals {
            self.first_set
                .insert(*terminal, HashSet::from_iter(vec![*terminal]));
        }
        for nonterminal in &self.nonterminals {
            self.first_set.insert(*nonterminal, HashSet::new());
        }
        let mut changed = true;
        while changed {
            changed = false;
            for p in &self.productions {
                let lhs = p.lhs;
                let rhs = self.first_of(&p.rhs);
                if !self.first_set.get(&lhs).unwrap().is_superset(&rhs) {
                    self.first_set.get_mut(&lhs).unwrap().extend(rhs);
                    changed = true;
                }
            }
        }
    }

    fn init_follow_set(&mut self) {
        for nonterminal in &self.nonterminals {
            self.follow_set.insert(*nonterminal, HashSet::new());
        }
        self.follow_set
            .get_mut(&self.augmented_start())
            .unwrap()
            .insert(EOF_SYMBOL);
        let mut changed = true;
        while changed {
            changed = false;
            for p in &self.productions {
                let mut trailer: HashSet<Symbol> = self.follow_set.get(&p.lhs).unwrap().clone();
                for symbol in p.rhs.iter().rev() {
                    match symbol {
                        Symbol::Terminal(_) => {
                            trailer = HashSet::from_iter(vec![*symbol]);
                        }
                        Symbol::NonTerminal(_) => {
                            if !self.follow_set.get(&symbol).unwrap().is_superset(&trailer) {
                                changed = true;
                                self.follow_set
                                    .get_mut(&symbol)
                                    .unwrap()
                                    .extend(trailer.clone());
                            }
                            trailer.extend(self.first_set.get(&symbol).unwrap());
                            trailer.remove(&EPSILON_SYMBOL);
                        }
                    }
                }
            }
        }
    }

    pub fn first_of(&self, symbols: &Vec<Symbol>) -> HashSet<Symbol> {
        let mut ret = HashSet::new();
        let mut trailing = true;
        for s in symbols {
            ret.extend(self.first_set.get(s).unwrap());
            ret.remove(&EPSILON_SYMBOL);
            if !self.first_set.get(&s).unwrap().contains(&EPSILON_SYMBOL) {
                trailing = false;
                break;
            }
        }
        if trailing {
            ret.insert(EPSILON_SYMBOL);
        }
        ret
    }

    #[allow(dead_code)]
    pub fn print_first_set(&self) {
        for (symbol, first_set) in &self.first_set {
            eprint!("FIRST({}) = {{", self.symbols[symbol.index()]);
            for first in first_set {
                eprint!(" {},", self.symbols[first.index()]);
            }
            eprintln!(" }}");
        }
    }

    #[allow(dead_code)]
    pub fn print_follow_set(&self) {
        for (symbol, follow_set) in &self.follow_set {
            eprint!("FOLLOW({}) = {{", self.symbols[symbol.index()]);
            for follow in follow_set {
                eprint!(" {},", self.symbols[follow.index()]);
            }
            eprintln!(" }}");
        }
    }
}

// From GrammarDef to Grammar
impl From<&GrammarDef> for Grammar {
    fn from(def: &GrammarDef) -> Self {
        // we have check all symbols are well defined
        let mut grammar = Grammar::new();
        // term
        for rule in &def.lex_rules {
            grammar.new_term(&rule.name);
        }
        // nonterm
        let augmented_start = grammar.augmented_start();
        let origin_start = grammar.new_nonterm(&def.syntax_start);
        grammar.new_production(augmented_start, &vec![origin_start, EOF_SYMBOL]);
        for rule in &def.syntax_rules {
            grammar.new_nonterm(&rule.lhs);
        }
        // productions
        for rule in &def.syntax_rules {
            let lhs = if let Some(symbol) = grammar.name2symbol.get(&rule.lhs) {
                // lhs must be nonterm
                match symbol {
                    Symbol::Terminal(_) => unreachable!(),
                    Symbol::NonTerminal(_) => *symbol,
                }
            } else {
                unreachable!()
            };
            let rhs = rule
                .rhs
                .iter()
                .map(|symbol| {
                    if let Some(symbol) = grammar.name2symbol.get(symbol) {
                        *symbol
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Vec<_>>();
            grammar.new_production(lhs, &rhs);
        }
        grammar.init_first_set();
        grammar.init_follow_set();
        grammar
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Item {
    production: Production,
    dot: usize,
    lookahead: Symbol,
}

impl Item {
    fn follow_dot(&self) -> Option<Symbol> {
        if self.dot < self.production.rhs.len() {
            Some(self.production.rhs[self.dot])
        } else if self.dot == self.production.rhs.len() {
            None
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug)]
enum Action {
    Shift(usize),
    Reduce(Production),
    Accept,
}

pub(crate) struct ParserGenerator {
    grammar: Grammar,
    canonical_collection_set: Vec<HashSet<Item>>,
    action: HashMap<(usize, Symbol), Action>,
    goto: HashMap<(usize, Symbol), usize>,
}

impl ParserGenerator {
    pub fn new(grammar: Grammar) -> Self {
        let mut parser_gen = Self {
            grammar,
            canonical_collection_set: Vec::new(),
            action: HashMap::new(),
            goto: HashMap::new(),
        };
        parser_gen.init_canonical_collection_set();
        parser_gen.init_action_goto();
        parser_gen
    }

    #[allow(dead_code)]
    fn print_cc(&self) {
        for (i, cc) in self.canonical_collection_set.iter().enumerate() {
            eprintln!("I{}:", i);
            for item in cc {
                self.print_item(item);
            }
        }
    }

    #[allow(dead_code)]
    fn print_item(&self, item: &Item) {
        eprint!("{} -> ", self.grammar.symbols[item.production.lhs.index()]);
        for (i, symbol) in item.production.rhs.iter().enumerate() {
            if i == item.dot {
                eprint!(" . ");
            }
            eprint!("{} ", self.grammar.symbols[symbol.index()]);
        }
        if item.dot == item.production.rhs.len() {
            eprint!(" . ");
        }
        eprintln!(", {}", self.grammar.symbols[item.lookahead.index()]);
    }

    fn init_canonical_collection_set(&mut self) {
        // cc stands for canonical collection
        let cc0 = self.closure(&HashSet::from_iter(vec![Item {
            production: self.grammar.productions[0].clone(),
            dot: 0,
            lookahead: EOF_SYMBOL,
        }]));
        self.canonical_collection_set.push(cc0);
        let mut changed = true;
        while changed {
            changed = false;
            let mut update = Vec::new();
            for cc in self.canonical_collection_set.iter() {
                for symbol in self.grammar.symbols.iter() {
                    let goto = self.goto(cc, self.grammar.name2symbol.get(symbol).unwrap());
                    update.push(goto);
                }
            }
            for cc in update {
                if !self.canonical_collection_set.contains(&cc) {
                    self.canonical_collection_set.push(cc);
                    changed = true;
                }
            }
        }
    }

    fn init_action_goto(&mut self) {
        for (i, cc) in self.canonical_collection_set.iter().enumerate() {
            for item in cc {
                if item.production.lhs == self.grammar.augmented_start()
                    && item.follow_dot().is_none()
                    && item.lookahead == EOF_SYMBOL
                {
                    self.action.insert((i, item.lookahead), Action::Accept);
                } else if item.follow_dot().is_none() {
                    let lookahead = item.lookahead;
                    self.action
                        .insert((i, lookahead), Action::Reduce(item.production.clone()));
                } else if item.follow_dot().unwrap().is_terminal() {
                    let lookahead = item.follow_dot().unwrap();
                    let goto = self.goto(cc, &lookahead);
                    let j = self
                        .canonical_collection_set
                        .iter()
                        .position(|cc| cc == &goto)
                        .unwrap();
                    self.action.insert((i, lookahead), Action::Shift(j));
                }
            }
            for nonterm in self.grammar.nonterminals.iter() {
                let goto = self.goto(cc, nonterm);
                let j = self
                    .canonical_collection_set
                    .iter()
                    .position(|cc| cc == &goto)
                    .unwrap();
                self.goto.insert((i, *nonterm), j);
            }
        }
    }

    fn closure(&self, items: &HashSet<Item>) -> HashSet<Item> {
        let mut closure = items.clone();
        let mut changed = true;
        while changed {
            changed = false;
            // for every A -> alpha dot follow_dot remain, a, where follow_dot is nonterminal
            // the symbol after dot is nonterminal
            let mut update = HashSet::new();
            for item in closure.iter().filter(|item| {
                item.follow_dot().is_some() && item.follow_dot().unwrap().is_nonterminal()
            }) {
                let follow_dot = item.follow_dot().unwrap();
                let mut remain = item.production.rhs[item.dot + 1..].to_vec();
                remain.push(item.lookahead);
                for production in self
                    .grammar
                    .productions
                    .iter()
                    .filter(|p| p.lhs == follow_dot)
                {
                    for lookahead in self.grammar.first_of(&remain) {
                        let new_item = Item {
                            production: production.clone(),
                            dot: 0,
                            lookahead,
                        };
                        if !closure.contains(&new_item) {
                            update.insert(new_item);
                            changed = true;
                        }
                    }
                }
            }
            closure.extend(update);
        }
        closure
    }

    fn goto(&self, items: &HashSet<Item>, symbol: &Symbol) -> HashSet<Item> {
        let mut moved = HashSet::new();
        for item in items {
            match &item.follow_dot() {
                Some(inner) if inner == symbol => {
                    moved.insert(Item {
                        production: item.production.clone(),
                        dot: item.dot + 1,
                        lookahead: item.lookahead,
                    });
                }
                Some(_) => {}
                None => {}
            }
        }
        self.closure(&moved)
    }

    fn symbol_enum(&self) -> TokenStream {
        let symbols = &self
            .grammar
            .symbols
            .iter()
            .map(|symbol| syn::Ident::new(symbol, proc_macro2::Span::call_site()))
            .collect::<Vec<_>>();
        let terminals = self
            .grammar
            .terminals
            .iter()
            .map(|symbol| {
                syn::Ident::new(
                    &self.grammar.symbols[symbol.index()],
                    proc_macro2::Span::call_site(),
                )
            })
            .collect::<Vec<_>>();
        let start = syn::Ident::new(AUGMENTED_START, proc_macro2::Span::call_site());
        let eof = syn::Ident::new(EOF, proc_macro2::Span::call_site());
        let enum_def = quote::quote! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
            pub enum Symbol {
                #(#symbols,)*
            }
        };
        let helpers = quote::quote! {
            fn is_term(&self) -> bool {
                match self {
                    #(Symbol::#terminals => true,)*
                    _ => false,
                }
            }
            fn is_none_term(&self) -> bool {
                !self.is_term()
            }
            fn is_start(&self) -> bool {
                match self {
                    Symbol::#start => true,
                    _ => false,
                }
            }
            fn is_eof(&self) -> bool {
                match self {
                    Symbol::#eof => true,
                    _ => false,
                }
            }
        };
        quote::quote! {
            #enum_def
            impl Symbol {
                #helpers
            }
        }
    }

    fn production_token_stream(&self, production: &Production) -> TokenStream {
        let lhs = syn::Ident::new(
            &self.grammar.symbols[production.lhs.index()],
            proc_macro2::Span::call_site(),
        );
        let rhs = production
            .rhs
            .iter()
            .map(|symbol| {
                let symbol = syn::Ident::new(
                    &self.grammar.symbols[symbol.index()],
                    proc_macro2::Span::call_site(),
                );
                quote::quote! {
                    Symbol::#symbol
                }
            })
            .collect::<Vec<_>>();
        quote::quote! {
            Production {
                lhs: Symbol::#lhs,
                rhs: vec![#(#rhs,)*],
            }
        }
    }

    fn parser_struct(&self) -> TokenStream {
        let start = syn::Ident::new(AUGMENTED_START, proc_macro2::Span::call_site());
        let actions = self.action.iter().map(|((state, symbol), action)| {
            let state = syn::LitInt::new(&state.to_string(), proc_macro2::Span::call_site());
            let symbol = syn::Ident::new(
                &self.grammar.symbols[symbol.index()],
                proc_macro2::Span::call_site(),
            );
            match action {
                Action::Shift(next_state) => {
                    let next_state =
                        syn::LitInt::new(&next_state.to_string(), proc_macro2::Span::call_site());
                    quote::quote! {
                        ((#state, Symbol::#symbol), Action::Shift(#next_state)),
                    }
                }
                Action::Reduce(production) => {
                    let production = self.production_token_stream(production);
                    quote::quote! {
                        ((#state, Symbol::#symbol), Action::Reduce(#production)),
                    }
                }
                Action::Accept => {
                    quote::quote! {
                        ((#state, Symbol::#symbol), Action::Accept),
                    }
                }
            }
        });
        let gotos = self.goto.iter().map(|((state, symbol), next_state)| {
            let state = syn::LitInt::new(&state.to_string(), proc_macro2::Span::call_site());
            let symbol = syn::Ident::new(
                &self.grammar.symbols[symbol.index()],
                proc_macro2::Span::call_site(),
            );
            let next_state =
                syn::LitInt::new(&next_state.to_string(), proc_macro2::Span::call_site());
            quote::quote! {
                ((#state, Symbol::#symbol), #next_state),
            }
        });
        let symbol_of_token = self
            .grammar
            .terminals
            .iter()
            .filter(|symbol| !(symbol == &&EPSILON_SYMBOL || symbol == &&EOF_SYMBOL))
            .map(|symbol| {
                let symbol = syn::Ident::new(
                    &self.grammar.symbols[symbol.index()],
                    proc_macro2::Span::call_site(),
                );
                quote::quote! {
                    Token::#symbol{..} => Symbol::#symbol,
                }
            });
        quote::quote! {
            use std::collections::HashMap;

            #[derive(Debug, Clone)]
            struct Production {
                lhs: Symbol,
                rhs: Vec<Symbol>,
            }

            #[derive(Debug, Clone)]
            enum Action {
                Shift(usize),
                Reduce(Production),
                Accept,
            }

            type State = usize;

            pub struct Parser<'input> {
                lexer: Lexer<'input>,
                stack: Vec<(Symbol, State)>,
                action: HashMap<(State, Symbol), Action>,
                goto: HashMap<(State, Symbol), State>,
                nodes: Vec<Box<Node<'input>>>,
                verbose: bool,
            }

            #[derive(Debug, Clone)]
            pub struct Node<'input> {
                pub symbol: Symbol,
                pub token: Option<Token<'input>>,
                pub children: Vec<Box<Node<'input>>>,
            }

            fn symbol_of_token<'input>(token: &Option<Token<'input>>) -> Symbol {
                if token.is_none() {
                    return Symbol::EOF;
                }
                match token.as_ref().unwrap() {
                    #(#symbol_of_token)*
                    _ => unreachable!(),
                }
            }

            impl<'input> Parser<'input> {
                pub fn new(src: &'input str) -> Self {
                    Self {
                        lexer: Lexer::new(src),
                        stack: vec![(Symbol::#start, 0)],
                        action: HashMap::from_iter(vec![
                            #(#actions)*
                        ]),
                        goto: HashMap::from_iter(vec![
                            #(#gotos)*
                        ]),
                        nodes: Vec::new(),
                        verbose: false,
                    }
                }

                fn reduce(&mut self, production: &Production) {
                    let mut children = Vec::new();
                    for _ in 0..production.rhs.len() {
                        self.stack.pop();
                        let child = self.nodes.pop().unwrap();
                        children.push(child);
                    }
                    children.reverse();
                    let node = Box::new(Node {
                        symbol: production.lhs,
                        token: None,
                        children,
                    });
                    self.nodes.push(node);
                    let state = self.stack.last().unwrap().1;
                    let symbol = production.lhs;
                    let next_state = self.goto.get(&(state, symbol)).unwrap();
                    self.stack.push((symbol, *next_state));
                }

                fn next_word(&mut self) -> Option<Token<'input>> {
                    let mut word = self.lexer.next();
                    // skip comments
                    while let Some(Token::Comment{..}) = word {
                        word = self.lexer.next();
                    }
                    word
                }

                pub fn set_verbose(&mut self, verbose: bool) {
                    self.verbose = verbose;
                }

                pub fn parse(&mut self) -> Box<Node<'input>> {
                    let mut word = self.next_word();
                    if self.verbose {
                        println!("word: {:?}", word);
                    }
                    loop {
                        let state = self.stack.last().unwrap().1;
                        let symbol = symbol_of_token(&word);
                        let action = self.action.get(&(state, symbol)).expect("No action found").clone();
                        if self.verbose {
                            println!("{:?}", action);
                        }
                        match action {
                            Action::Shift(next_state) => {
                                self.stack.push((symbol, next_state));
                                self.nodes.push(Box::new(Node {
                                    symbol,
                                    token: word,
                                    children: Vec::new(),
                                }));
                                word = self.next_word();
                                if self.verbose {
                                    println!("word: {:?}", word);
                                }
                            }
                            Action::Reduce(production) => {
                                self.reduce(&production);
                            }
                            Action::Accept => {
                                break;
                            }
                        }
                    }
                    self.nodes.first().unwrap().clone()
                }
            }
        }
    }

    pub fn generate(&self) -> TokenStream {
        let symbol_enum = self.symbol_enum();
        let parser_struct = self.parser_struct();
        quote::quote! {
            #symbol_enum
            #parser_struct
        }
    }
}
