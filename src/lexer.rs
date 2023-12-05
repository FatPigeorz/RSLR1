use super::grammar::GrammarDef;
use proc_macro2::TokenStream;

pub(crate) fn token_matcher(grammar: &GrammarDef) -> TokenStream {
    let matcher_fns = grammar.lex_rules.iter().map(|rule| {
        let name = syn::Ident::new(&rule.name, proc_macro2::Span::call_site());
        let regex = syn::LitStr::new(&rule.regex, proc_macro2::Span::call_site());
        let const_re_name = syn::Ident::new(&format!("RE_{}",rule.name.to_uppercase().replace("-", "_")), proc_macro2::Span::call_site());
        let matcher_name = syn::Ident::new(&format!("match_{}",rule.name.to_lowercase().replace("-", "_")), proc_macro2::Span::call_site());
        quote::quote! {
            static #const_re_name: once_cell::sync::Lazy<regex::Regex> = once_cell::sync::Lazy::new(|| regex::Regex::new(#regex).unwrap());
            fn #matcher_name<'input>(src: &'input str, line: usize) -> Option<Token<'input>> {
                let caps = #const_re_name.captures(src)?;
                // Doc of rust regex: When i == 0, this is guaranteed to return a non-None value.
                // https://docs.rs/regex/latest/regex/struct.Captures.html
                if caps.get(0).unwrap().start() != 0 {
                    return None;
                }
                let lexme = caps.get(0).unwrap().as_str();
                Some(Token::#name{
                    lexme,
                    line,
                })
            }
        }
    }).collect::<Vec<_>>();

    let matcher_name = grammar
        .lex_rules
        .iter()
        .map(|rule| {
            syn::Ident::new(
                &format!("match_{}", rule.name.to_lowercase().replace("-", "_")),
                proc_macro2::Span::call_site(),
            )
        })
        .collect::<Vec<_>>();

    quote::quote! {
        #(#matcher_fns)*
        const MATCHERS: &[fn(&str, usize) -> Option<Token>] = &[
            #(#matcher_name),*
        ];
    }
}

pub(crate) fn token_enum(grammar: &GrammarDef) -> TokenStream {
    let names = grammar
        .lex_rules
        .iter()
        .map(|rule| syn::Ident::new(&rule.name, proc_macro2::Span::call_site()))
        .collect::<Vec<_>>();

    quote::quote! {
        #[allow(unused_macros)]
        macro_rules! token_lexme {
            ($token: expr) => {
                match $token {
                    #(Token::#names{lexme, ..} => lexme,)*
                }
            };
        }

        #[allow(unused_macros)]
        macro_rules! token_line {
            ($token: expr) => {
                match $token {
                    #(Token::#names{line, ..} => line,)*
                    _ => unreachable!(),
                }
            };
        }

        #[non_exhaustive]
        #[derive(Debug, PartialEq, Eq, Clone)]
        pub enum Token<'input> {
            #(#names{lexme: &'input str, line: usize},)*
        }
    }
}

pub(crate) fn lexer(_grammar: &GrammarDef) -> TokenStream {
    quote::quote! {
        pub struct Lexer<'input> {
            src: &'input str,
            curr: usize,
            line: usize,
        }

        impl<'input> Lexer<'input> {
            pub fn new(src: &'input str) -> Self {
                let mut lexer = Lexer {
                    src,
                    curr: 0,
                    line: 1,
                };
                // skip whitespace at the beginning
                lexer.skip_whitespace();
                lexer
            }

            fn skip_whitespace(&mut self) {
                while self.curr < self.src.len() {
                    // since we checked that the string is not empty, unwrap is safe
                    match self.src[self.curr..].chars().next().unwrap() {
                        ' ' | '\t' | '\n' => {
                            if self.src[self.curr..].starts_with("\n") {
                                self.line += 1;
                            }
                            self.curr += 1;
                        }
                        _ => break,
                    }
                }
            }
        }

        impl<'input> std::iter::Iterator for Lexer<'input> {
            type Item = Token<'input>;

            fn next(&mut self) -> Option<Self::Item> {
                use rayon::prelude::*;
                // only keep the those match start at self.curr
                if self.curr >= self.src.len() {
                    return None;
                }
                let tokens = MATCHERS.par_iter().map(|matcher| matcher(&self.src[self.curr..], self.line)).filter_map(|token| token).collect::<Vec<_>>();
                if tokens.is_empty() {
                    panic!("unexpected error at line {}:\n{}", self.line, self.src.split("\n").nth(self.line - 1).unwrap());
                }
                let longest_token = tokens.iter().max_by_key(|token| token_lexme!(token).len()).unwrap().clone();
                self.curr += token_lexme!(&longest_token).len();
                self.line += token_lexme!(&longest_token).matches("\n").count();
                self.skip_whitespace();
                Some(longest_token)
            }
        }

    }
    .into()
}
