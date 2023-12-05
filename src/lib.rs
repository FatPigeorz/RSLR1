extern crate quote;
extern crate syn;

mod grammar;
mod lexer;
mod parser;
use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let def = parse_macro_input!(input as grammar::GrammarDef);
    def.check_well_defined();
    let token_matcher = lexer::token_matcher(&def);
    let token_enum = lexer::token_enum(&def);
    let lexer = lexer::lexer(&def);
    let grammar = parser::Grammar::from(&def);
    let parser_gen = parser::ParserGenerator::new(grammar);
    let parser = parser_gen.generate();
    quote::quote! {
        pub mod lr1 {
            #token_enum
            #token_matcher
            #lexer
            #parser
        }
    }
    .into()
}
