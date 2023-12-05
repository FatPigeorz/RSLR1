# RuSt LR1 parser generator
rust proc_macro based LR1 parser generator

a simple C-- parser example:
```rust
use RSLR1::parser;
parser! {
    lex {
        IntegerLiteral : r"[0-9]+";
        FloatLiteral : r"[0-9]+\.([0-9]+)?";
        Identifier: r"[a-zA-Z_][a-zA-Z0-9_]*";
        StringLiteral: r#""([^"\\]|\\.)*""#;
        CharLiteral: r"'([^'\\]|\\.)*'";
        Comment: r"//[^\n]*|/\*([^*]|\*[^/])*\*/";
        Void: r"void";
        Bool: r"bool";
        Char: r"char";
        Int: r"int";
        Float: r"float";
        True: r"true";
        False: r"false";
    }

    syntax {
        start => ExternalDeclaration;

        ExternalDeclaration => {
            Declaration;
            FunctionDefinition;
        }
        TypeSpecifier => {
            Void;
            Bool;
            Char;
            Int;
            Float;
        }
        Declaration => {
            TypeSpecifier DeclarationParameter DeclarationParameterSuffix;
        }
        DeclarationParameter => {
            Identifier DeclarationParameterAssign;
        }
        DeclarationParameterAssign => {
            Expression;
            EPSILON;
        }
        DeclarationParameterSuffix => {
            "," DeclarationParameter DeclarationParameterSuffix;
            EPSILON;
        }
        PrimaryExpression => {
            Identifier;
            Literal;
            "(" Expression ")";
        }
        Literal => {
            IntegerLiteral;
            FloatLiteral;
            BoolLiteral;
            CharLiteral;
        }
        BoolLiteral => {
            True;
            False;
        }
        Operator => {
            "+";
            "-";
            "*";
            "/";
            "%";
            "<";
            "<=";
            ">";
            ">=";
            "==";
        }
        AssignmentOperator => {
            "=";
            "+=";
            "-=";
            "*=";
            "/=";
            "%=";
        }
        AssignmentExpression => {
            Identifier AssignmentOperator Expression;
        }
        AssignmentExpressionListSuffix => {
            "," AssignmentExpression AssignmentExpressionListSuffix;
            EPSILON;
        }
        AssignmentExpressionList => {
            AssignmentExpression AssignmentExpressionListSuffix;
            EPSILON;
        }
        Expression => {
            ConstantExpression;
            FunctionExpression;
        }
        ConstantExpression => {
            PrimaryExpression;
            AssignmentExpression;
        }
        ArithmeticExpression => {
            Operator;
            PrimaryExpression ArithmeticExpression;
            EPSILON;
        }
        FunctionExpression => {
            Identifier "(" ExpressionList ")";
        }
        ExpressionListSuffix => {
            "," Expression ExpressionListSuffix;
            EPSILON;
        }
        ExpressionList => {
            Expression ExpressionListSuffix;
            EPSILON;
        }
        FunctionDefinition => {
            TypeSpecifier Identifier "(" FunctionParameterList ")" CompoundStatement;
            EPSILON;
        }
        FunctionParameterList => {
            FunctionParameter FunctionParameterListSuffix;
            EPSILON;
        }
        FunctionParameter => {
            TypeSpecifier Identifier;
        }
        FunctionParameterListSuffix => {
            "," FunctionParameter FunctionParameterListSuffix;
            EPSILON;
        }
        CompoundStatement => {
            "{" StatementList "}";
        }
        StatementList => {
            Statement StatementList;
            EPSILON;
        }
        Statement => {
            ExpressionStatement;
            JumpStatement;
            SelectionStatement;
            IterationStatement;
            CompoundStatement;
            Declaration;
        }
        ExpressionStatement => {
            AssignmentExpressionList ";";
        }
        JumpStatement => {
            "continue" ";";
            "break" ";";
            "return" Expression ";";
        }
        SelectionStatement => {
            "if" "(" Expression ")" Statement "else" Statement;
        }
        IterationStatement => {
            "while" "(" Expression ")" Statement;
            "for" "(" Declaration Expression ";" AssignmentExpression ")" Statement;
        }
    }
}

#[test]
fn test_simple_parse() {
    let src = r#"
        int main() {
            int a;
            int b;
            a = 1;
            b = 1;
            return 0;
        }
    "#;
    let mut parser = Parser::new(src);
    let _parse_tree = parser.parse();
}
```
