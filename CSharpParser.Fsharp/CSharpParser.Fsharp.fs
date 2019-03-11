//Parsing reference at https://github.com/dotnet/roslyn/blob/master/src/Compilers/CSharp/Portable/Parser/Lexer.cs

module Program

open System

type TokenSyntaxKind = 
    | BadToken
    | EndOfFileToken
    | WhitespaceToken
    | NumberToken
    | PlusToken
    | SlashToken
    | StarToken
    | MinusToken
    | OpenParenthesisToken
    | CloseParenthesisToken
    | OpenBraceToken
    | CloseBraceToken
    | IdentifierToken
    | BangToken
    | AmpersandToken
    | AmpersandAmpersandToken
    | PipeToken
    | PipePipeToken
    | TildeToken
    | HatToken
    | SemiColonToken
    | EqualsToken
    | EqualsEqualsToken
    | ArrowToken
    | BangEqualsToken
    | LessOrEqualsToken
    | LessToken
    | GreaterOrEqualsToken
    | GreaterToken

type KeywordSyntaxKind = 
    | FalseKeyword
    | TrueKeyword
    | VarKeyword
    | IfKeyword
    | ElseKeyword
    | WhileKeyword
    | ForKeyword

type BinaryOperator =
    | Plus
    | Minus
type UnaryOperator =
    | Bang

type SyntaxKind = 
    | TokenSyntaxKind of TokenSyntaxKind
    | KeywordSyntaxKind of KeywordSyntaxKind
    | Integer
    | BinaryOperator of BinaryOperator
    | UnaryOperator of UnaryOperator
    //Others things

type SyntaxToken = { position : int; text: string; kind: SyntaxKind;  }

type Expression = 
    | BinaryExpression of operand1: SyntaxToken * operator: SyntaxToken * operand2: SyntaxToken // + operator
    | UnaryExpression of operand: SyntaxToken * operator: SyntaxToken    // ! Operator, for example

let isNumber (string : string) =
    match Int32.TryParse string with
    | true, _ -> true
    | false, _ -> false

let isNumeric char = 
    char >= '0' && char <= '9'
let isAsciiLetter char = 
    char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z'

let isWhitespace char = 
    char = ' '

let rec readNumericLiteralToken input =
    match input with 
        | [] -> ""
        | [head] -> head.ToString()
        | head::tail ->
            match head with 
            | x when isNumeric x || x = '.'  -> 
                let text = readNumericLiteralToken tail
                x.ToString() + text
            | x when isWhitespace x ->  ""
            //| x when x = 'f' -> Handle float, decimal...
            | _ -> ""

let isIdentifierValidStartChar x = isAsciiLetter x
let isIdentifierValidChar x = isIdentifierValidStartChar x || isNumeric x 

let getKeyWord literal = 
    match literal with
        | "false" -> Some FalseKeyword
        | "true" -> Some TrueKeyword
        | "var" -> Some VarKeyword
        | "if" -> Some IfKeyword
        | "else" -> Some ElseKeyword
        | "while" -> Some WhileKeyword
        | "for" -> Some ForKeyword
        | _ -> None

let rec readKeywordOrIdentifier input = 
    match input with 
        | [] -> ""
        | [head] -> head.ToString()
        | head::tail ->
            match head with 
            | x when isWhitespace x ->  ""
            | x when isIdentifierValidChar x -> 
                let text = readKeywordOrIdentifier tail
                x.ToString() + text
            | _ -> "" // Is it right ?

let rec lex input position = 
    match input with
        | [] -> []
        | '\000'::_ ->  [{ position = position; text = ""; kind = TokenSyntaxKind EndOfFileToken }]
        | '+'::tail -> { position = position; text = "+"; kind = TokenSyntaxKind PlusToken }:: (lex tail (position + 1))
        | '-'::tail -> { position = position; text = "-"; kind = TokenSyntaxKind MinusToken }:: (lex tail (position + 1))
        | '*'::tail -> { position = position; text = "*"; kind = TokenSyntaxKind StarToken }:: (lex tail (position + 1))
        | '/'::tail -> { position = position; text = "/"; kind = TokenSyntaxKind SlashToken }:: (lex tail (position + 1))
        | '('::tail -> { position = position; text = "("; kind = TokenSyntaxKind OpenParenthesisToken }:: (lex tail (position + 1))
        | ')'::tail -> { position = position; text = ")"; kind = TokenSyntaxKind CloseParenthesisToken }:: (lex tail (position + 1))
        | '{'::tail -> { position = position; text = "{"; kind = TokenSyntaxKind OpenBraceToken }:: (lex tail (position + 1))
        | '}'::tail -> { position = position; text = "}"; kind = TokenSyntaxKind CloseBraceToken }:: (lex tail (position + 1))
        | '~'::tail -> { position = position; text = "~"; kind = TokenSyntaxKind TildeToken }:: (lex tail (position + 1))
        | '^'::tail -> { position = position; text = "^"; kind = TokenSyntaxKind HatToken }:: (lex tail (position + 1))
        | ';'::tail -> { position = position; text = ";"; kind = TokenSyntaxKind SemiColonToken }:: (lex tail (position + 1))
        | '&'::tail -> 
            match tail.[0] with
                | '&' -> { position = position; text = "&&"; kind = TokenSyntaxKind AmpersandAmpersandToken }:: (lex tail (position + 2))
                | _ -> { position = position; text = "&"; kind = TokenSyntaxKind AmpersandToken }:: (lex tail (position + 1))
        | '|'::tail -> 
            match tail.[0] with
                | '|' -> { position = position; text = "||"; kind = TokenSyntaxKind PipePipeToken }:: (lex tail (position + 2))
                | _ -> { position = position; text = "|"; kind = TokenSyntaxKind PipeToken }:: (lex tail (position + 1))
        | '='::tail -> 
            match tail.[0] with
                | '=' -> { position = position; text = "=="; kind = TokenSyntaxKind EqualsEqualsToken }:: (lex tail (position + 2))
                | '>' -> { position = position; text = "=>"; kind = TokenSyntaxKind ArrowToken }:: (lex tail (position + 2))
                | _ -> { position = position; text = "="; kind = TokenSyntaxKind EqualsToken }:: (lex tail (position + 1))
        | '!'::tail -> 
            match tail.[0] with
                | '=' -> { position = position; text = "!="; kind = TokenSyntaxKind BangEqualsToken }:: (lex tail (position + 2))
                | _ -> { position = position; text = "!"; kind = TokenSyntaxKind BangToken }:: (lex tail (position + 1))
        | '<'::tail -> 
            match tail.[0] with
                | '=' -> { position = position; text = "<="; kind = TokenSyntaxKind LessOrEqualsToken }:: (lex tail (position + 2))
                | _ -> { position = position; text = "<"; kind = TokenSyntaxKind LessToken }:: (lex tail (position + 1))
        | '>'::tail -> 
            match tail.[0] with
                | '=' -> { position = position; text = ">="; kind = TokenSyntaxKind GreaterOrEqualsToken }:: (lex tail (position + 2))
                | _ -> { position = position; text = ">"; kind = TokenSyntaxKind GreaterToken }:: (lex tail (position + 1))
        | x::tail when isNumeric x -> 
            let literal = readNumericLiteralToken input
            { position = position; text = literal; kind = TokenSyntaxKind NumberToken }:: (lex tail.[literal.Length - 1..] (position + literal.Length))
        | x::tail when isWhitespace x ->
            lex tail (position + 1)
        | x::tail when isIdentifierValidStartChar x -> 
            let literal = readKeywordOrIdentifier input
            match literal |> getKeyWord with
                | Some x-> { position = position; text = literal; kind = KeywordSyntaxKind x }:: (lex tail.[literal.Length - 1..] (position + literal.Length))
                | None -> { position = position; text = literal; kind = TokenSyntaxKind IdentifierToken }:: (lex tail.[literal.Length - 1..] (position + literal.Length))
        | x::tail -> { position = position; text = x.ToString(); kind = TokenSyntaxKind BadToken }:: (lex tail (position + 1))
        

let parseTokens input = lex input 0

let rec parseExpressions input = 
    match input |> List.map (fun i -> i.kind) with
    | [] -> []
    | Integer::BinaryOperator _::Integer::_ -> BinaryExpression (input.[0],input.[1],input.[2]) :: (parseExpressions input.[3..])
    | UnaryOperator _::Integer::_ -> UnaryExpression (operand= input.[1], operator=input.[0]) :: (parseExpressions input.[2..])
    | _ -> []
                
[<EntryPoint>]
let main argv =
    let input = "123 + 132 2 + 2 ! 3+4 for myIdentifier"
    let tokens = parseTokens (input.ToCharArray() |> List.ofArray)
      
    printfn "%A" tokens

    let expressions = parseExpressions tokens
    printfn "%A" expressions

    0