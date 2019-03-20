//Parsing reference at https://github.com/dotnet/roslyn/blob/master/src/Compilers/CSharp/Portable/Parser/Lexer.cs

module Program

open System
open System.Linq.Expressions

type TokenSyntaxKind = 
    | BadToken
    | EndOfFileToken
    | WhitespaceToken
    | NumberToken
    | LiteralToken
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
    | BinaryOperator of BinaryOperator
    | UnaryOperator of UnaryOperator

type SyntaxToken = { text: string; kind: SyntaxKind;  }

type Expression = 
    | BinaryExpression of operand1: SyntaxToken * operator: SyntaxToken * operand2: SyntaxToken // + operator
    | UnaryExpression of operand: SyntaxToken * operator: SyntaxToken    // ! Operator, for example

let isNumber (string : string) =
    match Int32.TryParse string with //Handle decimal,...
    | true, _ -> true
    | false, _ -> false

let isNumeric char = 
    char >= '0' && char <= '9'
let isAsciiLetter char = 
    char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z'

let isWhitespace char = 
    char = ' '


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


type ParsingResult<'a> = 
    | Success of value: 'a list * remainder : string
    | Failure of remainder: string
 
let toString (input: char list) = 
    String.concat "" (List.map string input)

type ReadLiteralTokenResult = 
    | ReadLiteralSuccess of tokenString : string * remainder : string
    | ReadLiteralFailure of remainder: string

let readLiteralToken (input : string)= 
    let rec readInternal (inputCh : char list) =
        match inputCh with
        | [] -> ReadLiteralFailure (toString inputCh)
        | head::tail when isIdentifierValidChar head ->
            let readResult = readInternal tail
            match readResult with 
            | ReadLiteralSuccess (value, remainder) -> ReadLiteralSuccess ((String.concat "" [string (head);value] ), remainder)
            | ReadLiteralFailure remainder -> ReadLiteralSuccess ((head.ToString()), remainder)
        | _ -> ReadLiteralFailure (toString inputCh)

    let readResult = readInternal (input.Trim().ToCharArray() |> List.ofArray)
    match readResult with 
        | ReadLiteralSuccess (value, remainder) when value.Length > 0 ->  
            let kind = if isNumber value then NumberToken else LiteralToken
            Success ([{ text = value; kind = TokenSyntaxKind kind; }], remainder)
               
        | ReadLiteralFailure remainder -> Failure remainder
        | _ -> Failure input


let getSyntaxKindString (syntaxKind: SyntaxKind) = 
    match syntaxKind with
    | TokenSyntaxKind PlusToken -> "+"
    | TokenSyntaxKind MinusToken -> "-"
    | TokenSyntaxKind StarToken -> "*"
    | TokenSyntaxKind SlashToken-> "/" 
    | TokenSyntaxKind SemiColonToken-> ";" 
    | TokenSyntaxKind BangToken-> "!" 
    | TokenSyntaxKind AmpersandAmpersandToken-> "&&" 
    | TokenSyntaxKind AmpersandToken-> "&" 
    | TokenSyntaxKind EqualsToken-> "=" 
    | _ -> failwith "Unsupported token kind"



let tokenParser (tokenKind: SyntaxKind) (inputString: string) =

    let rec tokenParserInternal (tokenString: string) (inputString: string) =
        if tokenString = "" then
            true, inputString
        else if inputString = "" then
            false, inputString
        else
            if tokenString.[0] = inputString.[0] then 
                tokenParserInternal tokenString.[1..] inputString.[1..]
            else 
                false, inputString
    
    let tokenString = getSyntaxKindString tokenKind
    let found, remainder = tokenParserInternal tokenString inputString
    match found with        
    | true -> Success ([{ text = tokenString; kind = tokenKind; }], remainder)
    | false -> Failure remainder

let plusParser = tokenParser (TokenSyntaxKind PlusToken)
let minusParser = tokenParser (TokenSyntaxKind MinusToken)
let bangParser = tokenParser (TokenSyntaxKind BangToken)
let semicolonParser = tokenParser (TokenSyntaxKind SemiColonToken)
let equalsParser = tokenParser (TokenSyntaxKind EqualsToken)

let ampersandParser = tokenParser (TokenSyntaxKind AmpersandToken)
let ampersandAmpersandParser = tokenParser (TokenSyntaxKind AmpersandAmpersandToken)


let (>|>) someInput f  = 
    match someInput with
    | Success (s, remainder) -> 
        let result = (f remainder)
        match result with
        | Success (t, rem) -> Success (s@t, rem)
        | Failure (v)-> Failure v
    | Failure (v)-> Failure v

let orParser p1 p2 input =
    match p1 input with
    | Success (l, rem) -> Success (l, rem)
    | Failure _ ->
        match p2 input with
        | Success (l, rem) -> Success (l, rem)
        | Failure f -> Failure f

let (<|>) = orParser

let binaryOperatorParser (input : string) =
    input.Trim() |> (plusParser <|> minusParser <|> equalsParser <|> ampersandAmpersandParser <|> ampersandParser)
let unaryOperatorParser (input : string)  =
    input.Trim() |> (plusParser <|> bangParser)


let rec parseBinaryExpression input = 
    let res = input |> readLiteralToken >|> binaryOperatorParser >|> readLiteralToken
    match res with 
    | Success (l, rem) -> Success ([BinaryExpression (l.[0],l.[1],l.[2])], rem)
    | Failure rem -> Failure rem
and parsePrefixUnaryExpression input = 
    let res = input |> unaryOperatorParser >|> readLiteralToken
    match res with 
    | Success (l, rem) -> Success ([UnaryExpression (l.[1],l.[0])], rem)
    | Failure rem -> Failure rem
and parsePostfixUnaryExpression input = 
    let res = input |> readLiteralToken >|> unaryOperatorParser 
    match res with 
    | Success (l, rem) -> Success ([UnaryExpression (l.[0],l.[1])], rem)
    | Failure rem -> Failure rem
and parseExpressions input = 
    let stg = input |> (parseBinaryExpression <|> parsePrefixUnaryExpression <|> parsePostfixUnaryExpression)
    match stg with 
    | Success (l, rem) -> l.[0]::(parseExpressions (rem.Trim()))
    | Failure rem -> []

                
[<EntryPoint>]
let main argv =

    let input = "1+babar 1-123 +45 42! toto = 42"
    let expressions = parseExpressions input
    printfn "%A" expressions

    0

