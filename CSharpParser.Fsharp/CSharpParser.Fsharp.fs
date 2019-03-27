module Program

open System

////////////////////////// LEXER
/// Transforms an input text into a SyntaxToken List

type TokenSyntaxKind = 
    | BadToken
    | EndOfFileToken
    | WhitespaceToken
    //Delimiters
    | OpenParenthesisToken
    | CloseParenthesisToken
    | OpenBraceToken
    | CloseBraceToken
    | SemiColonToken
    //Literals
    | NumberToken
    | LiteralToken
    | IdentifierToken
    //Math Token
    | PlusToken
    | PlusPlusToken
    | PlusEqualsToken
    | SlashToken
    | SlashEqualsToken
    | StarToken
    | StarEqualsToken
    | MinusToken
    | MinusMinusToken
    | MinusEqualsToken
    //ComparisonOperators
    | EqualsEqualsToken
    | BangEqualsToken
    | LessOrEqualsToken
    | LessToken
    | GreaterOrEqualsToken
    | GreaterToken

    | BangToken
    | AmpersandToken
    | AmpersandAmpersandToken
    | PipeToken
    | PipePipeToken
    | TildeToken
    | HatToken
    | EqualsToken
    | MinusGreaterToken

type KeywordSyntaxKind = 
    | FalseKeyword
    | TrueKeyword
    | VarKeyword
    | IfKeyword
    | ElseKeyword
    | WhileKeyword
    | ForKeyword

type StatementSyntaxKind =
    | BlockStatement
    | VariableDeclaration
    | IfStatement
    | WhileStatement
    | ForStatement
    | ExpressionStatement

type ExpressionSyntaxKind =
    | LiteralExpression
    | NameExpression
    | UnaryExpression
    | BinaryExpression
    | ParenthesizedExpression
    | AssignmentExpression

type SyntaxKind = 
    | TokenSyntaxKind of TokenSyntaxKind
    | KeywordSyntaxKind of KeywordSyntaxKind
    | StatementSyntaxKind of StatementSyntaxKind
    | ExpressionSyntaxKind of ExpressionSyntaxKind
    | ElseClauseSyntaxKind

type SyntaxToken = { text: string; kind: SyntaxKind;  }

let private getSyntaxKindString (syntaxKind: SyntaxKind) = 
    match syntaxKind with
    | TokenSyntaxKind WhitespaceToken -> " "
    | TokenSyntaxKind PlusToken -> "+"
    | TokenSyntaxKind PlusPlusToken -> "++"
    | TokenSyntaxKind PlusEqualsToken -> "+="
    | TokenSyntaxKind MinusToken -> "-"
    | TokenSyntaxKind MinusMinusToken -> "--"
    | TokenSyntaxKind MinusEqualsToken -> "-="
    | TokenSyntaxKind MinusGreaterToken -> "->"
    | TokenSyntaxKind StarToken -> "*"
    | TokenSyntaxKind StarEqualsToken -> "*="
    | TokenSyntaxKind SlashToken-> "/" 
    | TokenSyntaxKind SlashEqualsToken-> "/=" 
    | TokenSyntaxKind SemiColonToken-> ";" 
    | TokenSyntaxKind BangToken-> "!" 
    | TokenSyntaxKind BangEqualsToken-> "!=" 
    | TokenSyntaxKind AmpersandAmpersandToken-> "&&" 
    | TokenSyntaxKind AmpersandToken-> "&" 
    | TokenSyntaxKind PipePipeToken-> "||" 
    | TokenSyntaxKind PipeToken-> "|" 
    | TokenSyntaxKind EqualsToken-> "=" 
    | TokenSyntaxKind EqualsEqualsToken-> "==" 
    | TokenSyntaxKind OpenParenthesisToken-> "(" 
    | TokenSyntaxKind CloseParenthesisToken-> ")" 
    | TokenSyntaxKind OpenBraceToken-> "{" 
    | TokenSyntaxKind CloseBraceToken-> "}" 
    | TokenSyntaxKind LessToken-> "<" 
    | TokenSyntaxKind LessOrEqualsToken -> "<=" 
    | TokenSyntaxKind GreaterToken-> ">" 
    | TokenSyntaxKind GreaterOrEqualsToken-> ">=" 
    | KeywordSyntaxKind FalseKeyword-> "false" 
    | KeywordSyntaxKind TrueKeyword-> "true" 
    | KeywordSyntaxKind VarKeyword-> "var" 
    | KeywordSyntaxKind IfKeyword-> "if" 
    | KeywordSyntaxKind ElseKeyword-> "else" 
    | KeywordSyntaxKind ForKeyword-> "for" 
    | KeywordSyntaxKind WhileKeyword-> "while" 
    | _ -> 
        printfn  "Unsupported token kind : %A"  syntaxKind 
        failwith  "Unsupported token kind"

let private toString (input: char list) = 
    String.concat "" (List.map string input)

let private isNumeric char = 
    char >= '0' && char <= '9'
let private  isAsciiLetter char = 
    char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z'

let private  isNumber (string : string) =
    match Int32.TryParse string with //Handle decimal,...
    | true, _ -> true
    | false, _ -> false
    
let private  isIdentifierValidStartChar x = isAsciiLetter x
let private  isIdentifierValidChar x = isIdentifierValidStartChar x || isNumeric x 

let private orPipe p1 p2 input =
    match p1 input with
    | Ok s -> Ok s
    | Error  _ ->
        match p2 input with
        | Ok s2 -> Ok s2
        | Error _ -> Error ()

let private (<|>) = orPipe

let private tokenParser (tokenKind: SyntaxKind) (inputString: string) =
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
    | true -> Ok ({ text = tokenString; kind = tokenKind; }, remainder)
    | false -> Error () 

let private semicolonParser = tokenParser (TokenSyntaxKind SemiColonToken)
let private braceParser = tokenParser (TokenSyntaxKind OpenBraceToken) <|> tokenParser (TokenSyntaxKind CloseBraceToken)  
let private parenthesisParser = tokenParser (TokenSyntaxKind OpenParenthesisToken) <|> tokenParser (TokenSyntaxKind CloseParenthesisToken)

let private plusParser = tokenParser (TokenSyntaxKind PlusToken)
let private plusPlusParser = tokenParser (TokenSyntaxKind PlusPlusToken)
let private plusEqualsParser = tokenParser (TokenSyntaxKind PlusEqualsToken)
let private minusParser = tokenParser (TokenSyntaxKind MinusToken)
let private minusMinusParser = tokenParser (TokenSyntaxKind MinusMinusToken)
let private minusEqualsParser = tokenParser (TokenSyntaxKind MinusEqualsToken)
let private minusGreaterParser = tokenParser (TokenSyntaxKind MinusGreaterToken)
let private starParser = tokenParser (TokenSyntaxKind StarToken)
let private starEqualsParser = tokenParser (TokenSyntaxKind StarEqualsToken)
let private slashParser = tokenParser (TokenSyntaxKind SlashToken)
let private slashEqualsParser = tokenParser (TokenSyntaxKind SlashEqualsToken)
let private equalsTokenParser = tokenParser (TokenSyntaxKind EqualsToken)
let private equalsEqualsTokenParser = tokenParser (TokenSyntaxKind EqualsEqualsToken)
let private bangTokenParser = tokenParser (TokenSyntaxKind BangToken)
let private bangEqualsTokenParser = tokenParser (TokenSyntaxKind BangEqualsToken)
let private lessOrEqualsTokenParser = tokenParser (TokenSyntaxKind LessOrEqualsToken)
let private lessTokenParser = tokenParser (TokenSyntaxKind LessToken)
let private greaterOrEqualsTokenParser = tokenParser (TokenSyntaxKind GreaterOrEqualsToken)
let private greaterTokenParser = tokenParser (TokenSyntaxKind GreaterToken)

let private falseKeywordParser = tokenParser (KeywordSyntaxKind FalseKeyword)
let private trueKeywordParser = tokenParser (KeywordSyntaxKind TrueKeyword)
let private varKeywordParser = tokenParser (KeywordSyntaxKind VarKeyword)
let private ifKeywordParser = tokenParser (KeywordSyntaxKind IfKeyword)
let private elseKeywordParser = tokenParser (KeywordSyntaxKind ElseKeyword)
let private whileKeywordParser = tokenParser (KeywordSyntaxKind WhileKeyword)
let private forKeywordParser = tokenParser (KeywordSyntaxKind ForKeyword)

let private literalTokenParser (input : string) = 
    let rec readInternal (inputCh : char list) =
        match inputCh with
        | [] -> Error (toString inputCh)
        | head::tail when isIdentifierValidChar head ->
            let readResult = readInternal tail
            match readResult with 
            | Ok (value, remainder) -> Ok ((String.concat "" [string (head);value] ), remainder)
            | Error remainder -> Ok ((head.ToString()), remainder)
        | _ -> Error (toString inputCh)

    let readResult = readInternal (input.ToCharArray() |> List.ofArray)
    match readResult with 
        | Ok (value, remainder) when value.Length > 0 ->  
            let kind = 
                    if isNumber value then NumberToken 
                    else IdentifierToken
                    // TODO : Handle LiteralToken ( = "blabla" )
            Ok ({ text = value; kind = TokenSyntaxKind kind; }, remainder)
               
        | Error _ -> Error ()
        | _ -> Error ()

let whitespaceParser input = 
    input |> tokenParser (TokenSyntaxKind WhitespaceToken)

let delimiterParser input = 
    input |>
    (semicolonParser <|> parenthesisParser <|> braceParser)

let operatorParser input = 
    input |>
    (semicolonParser <|> braceParser <|> parenthesisParser <|>
     plusPlusParser <|> plusEqualsParser <|> plusParser <|> 
     minusMinusParser <|> minusEqualsParser <|> minusGreaterParser <|> minusParser <|> 
     starEqualsParser <|> starParser <|> 
     slashEqualsParser <|> slashParser <|> 
     equalsEqualsTokenParser <|> equalsTokenParser <|> 
     bangEqualsTokenParser <|> bangTokenParser <|>
     lessOrEqualsTokenParser <|> lessTokenParser  <|> 
     greaterOrEqualsTokenParser <|> greaterTokenParser)

let keywordParser input = 
    input |>
    (falseKeywordParser <|> trueKeywordParser <|> 
     varKeywordParser <|> 
     ifKeywordParser <|> elseKeywordParser <|> 
     forKeywordParser <|> whileKeywordParser)

let rec tokenizer input = 
    let res = input |> (whitespaceParser <|> delimiterParser <|> operatorParser <|> keywordParser <|> literalTokenParser)
    match res with 
    | Ok (token, rem) -> token::(tokenizer rem)
    | Error _ -> []

////////////////////////// PARSER
/// Transforms an input SyntaxToken List into a StatementSyntax List

type ExpressionSyntax =
    | AssignmentExpressionSyntax of identifier : SyntaxToken * equalsOperator : SyntaxToken * expression : ExpressionSyntax
    | BinaryExpressionSyntax of left: ExpressionSyntax * operator : SyntaxToken * right: ExpressionSyntax
    | LiteralExpressionSyntax of literal: SyntaxToken
    | IdentifierExpressionSyntax of identifier: SyntaxToken
    | ParenthesizedExpressionSyntax of OpenParenthesis: SyntaxToken * expression: ExpressionSyntax * closeParenthesis : SyntaxToken
    | UnaryExpressionSyntax of operator: SyntaxToken * operand: ExpressionSyntax

type StatementSyntax = 
    | BlockStatementSyntax of openBrace : SyntaxToken option * innerStatements: StatementSyntax list * closeBrace: SyntaxToken
    | ClassDefinitionSyntax // TODO
    | ExpressionStatementSyntax of expression : ExpressionSyntax
    | ForStatementSyntax // TODO
    | FunctionDefinitionSyntax // TODO    
    | ElseClauseSyntax of elseKeyword: SyntaxToken * statement: StatementSyntax
    | IfStatementSyntax of ifKeyword: SyntaxToken * condition : ExpressionSyntax * statement: StatementSyntax // * elseClause : ElseClauseSyntax
    | InferredVariableDeclarationSyntax of keyword : SyntaxToken * identifier: SyntaxToken * equalsOperator: SyntaxToken * initializer: ExpressionSyntax
    | TypedVariableDeclarationSyntax of typeIdentifier : SyntaxToken * identifier: SyntaxToken * equalsOperator: SyntaxToken * initializer: ExpressionSyntax
    | WhileStatementSyntax // TODO

type Syntax = 
    | ExpressionSyntax of ExpressionSyntax
    | StatementSyntax of StatementSyntax

let rec private syntaxTokenMatcher tokenKind (inputTokens: SyntaxToken list) =
    match inputTokens with
    | [] -> Error ()
    | head::tail when head.kind = TokenSyntaxKind WhitespaceToken -> (syntaxTokenMatcher tokenKind tail)
    | head::tail -> 
        match head.kind = tokenKind with
        | true -> Ok ([head], tail)
        | false -> Error () 

let blockStatementStartMatcher  = syntaxTokenMatcher (TokenSyntaxKind OpenBraceToken)
let blockStatemenEndMatcher  = syntaxTokenMatcher (TokenSyntaxKind CloseBraceToken)
let varKeywordMatcher  = syntaxTokenMatcher (KeywordSyntaxKind VarKeyword)
let identifierMatcher = syntaxTokenMatcher (TokenSyntaxKind IdentifierToken)
let equalsOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind EqualsToken)
let literalMatcher = syntaxTokenMatcher (TokenSyntaxKind NumberToken)
let semicolonMatcher = syntaxTokenMatcher (TokenSyntaxKind SemiColonToken)

let plusOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind PlusToken)
let minusOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind MinusToken)
let starOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind StarToken)
let slashOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind SlashToken)
let ampersandOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind AmpersandToken)
let ampersandAmpersandOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind AmpersandAmpersandToken)
let pipeOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind PipeToken)
let pipePipeOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind PipePipeToken)
let equalsEqualsOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind EqualsEqualsToken)
let bangEqualsOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind BangEqualsToken)
let lessOrEqualsOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind LessOrEqualsToken)
let lessOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind LessToken)
let greaterOrEqualsOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind GreaterOrEqualsToken)
let greaterOperatorMatcher = syntaxTokenMatcher (TokenSyntaxKind GreaterToken)



let (>|>) input f  = 
    match input with
    | Ok (s, remainder) -> 
        let result = (f remainder)
        match result with
        | Ok (t, rem) -> Ok (s@t, rem)
        | Error _ -> Error ()
    | Error _ -> Error ()


let matchVarDeclarationStatement (input: SyntaxToken list) = 
    let res = input |> varKeywordMatcher >|> identifierMatcher >|> equalsOperatorMatcher >|> literalMatcher >|> semicolonMatcher
    match res with 
    | Ok (tokens, rem) -> Ok (InferredVariableDeclarationSyntax (tokens.[0], tokens.[1], tokens.[2], LiteralExpressionSyntax tokens.[3]))
    | Error _ -> Error ()


let rec matchAssignmentExpression input = // TODO : PlusEquals, MinusEquals ...
    let res = input |> identifierMatcher >|> equalsOperatorMatcher 
    match res with
    | Ok (tokens, rem) -> 
        let innerExprRes = matchAssignmentExpression rem
        match innerExprRes with
        | Ok m ->  
            match m with
                | ExpressionSyntax es ->  
                    let ret = AssignmentExpressionSyntax (tokens.[0], tokens.[1], es)
                    Ok (ExpressionSyntax ret)
                | _ -> Error () //Unexpected case
        | Error _ -> Error ()
    | Error _ -> Error ()

let matchIdentifierExpression input =
    match input |> identifierMatcher with
    | Ok (s, rem) -> Ok ([IdentifierExpressionSyntax s.[0]], rem)
    | Error _ -> Error ()

let matchLiteralExpression input = 
    match input |> literalMatcher with
    | Ok (s, rem) -> Ok ([LiteralExpressionSyntax s.[0]], rem )
    | Error _ -> Error ()

let rec parsePrimaryExpression input =
    match input with
    | [] -> Error ()
    | head::tail when head.kind = TokenSyntaxKind WhitespaceToken -> tail |> parsePrimaryExpression
    | head::tail ->
        match head.kind with
        | TokenSyntaxKind NumberToken -> matchLiteralExpression input
        | TokenSyntaxKind IdentifierToken -> matchIdentifierExpression input
        | _ -> failwithf "Unsupported token kind: %A" head.kind

let getUnaryOperatorPrecedence syntaxKind = 
    match syntaxKind with
    | TokenSyntaxKind PlusToken
    | TokenSyntaxKind MinusToken
    | TokenSyntaxKind BangToken
    | TokenSyntaxKind TildeToken
    | TokenSyntaxKind BangToken
        -> 6
    | _ -> 0

let getBinaryOperatorPrecedence syntaxKind = 
    match syntaxKind with
    | TokenSyntaxKind StarToken
    | TokenSyntaxKind SlashToken
        -> 5
    | TokenSyntaxKind PlusToken
    | TokenSyntaxKind MinusToken
        -> 4
    | TokenSyntaxKind EqualsEqualsToken
    | TokenSyntaxKind BangEqualsToken
    | TokenSyntaxKind LessToken
    | TokenSyntaxKind LessOrEqualsToken
    | TokenSyntaxKind GreaterToken
    | TokenSyntaxKind GreaterOrEqualsToken
        -> 3
    | TokenSyntaxKind AmpersandToken
    | TokenSyntaxKind AmpersandAmpersandToken
        -> 2
    | TokenSyntaxKind PipeToken
    | TokenSyntaxKind PipePipeToken
    | TokenSyntaxKind HatToken
        -> 1
    | _ -> 0

let matchBinaryOperator input = 
     input |> (plusOperatorMatcher <|> minusOperatorMatcher  <|>  starOperatorMatcher <|> slashOperatorMatcher <|> ampersandOperatorMatcher <|> ampersandAmpersandOperatorMatcher
     <|> pipeOperatorMatcher <|> pipePipeOperatorMatcher  <|> equalsEqualsOperatorMatcher <|> bangEqualsOperatorMatcher
     <|> lessOrEqualsOperatorMatcher  <|> lessOperatorMatcher <|> greaterOrEqualsOperatorMatcher <|> greaterOperatorMatcher)


let parseBinaryExpression input = 
    let rec parseBinaryExpressionInternal input = 
        let rPrimExpr = input |> parsePrimaryExpression
        match rPrimExpr with
        | Error _ -> 
            //printfn "1"
            Error ()
        | Ok (lPrimExpr, remPrimExpr) -> 
            let rOperator = remPrimExpr |> matchBinaryOperator
            match rOperator with
            | Error _ -> 
                //printfn "2" //Error ()     /////////////////////////////////////////////// ERROR ! LENGTH = 0 due to recursive call
                Error ()
            | Ok (lOperator, remOperator) -> 
                let rSecondExpr = remOperator |> (parseBinaryExpressionInternal <|> parsePrimaryExpression)
                match rSecondExpr with
                | Error _ -> 
                    //printfn "3" 
                    Error ()
                | Ok (lSecondExpr,remSecondExpr) -> Ok ([BinaryExpressionSyntax (lPrimExpr.[0], lOperator.[0], lSecondExpr.[0])], remSecondExpr)

    let tmp = input |> parseBinaryExpressionInternal 
    match tmp with
    | Ok (l, rem) -> Ok (ExpressionSyntax l.[0])
    | Error _ -> Error ()
    

let matchExpression (input: SyntaxToken list) = 
    input |> (matchAssignmentExpression 
               <|> parseBinaryExpression)

[<EntryPoint>]
let main argv =

    // let input = "123 + !toto / +- +=+ if else var tutu 4/12"

    // let tokens = tokenizer input

    // let input = "var toto = 123;"
    let input = "1 + 1 > 1 + 0"
    let tokens = tokenizer input
    let statement = matchExpression tokens

    printfn "%A" statement

    0