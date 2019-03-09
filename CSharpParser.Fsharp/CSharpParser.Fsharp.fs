module Program

open System

type BinaryOperator =
    | Plus
    | Minus
type UnaryOperator =
    | Bang

type SyntaxKind = 
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

let isWhitespace char = 
    char = ' '

let rec getTokenText (input: char list) = 
    match input with 
        | [] -> "", []
        | [head] -> head.ToString(), []
        | head::tail ->
            match head with 
            | x when  isWhitespace x ->  "", tail
            | x -> 
                let text, tail = getTokenText tail

                //if text is a token =>  return it else add another iteration ! !=
                x.ToString() + text, tail

let rec parseTokens input =
    match getTokenText input with
        | "", _ -> []
        | tokenText, tail -> 
            match tokenText with 
                | x when isNumber x -> { position = 0; text = tokenText; kind = SyntaxKind.Integer }:: (parseTokens tail)
                | "+" -> { position = 0; text = tokenText; kind = BinaryOperator Plus }:: (parseTokens tail)
                | "!" -> { position = 0; text = tokenText; kind = UnaryOperator Bang }:: (parseTokens tail)
                | _ -> (parseTokens tail) //Handle whitechars



let rec parseExpressions input = 


    match input |> List.map (fun i -> i.kind) with
    | [] -> []
    | Integer::BinaryOperator _::Integer::_ -> BinaryExpression (input.[0],input.[1],input.[2]) :: (parseExpressions input.[3..])
    | UnaryOperator _::Integer::_ -> UnaryExpression (operand= input.[1], operator=input.[0]) :: (parseExpressions input.[2..])
    | _ -> []
                
[<EntryPoint>]
let main argv =
    let input = "123 + 132 2 + 2 ! 3"
    let tokens = parseTokens (input.ToCharArray() |> List.ofArray)
      
    printfn "%A" tokens

    let expressions = parseExpressions tokens
    printfn "%A" expressions

    0