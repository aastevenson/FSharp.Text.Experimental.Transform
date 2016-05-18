module Lexer

open System.Text.RegularExpressions
open System.Collections.Generic

type TokenType =
    | TokenClass of string
    | EndOfFile

[<CustomEquality; NoComparison>]
type Token = 
    { RawText: string
      Type: TokenType
      Location: int }
      override x.Equals(yObj : obj) =       // ignore Location in structural equality
        match yObj with
        | :? Token as y -> (x.RawText, x.Type) = (y.RawText, y.Type)
        | _ -> false
      override x.GetHashCode() = hash (x.RawText, x.Type)

type TokenClassDefinition =
    { Name: string
      Regex: string
      Ignored: bool }

type ScanResult =
    | SuccessfulScan of LinkedList<Token>
    | UnmatchableText of charIndex:int

let private regexify tokenClasses = 
    tokenClasses 
    |> List.map (fun {Name=name; Regex=regex; Ignored=isIgnored} -> 
        match name with
        | "CIKeyword"   -> name, Regex("\G" + regex, RegexOptions.Compiled ||| RegexOptions.IgnoreCase), isIgnored
        | _             -> name, Regex("\G" + regex, RegexOptions.Compiled), isIgnored )
let private trimLastChar (str: string) = str.Substring(0, str.Length - 1)
let private lastCharIsAlphaNum (str: string) = 
    let lastChar = str.[str.Length - 1]
    (lastChar >= 'A' && lastChar <= 'Z') || (lastChar >= 'a' && lastChar <= 'z')

// returns a token stream ignoring whitespace
let tokenize tokenClasses (fileText: string) =
    let allTokens = LinkedList<Token>()
    let tokenClasses = 
        {Name="Whitespace"; Regex="\s+"; Ignored=true}::tokenClasses
        |> regexify

    let mutable syntaxError = None

    let rec createTokens startIndex =
        if startIndex < fileText.Length then 
            let matchResult = tokenClasses |> List.tryFind (fun (_, regex, _) -> regex.IsMatch(fileText, startIndex))
            match matchResult with
            | Some(tokenType, regex, isIgnored) ->                
                let regexMatch = regex.Match(fileText, startIndex)
                let isKeywordWithWSSuffix = (tokenType = "Keyword" || tokenType = "CIKeyword") && not (lastCharIsAlphaNum regexMatch.Value)
                if not isIgnored then
                    let tokenText = if isKeywordWithWSSuffix then trimLastChar regexMatch.Value else regexMatch.Value
                    allTokens.AddLast {RawText = tokenText; Type = TokenClass tokenType; Location = startIndex} |> ignore
                let tokenLength = if isKeywordWithWSSuffix then regexMatch.Length - 1 else regexMatch.Length
                createTokens (startIndex + tokenLength)
            | None -> syntaxError <- Some startIndex

    createTokens 0
    match syntaxError with
    | Some failureIndex -> UnmatchableText failureIndex
    | None -> 
        let eofToken = { RawText = ""; Type = EndOfFile; Location = allTokens.Last.Value.Location}
        allTokens.AddLast(eofToken) |> ignore
        SuccessfulScan allTokens
