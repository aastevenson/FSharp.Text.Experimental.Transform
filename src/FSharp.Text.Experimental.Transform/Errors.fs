module Errors

open System.Collections.Generic
open Lexer

let calcCol (text: string) charIndex = charIndex - text.LastIndexOf('\n', charIndex)

let calcRow (text: string) charIndex =
    let mutable index = text.LastIndexOf('\n', charIndex)
    let mutable newlineCount = 1
    while index <> -1 do
        index <- text.LastIndexOf('\n', index - 1)
        newlineCount <- newlineCount + 1
    newlineCount

let scanMessage charIndex (inputText: string) =
    let nextNchars n = 
        if charIndex + n > inputText.Length then 
            inputText.Substring(charIndex) 
        else 
            inputText.Substring(charIndex, n)
    
    sprintf "at the text beginning with '%s' on line %d at column %d" 
        (nextNchars 5) (calcRow inputText charIndex) (calcCol inputText charIndex) 

let parseMessage (tokenPosition: LinkedListNode<Token>) fileText =
    let prev (tokenPosition: LinkedListNode<Token>) = 
        if tokenPosition.Previous = null then None else Some tokenPosition.Previous
    
    let next (tokenPosition: LinkedListNode<Token>) = 
        if tokenPosition.Next = null then None else Some tokenPosition.Next
    
    let textOf (token: Token) =
        match token.Type with
        | EndOfFile -> "EOF"
        | _ -> token.RawText

    // returns the two tokens preceeding 'tokenPosition' (if f = prev)
    // or the two tokens following 'tokenPosition' (if f = next)
    let context f =
        (tokenPosition, 2) 
        |> List.unfold (fun (tok, num) -> 
            match f tok with
            | None -> None
            | Some (fTok: LinkedListNode<Token>) -> 
                if num = 0 then None
                else Some(textOf fTok.Value, (fTok, num - 1)))

    let preContext = context prev |> List.rev |> String.concat " " 
    let postContext = context next |> String.concat " "

    sprintf "on line %d at token:\n  %s >>> %s <<< %s" 
        (calcRow fileText tokenPosition.Value.Location) preContext (textOf tokenPosition.Value) postContext