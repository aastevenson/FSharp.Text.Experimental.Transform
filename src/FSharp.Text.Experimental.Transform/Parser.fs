module Parser

open System.Collections.Generic
open Lexer

type EbnfNode =
    | Order                             of EbnfNode list
    | Choice                            of EbnfNode list
    | Repeat                            of EbnfNode
    | OneOrMoreRepeat                   of EbnfNode
    | CommaSeparatedRepeat              of EbnfNode
    | OneOrMoreCommaSeparatedRepeat     of EbnfNode
    | Optional                          of EbnfNode
    | Nonterminal                       of string
    | Terminal                          of string
    | CaseInsensitiveTerminal           of string 

type EbnfGrammar = IDictionary<string, EbnfNode>

type ParseTreeNode =
    | POrder                of ParseTreeNode list
    | PChoice               of chosenIndex:int * totalChoices:int * chosen:ParseTreeNode
    | PRepeat               of ParseTreeNode list
    | PCommaSeparatedRepeat of ParseTreeNode list
    | POptional             of ParseTreeNode option
    | PNonterminal          of name:string * contents:ParseTreeNode
    | PTerminal             of Token

type ParseResult<'T> =
    | Success of 'T
    | SyntaxError of LinkedListNode<Token>
    | UnconsumedInputError of LinkedListNode<Token>

let parseAs topLevelGoal (grammar: EbnfGrammar) isTokenClass (tokens: LinkedList<Token>) =
    // This keeps track of how far we got in the token stream during the parse attempt
    let mutable failureToken = tokens.First
    let incrementFailureToken() = failureToken <- failureToken.Next

    /// Returns a sequence of valid parses of the goal (EbnfNode) starting at 'token'.
    /// Each valid parse is a tuple (P,T) where
    ///      P is the resulting parse tree, and
    ///      T is the new position in the token stream (after tokens have been consumed by the parse for P).
    /// An empty sequence means no parse is possible.
    let rec parses (token: LinkedListNode<Token>) = function
        | CaseInsensitiveTerminal text ->
            if text.ToLower() = token.Value.RawText.ToLower() then 
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next) 
            else Seq.empty
        
        | Terminal text ->
            if text = token.Value.RawText then 
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next) 
            else Seq.empty
        
        // The Nonterminal goal succeeds if its RHS production in the grammar succeeds
        // or the nonterminal represents a token class that matches the current token.
        // Each valid result is wrapped in a PNonterminal parse tree node
        | Nonterminal nonterminalName -> 
            if isTokenClass nonterminalName then
                if token.Value.Type = TokenClass nonterminalName then 
                    if failureToken = token then incrementFailureToken()
                    Seq.singleton (PNonterminal (nonterminalName, PTerminal token.Value), token.Next)
                else
                    Seq.empty
            elif grammar.ContainsKey nonterminalName then
                parses token grammar.[nonterminalName] |> Seq.map (fun (n,t) -> PNonterminal(nonterminalName, n), t)
            else
                failwithf "Unrecognized nonterminal '%s'" nonterminalName
        
        // The Optional goal is all the possibilities for valid 'node' parses followed by the single None option.
        // The Optional goal will always succeed because it can always derive (POptional None)
        | Optional node -> 
            seq {
                yield! parses token node |> Seq.map (fun (n,t) -> POptional(Some n), t) 
                yield POptional None, token
            }
                
        // The Choice goal succeeds iff any one of its alternatives succeeds
        | Choice nodes -> 
            seq { 
                for i = 0 to nodes.Length - 1 do 
                    yield! parses token nodes.[i] 
                           |> Seq.map (fun (n,t) -> PChoice(i, nodes.Length, n), t)
            }
        
        // The Repeat goal attempts to match the longest repetition of 'node'.
        // It does this recursively by first finding all possible parses of a single 'node', then for each of those,
        // inventing a new Repeat goal to solve at the new token position. The first match 'headParse' is then combined 
        // with the invented match 'tailParse' to yield the final PRepeat parse tree.
        // This goal will always succeed because it can derive empty (i.e. PRepeat [])
        | Repeat node ->
            let headParses = parses token node
            if Seq.isEmpty headParses then 
                Seq.singleton (PRepeat [], token) 
            else
                seq { 
                    for (headParse, positionAfter) in headParses do
                        let tailParses = parses positionAfter (Repeat node)
                        yield! tailParses |> Seq.map (fun (PRepeat tailParse, t) -> PRepeat(headParse::tailParse), t) 
                }
        
        // The OneOrMoreRepeat goal is the same result as Repeat but without an empty PRepeat parse
        | OneOrMoreRepeat node -> parses token (Repeat node) |> Seq.filter (fun (p,_) -> p <> PRepeat [])

        // The (node),+ goal is the same as solving for (node (',' node)*)
        | OneOrMoreCommaSeparatedRepeat node ->
            let repeatParses = parses token (Order [node; Repeat(Order [Terminal ","; node])])
            if Seq.isEmpty repeatParses then
                Seq.empty
            else
                repeatParses
                |> Seq.map (fun (POrder [first; PRepeat orderList], positionAfter) ->
                    PCommaSeparatedRepeat(first::(orderList |> List.map (function POrder [_; elem] -> elem))), positionAfter)

        // The (node),* goal is the same as (node),+ except if it fails, then (node),* succeeds with empty
        | CommaSeparatedRepeat node ->
            let repeatParses = parses token (OneOrMoreCommaSeparatedRepeat node)
            if Seq.isEmpty repeatParses then 
                Seq.singleton (PCommaSeparatedRepeat [], token)
            else
                repeatParses

        // The Order goal succeeds iff each goal in 'nodes' succeeds, applied consecutively.
        | Order nodes ->
            match nodes with
            | [] -> Seq.singleton (POrder [], token)
            | head::tail ->
                let headParses = parses token head
                seq { 
                    for (headParse, positionAfter) in headParses do
                        let tailOrderParses = parses positionAfter (Order tail)
                        yield! tailOrderParses |> Seq.map (fun (POrder tailParse, t) -> POrder(headParse::tailParse), t)
                }
    
    // gather all parses of the starting nonterminal (Program)
    match Seq.tryHead (parses tokens.First grammar.[topLevelGoal]) with
    | None -> SyntaxError failureToken
    | Some(parseTree, endToken) -> 
        match endToken.Value.Type with
        | EndOfFile -> Success (PNonterminal(topLevelGoal, parseTree))
        | _ -> UnconsumedInputError failureToken


let parse grammar = parseAs "Program" grammar

