module Replacement

open Lexer
open Parser
open Pattern
open System.Collections.Generic 

let patternTokenClasses =
   [ {Name="_CommaStarHole";   Regex= @"\$[a-zA-Z][a-zA-Z0-9]*,\*";   Ignored=false}
     {Name="_CommaPlusHole";   Regex= @"\$[a-zA-Z][a-zA-Z0-9]*,\+";   Ignored=false}
     {Name="_OptionalHole";    Regex= @"\$[a-zA-Z][a-zA-Z0-9]*\?";    Ignored=false}
     {Name="_RepeatStarHole";  Regex= @"\$[a-zA-Z][a-zA-Z0-9]*\*";    Ignored=false}
     {Name="_RepeatPlusHole";  Regex= @"\$[a-zA-Z][a-zA-Z0-9]*\+";    Ignored=false}
     {Name="_NonterminalHole"; Regex= @"\$[a-zA-Z][a-zA-Z0-9]*";      Ignored=false} ]

let private holePattern className {Type=tokenType; RawText=tokenText; Location=_} =
    match tokenType with
    | TokenClass name when name = className -> Some (tokenText.[1..].TrimEnd('*', '?', '+', ','))
    | _ -> None

let private (|AnonymousNonterminalHole|_|)       = holePattern "_NonterminalHole"
let private (|AnonymousOptionalHole|_|)          = holePattern "_OptionalHole"
let private (|AnonymousRepeatHole|_|)            = holePattern "_RepeatStarHole"
let private (|AnonymousOneOrMoreRepeatHole|_|)   = holePattern "_RepeatPlusHole"
let private (|AnonymousOneOrMoreCSVHole|_|)      = holePattern "_CommaPlusHole"
let private (|AnonymousCSVHole|_|)               = holePattern "_CommaStarHole"

/// Replacement parser
let parse topLevelGoal (grammar: EbnfGrammar) isTokenClass (tokens: LinkedList<Token>) =
    // This keeps track of how far we got in the token stream during the parse attempt
    let mutable failureToken = tokens.First
    let incrementFailureToken() = failureToken <- failureToken.Next

    /// Returns a sequence of valid parses of the goal (EbnfNode) starting at 'token'.
    /// Each valid parse is a tuple (P,T) where
    ///      P is the resulting parse tree,
    ///      T is the new position in the token stream (after tokens have been consumed by the parse for P), and
    /// An empty sequence means no parse is possible.
    let rec parses (token: LinkedListNode<Token>) = function
        | CaseInsensitiveTerminal terminalText ->
            if terminalText.ToLower() = token.Value.RawText.ToLower() then
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next) 
            else
                Seq.empty
        
        | Terminal terminalText -> 
            if terminalText = token.Value.RawText then
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next) 
            else
                Seq.empty
        
        | Nonterminal nonterminalName -> 
            match token.Value with
            | AnonymousNonterminalHole holeType when holeType = nonterminalName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->
                if isTokenClass nonterminalName then
                    if token.Value.Type = TokenClass nonterminalName then 
                        if failureToken = token then incrementFailureToken()
                        Seq.singleton (PNonterminal (nonterminalName, PTerminal token.Value), token.Next)
                    else
                        Seq.empty
                elif grammar.ContainsKey(nonterminalName) then
                    parses token grammar.[nonterminalName] |> Seq.map (fun (n,t) -> PNonterminal(nonterminalName, n), t)
                else
                    failwithf "Unrecognized nonterminal '%s'" nonterminalName
        
        | Optional node -> 
            match token.Value, node with
            | AnonymousOptionalHole holeType, Nonterminal ntName when holeType = ntName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->
                seq {
                    yield! parses token node |> Seq.map (fun (n,t) -> POptional(Some n), t) 
                    yield POptional None, token
                }
               
        | Choice nodes -> 
            seq { 
                for i = 0 to nodes.Length - 1 do 
                    yield! parses token nodes.[i] 
                           |> Seq.map (fun (n,t) -> PChoice(i, nodes.Length, n), t)
            }
        
        | Repeat node ->
            match token.Value, node with
            | AnonymousRepeatHole holeType, Nonterminal ntName when holeType = ntName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->
                let headParses = parses token node
                if Seq.isEmpty headParses then 
                    Seq.singleton (PRepeat [], token) 
                else
                    seq { 
                        for (headParse, positionAfter) in headParses do
                            let tailParses = parses positionAfter (Repeat node)
                            yield! tailParses 
                                   |> Seq.filter (function PTerminal(AnonymousRepeatHole _), _ -> false | _ -> true)
                                   |> Seq.map (fun (PRepeat tailParse, pos) -> PRepeat(headParse::tailParse), pos)
                    }
        
        | OneOrMoreRepeat node -> 
            match token.Value, node with
            | AnonymousOneOrMoreRepeatHole holeType, Nonterminal ntName when holeType = ntName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->        
                parses token (Repeat node) 
                |> Seq.filter (function PRepeat [], _ | PTerminal(AnonymousRepeatHole _), _ -> false | _ -> true)

        | OneOrMoreCommaSeparatedRepeat node ->
            match token.Value, node with
            | AnonymousOneOrMoreCSVHole holeType, Nonterminal ntName when holeType = ntName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->
                let repeatParses = parses token (Order [node; Repeat(Order [Terminal ","; node])])
                if Seq.isEmpty repeatParses then
                    Seq.empty
                else
                    repeatParses
                    |> Seq.map (fun (POrder [first; PRepeat orderList], positionAfter) ->
                        PCommaSeparatedRepeat(first::(orderList |> List.map (function POrder [_; elem] -> elem))), positionAfter)

        | CommaSeparatedRepeat node ->
            match token.Value, node with
            | AnonymousCSVHole holeType, Nonterminal ntName when holeType = ntName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->
                let repeatParses = 
                    parses token (OneOrMoreCommaSeparatedRepeat node)
                    |> Seq.filter (function PTerminal(AnonymousOneOrMoreCSVHole _), _ -> false | _ -> true)
                if Seq.isEmpty repeatParses then 
                    Seq.singleton (PCommaSeparatedRepeat [], token)
                else
                    repeatParses

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
    
    // gather all parses of the top level goal
    match Seq.tryHead (parses tokens.First grammar.[topLevelGoal]) with
    | None -> SyntaxError failureToken
    | Some(parseTree, endToken) -> 
        match endToken.Value.Type with
        | EndOfFile -> Success(PNonterminal(topLevelGoal, parseTree))
        | _ -> UnconsumedInputError failureToken

/// Constructs a new tree by filling in any holes in a replacement pattern tree
let rec construct (subtrees: System.Collections.Queue) = function
    | PTerminal(AnonymousNonterminalHole _)  -> subtrees.Dequeue() :?> ParseTreeNode
    | PTerminal(AnonymousOptionalHole _)     -> POptional(subtrees.Dequeue() :?> ParseTreeNode option)
    | PTerminal(AnonymousOneOrMoreRepeatHole holeType) ->
        let list = subtrees.Dequeue() :?> ParseTreeNode list
        if list.IsEmpty then
            failwithf "Placeholder '$%s+' expects a non-empty list but was given an empty list" holeType
        else
            PRepeat list
    | PTerminal(AnonymousRepeatHole _)       -> PRepeat(subtrees.Dequeue() :?> ParseTreeNode list)
    | PTerminal(AnonymousOneOrMoreCSVHole holeType) ->
        let list = subtrees.Dequeue() :?> ParseTreeNode list
        if list.IsEmpty then
            failwithf "Placeholder '$%s,+' expects a non-empty list but was given an empty list" holeType
        else
            PCommaSeparatedRepeat list
    | PTerminal(AnonymousCSVHole _) -> PCommaSeparatedRepeat(subtrees.Dequeue() :?> ParseTreeNode list)
    | PTerminal terminal            -> PTerminal terminal
    | PNonterminal(name, node)      -> PNonterminal(name, construct subtrees node)
    | POrder nodes                  -> POrder(List.map (construct subtrees) nodes)
    | PChoice(index, count, node)   -> PChoice(index, count, construct subtrees node)
    | POptional(Some node)          -> POptional(Some(construct subtrees node))
    | POptional None                -> POptional None
    | PRepeat nodes                 -> PRepeat(List.map (construct subtrees) nodes)
    | PCommaSeparatedRepeat nodes   -> PCommaSeparatedRepeat(List.map (construct subtrees) nodes)

let rec findAllHoleTypes = function
    | PTerminal(AnonymousNonterminalHole holeType)      -> [holeType]
    | PTerminal(AnonymousOptionalHole holeType)         -> [holeType + "?"]
    | PTerminal(AnonymousRepeatHole holeType)           -> [holeType + "*"]
    | PTerminal(AnonymousOneOrMoreRepeatHole holeType)  -> [holeType + "+"]
    | PTerminal(AnonymousOneOrMoreCSVHole holeType)     -> [holeType + ",+"]
    | PTerminal(AnonymousCSVHole holeType)              -> [holeType + ",*"]
    | PTerminal _ 
    | POptional None -> []
    | POptional(Some node) 
    | PChoice(_, _, node) 
    | PNonterminal(_, node) -> findAllHoleTypes node
    | POrder nodes
    | PCommaSeparatedRepeat nodes
    | PRepeat nodes -> List.collect findAllHoleTypes nodes