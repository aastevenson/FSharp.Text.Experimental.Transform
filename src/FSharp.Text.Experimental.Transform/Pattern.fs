module Pattern

open Lexer
open Parser
open System.Collections.Generic    

[<Literal>] 
let ANONYMOUS_VAR = ""

let patternTokenClasses =
   [ {Name="_CommaStarHole";   Regex= @"([a-zA-Z][a-zA-Z0-9]*)?\$[a-zA-Z][a-zA-Z0-9]*,\*";   Ignored=false}
     {Name="_CommaPlusHole";   Regex= @"([a-zA-Z][a-zA-Z0-9]*)?\$[a-zA-Z][a-zA-Z0-9]*,\+";   Ignored=false}
     {Name="_OptionalHole";    Regex= @"([a-zA-Z][a-zA-Z0-9]*)?\$[a-zA-Z][a-zA-Z0-9]*\?";    Ignored=false}
     {Name="_RepeatStarHole";  Regex= @"([a-zA-Z][a-zA-Z0-9]*)?\$[a-zA-Z][a-zA-Z0-9]*\*";    Ignored=false}
     {Name="_RepeatPlusHole";  Regex= @"([a-zA-Z][a-zA-Z0-9]*)?\$[a-zA-Z][a-zA-Z0-9]*\+";    Ignored=false}
     {Name="_NonterminalHole"; Regex= @"([a-zA-Z][a-zA-Z0-9]*)?\$[a-zA-Z][a-zA-Z0-9]*";      Ignored=false} ]

let private holePattern className {Type=tokenType; RawText=tokenText; Location=_} =
    match tokenType with
    | TokenClass name when name = className ->
        let split = tokenText.Split('$', '?', '*', '+', ',')
        let varName = split.[0]
        let typeName = split.[1]
        Some(varName, typeName)
    | _ -> None

let private (|NonterminalHole|_|)       = holePattern "_NonterminalHole"
let private (|OptionalHole|_|)          = holePattern "_OptionalHole"
let private (|RepeatHole|_|)            = holePattern "_RepeatStarHole"
let private (|OneOrMoreRepeatHole|_|)   = holePattern "_RepeatPlusHole"
let private (|OneOrMoreCSVHole|_|)      = holePattern "_CommaPlusHole"
let private (|CSVHole|_|)               = holePattern "_CommaStarHole"

let rec findAllNamedHoles = function
    | PTerminal(NonterminalHole(holeVar, holeType))     -> if holeVar = ANONYMOUS_VAR then [] else [holeVar, holeType]
    | PTerminal(OptionalHole(holeVar, holeType))        -> if holeVar = ANONYMOUS_VAR then [] else [holeVar, holeType + "?"]
    | PTerminal(RepeatHole(holeVar, holeType))          -> if holeVar = ANONYMOUS_VAR then [] else [holeVar, holeType + "*"]
    | PTerminal(OneOrMoreRepeatHole(holeVar, holeType)) -> if holeVar = ANONYMOUS_VAR then [] else [holeVar, holeType + "+"]
    | PTerminal(OneOrMoreCSVHole(holeVar, holeType))    -> if holeVar = ANONYMOUS_VAR then [] else [holeVar, holeType + ",+"]
    | PTerminal(CSVHole(holeVar, holeType))             -> if holeVar = ANONYMOUS_VAR then [] else [holeVar, holeType + ",*"]
    | PTerminal _ 
    | POptional None -> []
    | POptional(Some node) 
    | PChoice(_, _, node) 
    | PNonterminal(_, node) -> findAllNamedHoles node
    | POrder nodes
    | PCommaSeparatedRepeat nodes
    | PRepeat nodes -> List.collect findAllNamedHoles nodes
    
/// Pattern parser
let parse topLevelGoal (grammar: EbnfGrammar) isTokenClass (tokens: LinkedList<Token>) =
    // This keeps track of how far we got in the token stream during the parse attempt
    let mutable failureToken = tokens.First
    let incrementFailureToken() = failureToken <- failureToken.Next

    /// Returns a sequence of valid parses of the goal (EbnfNode) starting at 'token'.
    /// Each valid parse is a tuple (P,T,V) where
    ///      P is the resulting parse tree,
    ///      T is the new position in the token stream (after tokens have been consumed by the parse for P), and
    ///      V is a map from variable names to their types (grammar nonterminals)
    /// An empty sequence means no parse is possible.
    let rec parses (token: LinkedListNode<Token>) = function
        | CaseInsensitiveTerminal literalText ->
            if literalText.ToLower() = token.Value.RawText.ToLower() then
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next) 
            else
                Seq.empty
        
        | Terminal literalText -> 
            if literalText = token.Value.RawText then
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next) 
            else
                Seq.empty
        
        | Nonterminal nonterminalName -> 
            match token.Value with
            | NonterminalHole(_, holeType) when holeType = nonterminalName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->
                if isTokenClass nonterminalName then
                    if token.Value.Type = TokenClass nonterminalName then 
                        if failureToken = token then incrementFailureToken()
                        Seq.singleton (PNonterminal(nonterminalName, PTerminal token.Value), token.Next)
                    else
                        Seq.empty
                elif grammar.ContainsKey nonterminalName then
                    parses token grammar.[nonterminalName] |> Seq.map (fun (n,t) -> PNonterminal(nonterminalName, n), t)
                else
                    failwithf "Unrecognized nonterminal '%s'" nonterminalName
        
        | Optional node -> 
            match token.Value, node with
            | OptionalHole(_, holeType), Nonterminal ntName when holeType = ntName ->
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
            | RepeatHole(_, holeType), Nonterminal ntName when holeType = ntName ->
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
                                   |> Seq.filter (function PTerminal(RepeatHole _), _ -> false | _ -> true)
                                   |> Seq.map (fun (PRepeat tailParse, t) -> PRepeat(headParse::tailParse), t) 
                    }
        
        | OneOrMoreRepeat node -> 
            match token.Value, node with
            | OneOrMoreRepeatHole(_, holeType), Nonterminal ntName when holeType = ntName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->        
                parses token (Repeat node) 
                |> Seq.filter (function PRepeat [], _ | PTerminal(RepeatHole _), _ -> false | _ -> true)

        | OneOrMoreCommaSeparatedRepeat node ->
            match token.Value, node with
            | OneOrMoreCSVHole(_, holeType), Nonterminal ntName when holeType = ntName ->
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
            | CSVHole(_, holeType), Nonterminal ntName when holeType = ntName ->
                if failureToken = token then incrementFailureToken()
                Seq.singleton (PTerminal token.Value, token.Next)
            | _ ->
                let repeatParses = 
                    parses token (OneOrMoreCommaSeparatedRepeat node)
                    |> Seq.filter (function PTerminal(OneOrMoreCSVHole _), _ -> false | _ -> true)
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

module Matcher =
    let private bindHoleVariable holeVar value oldBindings =
        match holeVar with
        | ANONYMOUS_VAR -> oldBindings                        // anonymous hole variable, nothing new to bind
        | _ -> Map.add holeVar value oldBindings

    /// Takes a set of variable bindings and a (patternNode, candidateNode) pair. 
    /// returns Some(variable bindings) if they match, otherwise None
    let rec tryMatch bindings = function
        // hole of form var$Type matches PNonterminal("Type", subNode) and binds var to the match
        | PTerminal (NonterminalHole(holeVar, holeType)), PNonterminal(ntName, node) when holeType = ntName ->
            matchOrBind holeVar bindings (PNonterminal(ntName, node))
        // hole of form var$Type? matches POptional(Some(PNonterminal("Type", value))) and binds var to the match
        | PTerminal (OptionalHole(holeVar, holeType)), POptional(Some(PNonterminal(ntName, node))) when holeType = ntName -> 
            matchOrBind holeVar bindings (POptional(Some(PNonterminal(ntName, node))))
        // hole of form var$Type? matches POptional None and binds var to the match
        | PTerminal (OptionalHole(holeVar, holeType)), POptional None -> 
            matchOrBind holeVar bindings (POptional None)
        // hole of form var$Type* matches a repeat list of nonterminals of 'Type', 
        // i.e. PRepeat [PNonterminal("Type",_); PNonterminal("Type",_); ... ; PNonterminal("Type",_)]
        | PTerminal (RepeatHole(holeVar, holeType)), PRepeat list
            when list |> List.forall (function PNonterminal(ntName, _) when holeType = ntName -> true | _ -> false) -> 
                matchOrBind holeVar bindings (PRepeat list)
        // hole of form var$Type+ matches a repeat list with length > 0 and nonterminals of 'Type', 
        // i.e. PRepeat [PNonterminal("Type",_); PNonterminal("Type",_); ... ; PNonterminal("Type",_)]
        | PTerminal (OneOrMoreRepeatHole(holeVar, holeType)), PRepeat list
            when list.Length > 0 &&
                 list |> List.forall (function PNonterminal(ntName, _) when holeType = ntName -> true | _ -> false) -> 
                matchOrBind holeVar bindings (PRepeat list)
        // hole of form var$Type,+ match a comma separated list with length > 0 and nonterminals of 'Type',
        // i.e. PCSV [PNonterminal("Type", _); ...; PNonterminal("Type", _)]
        | PTerminal (OneOrMoreCSVHole(holeVar, holeType)), PCommaSeparatedRepeat list
            when list.Length > 0 &&
                 list |> List.forall (function PNonterminal(ntName, _) when holeType = ntName -> true | _ -> false) ->
                matchOrBind holeVar bindings (PCommaSeparatedRepeat list)
        // hole of form var$Type,* matches a comma separated list of nonterminals of 'Type', 
        // i.e. PCSV [PNonterminal("Type",_); PNonterminal("Type",_); ... ; PNonterminal("Type",_)]
        | PTerminal (CSVHole(holeVar, holeType)), PCommaSeparatedRepeat list
            when list |> List.forall (function PNonterminal(ntName, _) when holeType = ntName -> true | _ -> false) -> 
                matchOrBind holeVar bindings (PCommaSeparatedRepeat list)
        // the following cases are non-hole matches that don't bind any variables directly, although their subnodes might
        | PTerminal {Type=patternType; RawText=patternText; Location=_}, 
          PTerminal {Type=candidateType; RawText=candidateText; Location=_} 
            when (patternType, patternText) = (candidateType, candidateText) -> Some bindings
        | PNonterminal(patternName, patternNode), PNonterminal(candidateName, candidateNode)
            when patternName = candidateName -> tryMatch bindings (patternNode, candidateNode)
        | POptional None, POptional None -> Some bindings
        | POptional(Some patternNode), POptional(Some candidateNode) -> tryMatch bindings (patternNode, candidateNode)
        | PCommaSeparatedRepeat patternNodes, PCommaSeparatedRepeat candidateNodes ->
            match patternNodes, candidateNodes with
            | [], [] -> Some bindings
            | patternHead::patternTail, candidateHead::candidateTail ->
                match tryMatch bindings (patternHead, candidateHead) with
                | Some newBindings -> tryMatch newBindings (PCommaSeparatedRepeat patternTail, PCommaSeparatedRepeat candidateTail)
                | None -> None
            | _ -> None
        | PRepeat patternNodes, PRepeat candidateNodes ->
            match patternNodes, candidateNodes with
            | [], [] -> Some bindings
            | patternHead::patternTail, candidateHead::candidateTail ->
                match tryMatch bindings (patternHead, candidateHead) with
                | Some newBindings -> tryMatch newBindings (PRepeat patternTail, PRepeat candidateTail)
                | None -> None
            | _ -> None
        | POrder patternNodes, POrder candidateNodes 
            when patternNodes.Length = candidateNodes.Length ->
                List.zip patternNodes candidateNodes
                |> List.fold (fun maybeMatch pair ->
                        match maybeMatch with
                        | Some vars -> tryMatch vars pair
                        | None -> None )   (Some bindings)
        | PChoice(patternIndex, patternCount, patternNode), PChoice(candidateIndex, candidateCount, candidateNode)
            when (patternIndex, patternCount) = (candidateIndex, candidateCount) -> tryMatch bindings (patternNode, candidateNode)
        // no match
        | _ -> None

    // if 'holeVar' is a previously bound variable then try to match its contents, otherwise treat it as a new declaration and bind it
    and matchOrBind holeVar bindings candidateNode =
        match Map.tryFind holeVar bindings with
        | Some holeValue -> tryMatch bindings (holeValue, candidateNode)
        | None           -> bindHoleVariable holeVar candidateNode bindings |> Some

    let rec tryFind patternTree = function
        | PNonterminal(name, node)  -> tryMatch Map.empty (patternTree, PNonterminal(name, node))
        | PChoice(_, _, node)
        | POptional(Some node)      -> tryFind patternTree node
        | POptional None
        | PTerminal _               -> None
        | PRepeat nodes
        | PCommaSeparatedRepeat nodes
        | POrder nodes              -> List.tryPick (tryFind patternTree) nodes

    let rec findAll targetName = function
        | PNonterminal(name, node) when name = targetName -> PNonterminal(name, node)::(findAll targetName node)
        | PNonterminal(_, node)
        | PChoice(_, _, node)
        | POptional(Some node)      -> findAll targetName node
        | POptional None
        | PTerminal _               -> []
        | PRepeat nodes
        | PCommaSeparatedRepeat nodes
        | POrder nodes              -> List.collect (findAll targetName) nodes

    let rec findAllMatching targetName patternTree candidateTree =
        match candidateTree with
        | PNonterminal(name, node) when name = targetName ->
            match tryMatch Map.empty (patternTree, candidateTree) with
            | Some bindings -> bindings::(findAllMatching targetName patternTree node)
            | None          -> findAllMatching targetName patternTree node
        | PNonterminal(_, node) 
        | PChoice(_, _, node)
        | POptional(Some node)  -> findAllMatching targetName patternTree node
        | POptional None
        | PTerminal _           -> []
        | PRepeat nodes
        | PCommaSeparatedRepeat nodes
        | POrder nodes          -> List.collect (findAllMatching targetName patternTree) nodes

    let rec contains targetNode = function
        | node when node = targetNode   -> true
        | PTerminal _
        | POptional None                -> false
        | POptional(Some node)
        | PChoice(_, _, node)
        | PNonterminal(_, node)         -> contains targetNode node
        | PRepeat nodes
        | PCommaSeparatedRepeat nodes
        | POrder nodes                  -> List.exists (contains targetNode) nodes
