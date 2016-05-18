module GrammarAnalysis

open Lexer
open Parser
open Formatting
open System.Text.RegularExpressions

/// Determines if a grammar node can derive the empty string
let canDeriveEmpty (grammar: EbnfGrammar) startNode =
    let rec derivesEmpty = function
        | Nonterminal symbol                    -> if grammar.ContainsKey symbol then derivesEmpty grammar.[symbol] else false
        | Optional _                            
        | Repeat _                              
        | CommaSeparatedRepeat _                -> true
        | Order nodes                           -> List.forall derivesEmpty nodes
        | OneOrMoreRepeat node
        | OneOrMoreCommaSeparatedRepeat node    -> derivesEmpty node
        | Choice nodes                          -> List.exists derivesEmpty nodes
        | CaseInsensitiveTerminal _
        | Terminal _                            -> false

    derivesEmpty startNode

/// Returns Some(cycle) if a direct or indirect left-recursive cycle exists, otherwise None.
let tryFindLeftRecursion (grammar: EbnfGrammar) =
    let rec cycle callChain = function
        | Nonterminal symbol when symbol = List.last callChain -> Some(symbol::callChain)
        | Order nodes -> 
            nodes
            |> List.skipWhile (fun node -> cycle callChain node = None && canDeriveEmpty grammar node)
            |> List.tryHead
            |> function Some node -> cycle callChain node | None -> None
        | Optional node
        | Repeat node
        | CommaSeparatedRepeat node
        | OneOrMoreCommaSeparatedRepeat node
        | OneOrMoreRepeat node -> cycle callChain node
        | Choice nodes -> List.tryPick (cycle callChain) nodes
        | Nonterminal symbol -> 
            if grammar.ContainsKey symbol then 
                cycle (symbol::callChain) grammar.[symbol]     // nonterminal reference, so recurse with augmented chain
            else None                                          // token class reference, so no left-recursion possible
        | CaseInsensitiveTerminal _
        | Terminal _ -> None

    [for production in grammar -> cycle [production.Key] production.Value] 
    |> List.tryPick id

/// Searches for and returns a nonterminal reference that is not defined, or None if all are defined
let tryFindUndefinedSymbol (grammar: EbnfGrammar) isTokenClass =
    let rec findUndefined = function
        | Order nodes
        | Choice nodes -> List.tryPick findUndefined nodes
        | Repeat node
        | OneOrMoreRepeat node
        | CommaSeparatedRepeat node
        | OneOrMoreCommaSeparatedRepeat node
        | Optional node -> findUndefined node
        | CaseInsensitiveTerminal _
        | Terminal _ -> None
        | Nonterminal nonterminalName -> 
            if not (grammar.ContainsKey nonterminalName) && not (isTokenClass nonterminalName) then Some nonterminalName else None

    Seq.tryPick findUndefined grammar.Values

let rec private containsCSV = function
    | Order nodes
    | Choice nodes -> List.exists containsCSV nodes
    | Repeat node
    | OneOrMoreRepeat node
    | Optional node -> containsCSV node
    | CommaSeparatedRepeat _
    | OneOrMoreCommaSeparatedRepeat _ -> true
    | Nonterminal _
    | CaseInsensitiveTerminal _
    | Terminal _ -> false

/// Returns all case sensitive terminals in a grammar that satisfy predicate 'p'
let rec private caseSensitiveTerminals p = function
    | Order nodes
    | Choice nodes -> List.collect (caseSensitiveTerminals p) nodes
    | Repeat node
    | OneOrMoreRepeat node
    | CommaSeparatedRepeat node
    | OneOrMoreCommaSeparatedRepeat node
    | Optional node -> caseSensitiveTerminals p node
    | Nonterminal _ -> []
    | CaseInsensitiveTerminal _ -> []
    | Terminal literalText ->  if (p literalText) then [literalText] else []

/// Returns all case insensitive terminals in a grammar that satisfy predicate 'p'
let rec private caseInsensitiveTerminals p = function
    | Order nodes
    | Choice nodes -> List.collect (caseInsensitiveTerminals p) nodes
    | Repeat node
    | OneOrMoreRepeat node
    | CommaSeparatedRepeat node
    | OneOrMoreCommaSeparatedRepeat node
    | Optional node -> caseInsensitiveTerminals p node
    | Nonterminal _ -> []
    | CaseInsensitiveTerminal literalText -> if (p literalText) then [literalText] else []
    | Terminal _ -> []

let caseSensitiveKeywords = caseSensitiveTerminals (Regex("^[a-zA-Z]+$").IsMatch)
let caseInsensitiveKeywords = caseInsensitiveTerminals (Regex("^[a-zA-Z]+$").IsMatch)
let grammarPunctuation root = 
    let regex = Regex("^[^a-zA-Z\s]+$").IsMatch
    (caseSensitiveTerminals regex root) @ (caseInsensitiveTerminals regex root)

let private escape (str: string) =
    [@"\"; "+"; "*"; "?"; "["; "]"; "."; "("; ")"; "^"; "$"; "|"] 
    |> List.fold (fun (escapedStr:string) ch -> escapedStr.Replace(ch, @"\" + ch)) str

let grammarKeywordsAndPunctuation (grammar: EbnfGrammar) =
    [ for production in grammar do
        yield! caseSensitiveKeywords production.Value 
               |> List.map (fun (s:string) -> {Name="Keyword"; Regex= s + "([^a-zA-Z]|$)"; Ignored=false})
        yield! caseInsensitiveKeywords production.Value 
               |> List.map (fun (s:string) -> {Name="CIKeyword"; Regex= s + "([^a-zA-Z]|$)"; Ignored=false})
        yield! grammarPunctuation production.Value 
               |> List.sortByDescending (fun (s:string) -> s.Length)
               |> List.map (fun (s:string) -> {Name="Punctuation"; Regex= escape s; Ignored=false}) 
        if containsCSV production.Value then yield {Name="Punctuation"; Regex=","; Ignored=false}
    ] |> List.distinct

let isPunctuationOrKeyword (tokenClass: TokenClassDefinition) =
    match tokenClass.Name with
    | "Punctuation" | "Keyword" | "CIKeyword" -> true
    | _ -> false

/// Returns a list of all the symbols that are referenced in the grammar
let referencedSymbols (grammar: FormatEbnfGrammar) = 
    let rec symbols = function
        | FOrder nodes
        | FChoice nodes -> List.collect symbols nodes
        | FRepeat node
        | FOneOrMoreRepeat node
        | FCommaSeparatedRepeat node
        | FOneOrMoreCommaSeparatedRepeat node
        | FOptional node -> symbols node
        | FNonterminal symbolName -> [symbolName]
        | FTerminal _
        | FCaseInsensitiveTerminal _
        | Format _ -> []

    grammar.Values
    |> Seq.collect symbols
    |> Seq.distinct
    |> Seq.toList

/// Returns a list of symbols that are reachable from an initial start node in the grammar tree
let reachableSymbols (grammar: EbnfGrammar) fromNode =
    let rec reachable reachableSoFar = function
        | Nonterminal symbol -> 
            if List.contains symbol reachableSoFar then             // we already know that symbol is reachable
                reachableSoFar
            elif not (grammar.ContainsKey symbol) then              // symbol is a token class (cannot recurse)
                symbol::reachableSoFar
            else                                                    // symbol is a grammar rule (recurse to reach more symbols)
                reachable (symbol::reachableSoFar) grammar.[symbol]
        | Order nodes
        | Choice nodes -> List.collect (reachable reachableSoFar) nodes |> List.distinct
        | Repeat node
        | OneOrMoreRepeat node
        | CommaSeparatedRepeat node
        | OneOrMoreCommaSeparatedRepeat node
        | Optional node -> reachable reachableSoFar node
        | CaseInsensitiveTerminal _
        | Terminal _ -> reachableSoFar

    reachable [] fromNode

/// Builds an n x n boolean matrix where n is the number of token classes & nonterminals in the grammar,
/// collectively known as symbols. 
/// m[s1,s2] = true means s1 contains a path to s2 (equivalently s2 is reachable starting at s1)
// The "reaches" relation is transitive, but is neither reflexive nor symmetric
let reachabilityMatrix (grammar: EbnfGrammar) allSymbols =
    let symbolCount = Array.length allSymbols
    let matrix = Array2D.zeroCreate symbolCount symbolCount
    for startIndex = 0 to allSymbols.Length - 1 do
        let startSymbol = allSymbols.[startIndex]
        if grammar.ContainsKey startSymbol then       // skip token classes because they don't reach anything
            for reachedSymbol in reachableSymbols grammar grammar.[startSymbol] do
                let reachedIndex = Array.findIndex ((=) reachedSymbol) allSymbols
                matrix.[startIndex, reachedIndex] <- true
    matrix

/// Returns a list of symbols that are reachable from an initial start node in the grammar tree
let reachableRepeatSymbols (grammar: EbnfGrammar) fromNode =
    let rec reachable (reachableSoFar, seenSoFar) = function
        | Repeat(Nonterminal symbol)
        | OneOrMoreRepeat(Nonterminal symbol) -> 
            if List.contains symbol reachableSoFar then             // we already know that symbol is reachable
                reachableSoFar
            elif not (grammar.ContainsKey symbol) then              // symbol is a token class (cannot recurse)
                symbol::reachableSoFar
            else                                                    // symbol is a grammar rule (recurse to reach more symbols)
                reachable (symbol::reachableSoFar, symbol::seenSoFar) grammar.[symbol]
        | Nonterminal symbol ->
            if List.contains symbol seenSoFar || not (grammar.ContainsKey symbol) then
                reachableSoFar
            else
                reachable (reachableSoFar, symbol::seenSoFar) grammar.[symbol]
        | Order nodes
        | Choice nodes -> List.collect (reachable (reachableSoFar, seenSoFar)) nodes |> List.distinct
        | Repeat node
        | OneOrMoreRepeat node
        | CommaSeparatedRepeat node
        | OneOrMoreCommaSeparatedRepeat node
        | Optional node -> reachable (reachableSoFar, seenSoFar) node
        | CaseInsensitiveTerminal _
        | Terminal _ -> reachableSoFar

    reachable ([], []) fromNode