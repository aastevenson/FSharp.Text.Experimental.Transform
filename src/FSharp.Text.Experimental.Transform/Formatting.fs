module Formatting

open Lexer
open Parser
open System.Collections.Generic

type FormattingCue =
    | Newline
    | Space
    | Indent
    | Dedent

type FormatEbnfNode =
    | FOrder                            of FormatEbnfNode list
    | FChoice                           of FormatEbnfNode list
    | FRepeat                           of FormatEbnfNode
    | FOneOrMoreRepeat                  of FormatEbnfNode
    | FCommaSeparatedRepeat             of FormatEbnfNode
    | FOneOrMoreCommaSeparatedRepeat    of FormatEbnfNode
    | FOptional                         of FormatEbnfNode
    | FNonterminal                      of string
    | FTerminal                         of string
    | FCaseInsensitiveTerminal          of string
    | Format                            of FormattingCue

type FormatEbnfGrammar = IDictionary<string, FormatEbnfNode>

let rec private stripFormattingCues = function
    | FOrder nodes -> 
        nodes 
        |> List.filter (function Format _ -> false | _ -> true)
        |> List.map stripFormattingCues
        |> FOrder
    | FChoice nodes         -> FChoice(List.map stripFormattingCues nodes)
    | FRepeat node          -> FRepeat(stripFormattingCues node)
    | FOneOrMoreRepeat node -> FOneOrMoreRepeat(stripFormattingCues node)
    | FCommaSeparatedRepeat node             -> FCommaSeparatedRepeat(stripFormattingCues node)
    | FOneOrMoreCommaSeparatedRepeat node    -> FOneOrMoreCommaSeparatedRepeat(stripFormattingCues node)
    | FOptional node        -> FOptional(stripFormattingCues node)
    | Format cue ->
        let cueName =
            match cue with
            | Space     -> "space formatter (_)"
            | Newline   -> "newline formatter (/)"
            | Indent    -> "indent formatter (>)"
            | Dedent    -> "dedent formatter (<)"
        failwithf "Grammar definition cannot contain %s by itself" cueName
    | node -> node

let rec private eliminateOrderSingletons = function
    | Order [node]                          -> eliminateOrderSingletons node
    | Order nodes                           -> Order(List.map eliminateOrderSingletons nodes)
    | Choice nodes                          -> Choice(List.map eliminateOrderSingletons nodes)
    | Repeat node                           -> Repeat(eliminateOrderSingletons node)
    | OneOrMoreRepeat node                  -> OneOrMoreRepeat(eliminateOrderSingletons node)
    | CommaSeparatedRepeat node             -> CommaSeparatedRepeat(eliminateOrderSingletons node)
    | OneOrMoreCommaSeparatedRepeat node    -> OneOrMoreCommaSeparatedRepeat(eliminateOrderSingletons node)
    | Optional node                         -> Optional(eliminateOrderSingletons node)
    | Terminal literalText                  -> Terminal literalText
    | CaseInsensitiveTerminal literalText   -> CaseInsensitiveTerminal literalText
    | Nonterminal nonterminalName           -> Nonterminal nonterminalName

let rec private unformatEbnfNode = function
    | FChoice nodes                         -> Choice(List.map unformatEbnfNode nodes)
    | FOrder nodes                          -> Order(List.map unformatEbnfNode nodes)
    | FRepeat node                          -> Repeat(unformatEbnfNode node)
    | FOneOrMoreRepeat node                 -> OneOrMoreRepeat(unformatEbnfNode node)
    | FCommaSeparatedRepeat node            -> CommaSeparatedRepeat(unformatEbnfNode node)
    | FOneOrMoreCommaSeparatedRepeat node   -> OneOrMoreCommaSeparatedRepeat(unformatEbnfNode node)
    | FOptional node                        -> Optional(unformatEbnfNode node)
    | FNonterminal nonterminalName          -> Nonterminal nonterminalName
    | FTerminal literalText                 -> Terminal literalText
    | FCaseInsensitiveTerminal literalText  -> CaseInsensitiveTerminal literalText
    | Format cue                            -> failwithf "Unexpected formatting cue '%A' encountered" cue

let unformatEbnfGrammar (grammar: FormatEbnfGrammar) =
    let newGrammar = Dictionary<string, EbnfNode>()
    for production in grammar do
        newGrammar.Add(production.Key, production.Value
                                       |> stripFormattingCues
                                       |> unformatEbnfNode
                                       |> eliminateOrderSingletons )
    newGrammar

let private last5lines buffer =
    buffer.ToString().Split('\n')
    |> Array.rev
    |> Array.truncate 5
    |> Array.rev
    |> String.concat "\n"

/// Returns a pretty-printed string representing a parse tree
let pretty (grammar: FormatEbnfGrammar) startNode =
    let mutable indentLevel = 0
    let buffer = System.Text.StringBuilder()
    let emitToken (token: Lexer.Token) =
        if buffer.Length > 0 && buffer.Chars(buffer.Length - 1) = '\n' then buffer.Append(String.replicate indentLevel "    ") |> ignore
        buffer.Append token.RawText |> ignore
        buffer.Append ' ' |> ignore

    let rec emit = function
        | _, FOrder [] -> ()                            // base case: no order nodes left to process
        | node, FOrder(Format cue::fs) ->               // emit format cue
            match cue with
            | Space -> buffer.Append ' ' |> ignore
            | Newline -> buffer.Append '\n' |> ignore
            | Indent -> indentLevel <- indentLevel + 1
            | Dedent -> 
                indentLevel <- indentLevel - 1
                if indentLevel < 0 then 
                    failwithf "Indent level became negative with < (dedent) formatter. Unparse failed at:\n%s" (last5lines buffer)
            emit(node, FOrder fs)
        | POrder(n::ns), FOrder(f::fs) ->               // emit non-format node
            emit(n, f)
            emit(POrder ns, FOrder fs)
        | node, FOrder(f::fs) -> 
            emit(node, f)
            emit(node, FOrder fs)
        | PNonterminal(name, node), FNonterminal fName when name = fName && grammar.ContainsKey name ->
            emit(node, grammar.[fName])
        | PNonterminal(name, PTerminal token), FNonterminal fName when name = fName && not (grammar.ContainsKey name) ->
            emitToken token
        | PRepeat nodes, (FRepeat fNode | FOneOrMoreRepeat fNode) ->
            List.iter (fun node -> emit(node, fNode)) nodes
        | PCommaSeparatedRepeat nodes, (FCommaSeparatedRepeat fNode | FOneOrMoreCommaSeparatedRepeat fNode) ->
            match nodes with
            | [] -> ()
            | [node] -> emit(node, fNode)
            | head::tail ->
                emit(head, fNode)
                for node in tail do
                    emitToken {RawText=","; Type=TokenClass "Punctuation"; Location=0}
                    emit(node, fNode)
        | POptional None, FOptional _ -> ()
        | POptional(Some node), FOptional fNode -> emit(node, fNode)
        | PChoice(index, _, node), FChoice fNodes -> emit(node, fNodes.[index])
        | PTerminal token, FTerminal _ -> emitToken token
        | node, fNode -> failwithf "Unexpected formatting combination:\n%A\nwith\n%A" node fNode

    match startNode with
    | PTerminal token -> emitToken token
    | PNonterminal(name, PTerminal token) when not (grammar.ContainsKey name) -> emitToken token
    | PNonterminal(name, node) -> emit(node, grammar.[name])
    | node -> failwithf "Unparsing expected a named nonterminal, but got '%s' instead" (node.GetType().Name)

    // apply default space suppression around some characters (e.g. before list separators, around brackets)
    let result =
        buffer.Replace("( ", "(").Replace(" )", ")")      // remove spaces around brackets
              .Replace("[ ", "[").Replace(" ]", "]")
              .Replace(" . ", ".")
              .Replace(" ,", ",").Replace(" ;", ";")
              .Replace(" \n", "\n")
    result.ToString().TrimEnd(' ')
        