module Bootstrap

open Lexer
open Parser
open Formatting

type Statement =
    | RuleStatement of ruleName:string * definition:FormatEbnfNode
    | TerminalStatement of TokenClassDefinition

// Program:         (RuleStmt | TerminalStmt)*
// TerminalStmt:    'ignore'? 'terminal' Id ':' (Stringlit | Charlit) ';'
// RuleStmt:        Id ':' Expression ';'
// Expression:      Disjunct ('|' Disjunct)*
// Disjunct:        (Multiplicity | FormattingCue)+
// Multiplicity:    Primary ('?'|'*'|'+'|',*'|',+')?
// Primary:         Id | Charlit | Stringlit | '(' Expression ')'
// FormattingCue:   '_' | '/' | '>' | '<'
let bootstrapGrammar =
    dict [ 
        "Program",      Repeat(Choice [Nonterminal "RuleStmt"; Nonterminal "TerminalStmt"])
        "TerminalStmt", Order [Optional(Terminal "ignore"); Terminal "terminal"; Nonterminal "Id"; Terminal ":"; 
                                Choice [Nonterminal "Stringlit"; Nonterminal "Charlit"]; Terminal ";"]
        "RuleStmt",     Order [Nonterminal "Id"; Terminal ":"; Nonterminal "Expression"; Terminal ";"]
        "Expression",   Order [Nonterminal "Disjunct"; Repeat(Order [Terminal "|"; Nonterminal "Disjunct"])]
        "Disjunct",     OneOrMoreRepeat(Choice [Nonterminal "Multiplicity"; Nonterminal "FormattingCue"])
        "Multiplicity", Order [Nonterminal "Primary"; Optional(Choice [Terminal "?"; Terminal "*"; Terminal "+"; Terminal ",*"; Terminal ",+"])]
        "Primary",      Choice [Nonterminal "Id"; Nonterminal "Charlit"; Nonterminal "Stringlit"; Order [Terminal "("; Nonterminal "Expression"; Terminal ")"]]
        "FormattingCue",Choice [Terminal "_"; Terminal "/"; Terminal ">"; Terminal "<";]
    ]

let bootstrapTokenClasses = 
    GrammarAnalysis.grammarKeywordsAndPunctuation bootstrapGrammar @ 
        [{Name="Id";        Regex="[a-zA-Z][a-zA-Z0-9_]*";  Ignored=false}
         {Name="Charlit";   Regex="'[^'\\n]+'";                Ignored=false}
         {Name="Stringlit"; Regex="\"[^\"\\n]+\"";             Ignored=false}
         {Name="Comment";   Regex="%.*?\\n";                Ignored=true} ]

let defaultTokenClasses = [
        {Name="Id";            Regex="[a-zA-Z][a-zA-Z0-9_]*";   Ignored=false}
        {Name="Charlit";       Regex="'[^'\\n]+'";              Ignored=false}
        {Name="Stringlit";     Regex="\"([^\"\\n]|\\\\\")*\"";  Ignored=false}
        {Name="Dec";           Regex="-?[0-9]*.[0-9]+";         Ignored=false}
        {Name="Nat";           Regex="[0-9]+";                  Ignored=false}
        {Name="NegInt";        Regex="-[0-9]+";                 Ignored=false}
    ]

let defaultGrammarRules = [
        "Int",  FChoice [FNonterminal "Nat"; FNonterminal "NegInt"]
        "Num",  FChoice [FNonterminal "Dec"; FNonterminal "Nat"; FNonterminal "NegInt"]
        "Bool", FChoice [FTerminal "true"; FTerminal "false"]
    ]

let unquote (str: string) = str.[1 .. str.Length - 2]

let compileFormattingCue = function
    | PNonterminal("FormattingCue", PChoice(_,_, PTerminal formattingCue)) ->
        match formattingCue.RawText with
        | "_" -> Format Space
        | "/" -> Format Newline
        | ">" -> Format Indent
        | "<" -> Format Dedent
        | _ -> failwith "Unrecognized formatting cue"
    | _ -> failwith "Internal error: compileFormattingCue"

let rec compilePrimary = function
    | PNonterminal("Primary", PChoice(0, _, PNonterminal("Id", PTerminal idToken))) -> FNonterminal idToken.RawText
    | PNonterminal("Primary", PChoice(1, _, PNonterminal("Charlit", PTerminal charlitToken))) ->
        FTerminal (unquote charlitToken.RawText)
    | PNonterminal("Primary", PChoice(2, _, PNonterminal("Stringlit", PTerminal stringlitToken))) ->
        FCaseInsensitiveTerminal (unquote stringlitToken.RawText)
    | PNonterminal("Primary", PChoice(3, _, POrder [_; expr; _])) -> compileExpression expr
    | _ -> failwith "Internal error: compilePrimary"

and compileMultiplicity = function
    | PNonterminal("Multiplicity", POrder [node; POptional None]) -> compilePrimary node
    | PNonterminal("Multiplicity", POrder [node; POptional(Some (PChoice(_, _, PTerminal multToken)))]) ->
        match multToken.RawText with
        | "?"  -> FOptional (compilePrimary node)
        | "*"  -> FRepeat (compilePrimary node)
        | "+"  -> FOneOrMoreRepeat (compilePrimary node)
        | ",*" -> FCommaSeparatedRepeat (compilePrimary node)
        | ",+" -> FOneOrMoreCommaSeparatedRepeat (compilePrimary node)
        | _ -> failwith "Internal error: unrecognized multiplicity operator"
    | _ -> failwith "Internal error: compileMultiplicity"

and compileMultiplicityOrCue = function
    | PChoice(0, _, node) -> compileMultiplicity node
    | PChoice(1, _, node) -> compileFormattingCue node
    | _ -> failwith "Internal error: compileMultiplicityOrCue"

and compileDisjunct = function
    | PNonterminal("Disjunct", PRepeat multOrCues) ->
        match List.map compileMultiplicityOrCue multOrCues with
        | [node] -> node
        | nodes -> FOrder nodes
    | _ -> failwith "Internal error: compileDisjunct"

and compileOrDisjunct = function
    | POrder [_; node] -> compileDisjunct node
    | _ -> failwith "Internal error: compileOrDisjunct"

and compileExpression = function
    | PNonterminal("Expression", POrder [node; PRepeat []]) -> compileDisjunct node
    | PNonterminal("Expression", POrder [firstDisjunct; PRepeat orDisjuncts]) ->
        FChoice ((compileDisjunct firstDisjunct)::(List.map compileOrDisjunct orDisjuncts))
    | _ -> failwith "Internal error: compileExpression"

let compileRule = function
    | POrder [PNonterminal(_, PTerminal ruleToken); _; rhs; _] ->
        RuleStatement(ruleToken.RawText, compileExpression rhs)
    | _ -> failwith "Internal error: compileRule"

let compileTerminalStmt = function
    | POrder [POptional(ignored); _; PNonterminal("Id", PTerminal terminalToken); _; PChoice(_, _, PNonterminal(_, PTerminal regexToken)); _] ->
        try
            if System.Text.RegularExpressions.Regex.IsMatch("", unquote regexToken.RawText |> sprintf "^%s$") then
                failwithf "Regular expression %s for terminal '%s' must not match the empty string" regexToken.RawText terminalToken.RawText
        with
        | :? System.ArgumentException -> failwithf "Regular expression %s for terminal '%s' is malformed" regexToken.RawText terminalToken.RawText
        
        TerminalStatement{Name=terminalToken.RawText; Regex= unquote regexToken.RawText; Ignored=ignored.IsSome}
    | _ -> failwith "Internal error: compileTerminalStmt"

let compileStatement = function
    | PChoice(0, _, PNonterminal("RuleStmt", rule)) -> compileRule rule
    | PChoice(1, _, PNonterminal("TerminalStmt", terminalStmt)) -> compileTerminalStmt terminalStmt
    | _ -> failwith "Internal error: compileStatement"

let compileStatements = function
    | PNonterminal("Program", PRepeat statements) -> List.map compileStatement statements
    | _ -> failwith "Internal error: compileStatements"

let compileGrammar grammarNode =
    let statements = compileStatements grammarNode
    
    let parserRules = System.Collections.Generic.Dictionary<string, FormatEbnfNode>()
    statements 
    |> List.choose (function RuleStatement(name, node) -> Some(name, node) | _ -> None)
    |> List.iter parserRules.Add

    let lexerRules =
        statements
        |> List.choose (function TerminalStatement terminalDefinition -> Some terminalDefinition | _ -> None)

    let used = GrammarAnalysis.referencedSymbols parserRules
    defaultGrammarRules
    |> List.filter (fun (nonterminalName, _) -> not (parserRules.ContainsKey nonterminalName) && List.contains nonterminalName used)
    |> List.iter parserRules.Add
    let used = GrammarAnalysis.referencedSymbols parserRules
    let literals = 
        parserRules
        |> unformatEbnfGrammar
        |> GrammarAnalysis.grammarKeywordsAndPunctuation 
    
    // token class set (order matters) is: literals + (default + user-defined - unused)
    let tokenClasses =
        lexerRules @ defaultTokenClasses
        |> List.distinctBy (fun tc -> tc.Name)      // drop default token classes that are overridden by user-defined lexer rules
        |> List.filter (fun tokenClassDef -> List.contains tokenClassDef.Name used)
        |> List.append literals

    tokenClasses, parserRules

