module Documentation

open System.Text.RegularExpressions
open Parser

let rec private uniqueNames name (suffix: int) (patternStr: string) =
    let index = patternStr.IndexOf(sprintf "%s$" name)
    if index = -1 then patternStr else patternStr.Insert(index + 3, string suffix) |> uniqueNames name (suffix + 1)

let private uniqueVars      = uniqueNames "var" 1
let private uniqueLists     = uniqueNames "list" 1
let private uniqueFirsts    = uniqueNames "first" 1
let private uniqueSeconds   = uniqueNames "second" 1
let private uniqueRests     = uniqueNames "rest" 1

let private countDollarSigns (patternStr: string) = patternStr.Split('$').Length - 1
let private extractVars (patternStr: string) =
    let capitalize (str: string) = str.Substring(0,1).ToUpper() + str.Substring(1)
    [ for m in Regex("(var|list|first|second|rest)[1-9]\$").Matches(patternStr) -> 
        "b." + capitalize m.Value.[0 .. m.Value.Length - 2]
    ] |> String.concat " "

let rec tryMatchExample = function
    | Nonterminal name              -> "var$" + name, "%A", "b.Var"
    | CaseInsensitiveTerminal text
    | Terminal text                 -> text, text, ""
    | Choice nodes                  -> tryMatchExample nodes.[0]
    | Optional(Nonterminal name)    -> sprintf "var$%s?" name, "%A", "b.Var"
    | Repeat(Nonterminal name)      -> 
        sprintf "first$%s second$%s rest$%s*" name name name, 
        "first=%A, second=%A rest=%A" , 
        "b.First b.Second b.Rest"
    | CommaSeparatedRepeat(Nonterminal name)      -> 
        sprintf "first$%s, second$%s, rest$%s,*" name name name, 
        "first=%A, second=%A rest=%A" , 
        "b.First b.Second b.Rest"
    | OneOrMoreRepeat(Nonterminal name) -> sprintf "list$%s+" name, "%A", "b.List"
    | Optional node                 
    | Repeat node
    | CommaSeparatedRepeat node
    | OneOrMoreCommaSeparatedRepeat node
    | OneOrMoreRepeat node          -> tryMatchExample node
    | Order nodes                   ->
        let patterns, _, _ = 
            nodes 
            |> List.map tryMatchExample 
            |> List.unzip3
        let newPattern = 
            patterns 
            |> String.concat " " 
            |> uniqueVars
            |> uniqueLists
            |> uniqueFirsts
            |> uniqueSeconds
            |> uniqueRests
        newPattern, String.replicate (countDollarSigns newPattern) "%A ", extractVars newPattern

let tryMatchMethod topLevelGoal =
    sprintf 
        """<summary>
        <para>Tries to match this object with a parse pattern. The parse pattern is specified by a static string parameter that 
        must be a valid '%s'. This pattern may contain wildcards of the form var$Type where 'var' is a variable
        identifier and 'Type' is a grammar nonterminal or token class. If a match is found, the pattern is unified with this
        object's parse tree and each wildcard variable will be bound to its corresponding subtree. Anonymous variables are
        specified by omitting the variable identifier (e.g. $Type).</para>

        <para>Returns Some(BindingType) if a match exists, otherwise returns None. The BindingType class gives access to 
        wildcard variables bound in the pattern.</para>
                       
        <para>Sample use:</para>

        <para>match my%s.TryMatch&lt;"$%s"&gt;() with </para>
        <para>| Some b -&gt; printfn "Match found" </para>
        <para>| None -&gt; printfn "No match" </para>
        </summary>
        """ topLevelGoal topLevelGoal topLevelGoal

let tryFindMethod targetName =
    sprintf 
        """<summary>
        <para>Searches this object to find a %s subtree that matches the specified parse pattern. The parse pattern 
        is specified by a static string parameter that must be a valid '%s'. This pattern may contain wildcards of the form 
        var$Type where 'var' is a variable identifier and 'Type' is a grammar nonterminal or token class. If a match is found, 
        the pattern is unified with the matching parse tree and each wildcard variable will be bound to its corresponding 
        subtree. Anonymous variables are specified by omitting the variable identifier (e.g. $Type).</para>

        <para>Returns Some(BindingType) if a match exists, otherwise returns None. The BindingType class gives access to 
        wildcard variables bound in the pattern.</para>
                       
        <para>Sample use:</para>

        <para>match myTree.TryFind&lt;"%s pattern"&gt;() with </para>
        <para>| Some b -&gt; printfn "Match found"</para>
        <para>| None -&gt; printfn "No match" </para>
        </summary>
        """ targetName targetName targetName

let findAllMatchingMethod =
    sprintf "Returns a list of values for each pattern variable from subtrees that match the specified pattern."

let constructMethod topLevelGoal =
    sprintf 
        """<summary>
        <para>Constructs a new %s object based on a parse pattern. The parse pattern is specified by a static string parameter that 
        may contain holes of the form $Type where 'Type' is a grammar nonterminal or token class, and which are filled in by the 
        values provided in this method's parameters.</para>

        <para>Returns a new %s object</para>""" topLevelGoal topLevelGoal

let applyOnePassMethod searchType =
    sprintf  """Performs one pass over the parse tree, applying the given transformation function to each '%s' element. 
                Nested %s elements are not transformed because this operation is not recursively applied to replacements.
                This operation is guaranteed to return.""" searchType searchType

let applyRecursivelyMethod searchType =
    sprintf  """Applies the given transformation function to each '%s' element in the parse tree, and recursively applies the
                transformation function to any replacements that are made. Multiple passes of the tree are performed until no
                further changes are made by the transformation function. CAUTION: This operation is not guaranteed to return;
                if the replacement can be unified with the match pattern this operation will never return.""" searchType