(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../src/FSharp.Text.Experimental.Transform/bin/Debug/FSharp.Text.Experimental.Transform.dll"
#r "FParsecCS.dll"
#r "FParsec.dll"
open FParsec
let str = pstring

(**
Parsing with FSharp.Text.Experimental.Transform
===============================================

Although the FSharp.Text.Experimental.Transform library excels at text transformation tasks, its
powerful backtracking parser can be used on its own. This parser is implemented as an F# type
provider, so navigating the produced AST is akin to navigating an object model customized for
your grammar. The provided methods make it easy to do deep searches into the parse tree to find
and extract the data you want without manually traversing the parts of the parse tree you don't
care about. Some such methods are:

*   `Contains`: Returns true if *subtree* is contained somewhere in *tree*, otherwise returns false.
    <pre><em>tree</em>.Contains(<em>subtree</em>)</pre>
    
*   `TryFind`: Searches in *tree* for a *Nonterminal* subtree that matches *pattern*. Returns 
    Some(variable bindings) for the first matching subtree found, or returns None if no match is found.
    <pre><em>tree</em>.TryFind<em>Nonterminal</em>< <em>pattern</em> >()</pre>

*   `FindAll`: Returns all instances of subtrees of type *Nonterminal* within *tree*.
    <pre><em>tree</em>.FindAll<em>Nonterminal</em>()</pre>

*   `FindAllMatching`: Searches in *tree* for all *Nonterminal* subtrees that match *pattern*. Returns 
    a list of variable bindings, one set of bindings per match. See <a href="#e2">Example 2</a> below
    for an example of its use.
    <pre><em>tree</em>.FindAll<em>Nonterminal</em>Matching< <em>pattern</em> >()</pre>

See the [API Reference](api.html) for more information about these methods.

Comparison with FParsec
-----------------------

[FParsec](http://www.quanttec.com/fparsec/) is an excellent parser combinator library that is often the first 
library people recommend for parsing in F#. FParsec and FSharp.Text.Experimental.Transform can both be used 
to parse text, but their approaches differ. Below is a short list of the major differences between the two libraries:

*   FParsec works by combining small, special-purpose parsers into larger, more complex parsers. 
    FSharp.Text.Experimental.Transform's parser works by interpreting a single monolithic complete grammar definition.

*   FParsec is a scannerless parser; every character in the input string must be accounted for in the grammar rules.
    In contrast, FSharp.Text.Experimental.Transform uses a lexer to tokenize the input string in an initial scanning
    phase, before the parser is invoked. The most relevant consequence of this is that FParsec grammar rules must deal 
    with whitespace explicitly while FSharp.Text.Experimental.Transform rules do not because the scanner automatically 
    ignores whitespace tokens. FParsec has the advantage of being able to parse text where whitespace is significant,
    which is one of the [limitations]() of FSharp.Text.Experimental.Transform.

*   A grammar in FParsec is defined using a series of F# functions and custom operators, in essence F# code. This
    allows all the benefits of IDE features such as intellisense, syntax highlighting, etc. while constructing the
    grammar, but is less intuitive than a direct syntax. A grammar in FSharp.Text.Experimental.Transform is defined 
    with a custom syntax that closely resembles Extended Backus-Naur Form. This is much more intuitive, but must be
    defined in a separate text file or F# string literal, losing the benefits of syntax highlighting and navigation.

*   Both are top-down parsers with infinite lookahead, but differ slightly in their backtracking behaviour. For example,
    FParsec's default alternative operator `<|>` will try the right-hand side only if the left-hand side fails *without consuming
    any input*, and works this way [for performance reasons](http://www.quanttec.com/fparsec/users-guide/parsing-alternatives.html#:FN:1).
    FSharp.Text.Experimental.Transform alternatives are fully backtracking, meaning the right-hand alternative is always 
    tried if the left-hand alternative fails.

To get a better sense of the differences between the two libraries let's consider a side-by-side comparison of a JSON parser in each.
We'll use the FParsec implementation from the [FParsec tutorial](http://www.quanttec.com/fparsec/tutorial.html#parsing-json), and compare
each step with its FSharp.Text.Experimental.Transform equivalent.

<table border="1">
<tr><th>FParsec</th><th>FSharp.Text.Experimental.Transform</th></tr>
<tr>
<td>Associate grammar elements with F# types:
*)
type Json = 
    | JString of string
    | JNumber of float
    | JBool   of bool
    | JNull
    | JList   of Json list
    | JObject of Map<string, Json>
(**
</td>
<td>
Not applicable. Simple types (int, float, string, etc.) are automatically associated, and complex types must be handled separately.
</td>
</tr>
<tr>
<td> Cover the simple null case:
*)
let jnull = stringReturn "null" JNull
(**
</td>
<td><pre>Null   :   'null'; </pre></td>
</tr>
<tr>
<td>Cover the boolean case:
*)
let jbool =      (stringReturn "true"  (JBool true))
             <|> (stringReturn "false" (JBool false))
(**
</td>
<td>
The rule for boolean literals is unnecessary because it is implemented by default in FSharp.Text.Experimental.Transform.
Simply refer to the nonterminal 'Bool' in the grammar and it will be included.
</td>
</tr>
<tr>
<td>Cover the number case:
*)
let jnumber = pfloat |>> JNumber
(**
</td>
<td>Not applicable. Again, the number type is provided by default simply by referencing 'Num' somewhere in the grammar.</td>
</tr>
<tr>
<td>Define a string literal consistent with the JSON specification:
*)
let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)
let jstring = stringLiteral |>> JString
(**
</td>
<td> String literals are defined using regular expressions. We must override the default 'Stringlit' token class with
a new definition, using the <tt>terminal</tt> keyword:
<pre>terminal Stringlit :   '"(\\([\\"/bnfrt]|u[0-9a-fA-F]{4})|[^"\\])*"'; </pre>
</td>
</tr>
<tr>
<td>Create a forward reference to a parser we have not yet defined:
*)
let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()
(**
</td>
<td>Not applicable. An explicit forward reference is needed by FParsec for recursive grammar definitions, but
FSharp.Text.Experimental.Transform resolves these automatically.</td>
</tr>
<tr>
<td>Cover all whitespace characters:
*)
let ws = spaces
(**
</td>
<td>Not applicable. Whitespace is ignored by default.</td>
</tr>
<tr>
<td>Cover the list case:
*)
let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)
let jlist   = listBetweenStrings "[" "]" jvalue JList
(**
</td>
<td>
<pre>
List    :   '[' Value,* ']';
</pre>
</td>
<tr>
<td>Cover the key-value pairs and object cases:
*)
let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)
let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)
(**
</td>
<td><pre>
KeyValue    :   Stringlit ':' Value;
Object      :   '{' KeyValue,* '}';
</pre></td>
</tr>
<tr>
<td>Combine all cases into a choice:
*)
do jvalueRef := 
    choice [jobject
            jlist
            jstring
            jnumber
            jbool
            jnull]
(**
</td>
<td>
<pre>
Value   :   Object 
        |   List 
        |   Stringlit 
        |   Num 
        |   Bool 
        |   Null;
</pre>
</td>
</tr>
<tr>
<td>Define top-level parser:
*)
let json = ws >>. jvalue .>> ws .>> eof
(**
</td>
<td><pre>
Program    :   Value;
</pre></td>
</tr>
</table>

That concludes the grammar definition. Here are both grammar definitions shown in full side by side:
<table border="1">
<tr><th>FParsec</th><th>FSharp.Text.Experimental.Transform</th></tr>
<tr>
<td>
*)
type Json = 
    | JString of string
    | JNumber of float
    | JBool   of bool
    | JNull
    | JList   of Json list
    | JObject of Map<string, Json>

let jnull = stringReturn "null" JNull
let jbool =      (stringReturn "true"  (JBool true))
             <|> (stringReturn "false" (JBool false))
let jnumber = pfloat |>> JNumber
let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)
let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()
let ws = spaces
let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)
let jlist   = listBetweenStrings "[" "]" jvalue JList
let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)
let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)
do jvalueRef := 
    choice [jobject
            jlist
            jstring
            jnumber
            jbool
            jnull]
let json = ws >>. jvalue .>> ws .>> eof

(**
</td>
<td>
*)
open FSharp.Text.Experimental.Transform

[<Literal>]
let grammar = """
    terminal Stringlit :   '"(\\([\\"/bnfrt]|u[0-9a-fA-F]{4})|[^"\\])*"';

    Program     :   Value;
    Value       :   Object 
                |   List 
                |   Stringlit 
                |   Num 
                |   Bool 
                |   Null;
    KeyValue    :   Stringlit ':' Value;
    Object      :   '{' KeyValue,* '}';
    List        :   '[' Value,* ']';
    Null        :   'null'; 
"""

type JSON = GrammarProvider<grammar>

(**
</td>
</tr>
</table>

--------

This comparison is not quite apples-to-apples because the FParsec parser is doing something extra: identifying
parts of the AST that are interesting to us and converting them to F#-friendly values. The FSharp.Text.Experimental.Transform
version simply expresses the structure of the input text, but does not say how to extract the useful bits.

So let's go a bit further and see how one might extract information from a concrete [JSON data sample](http://json.org/example.html).
*)
let sample = """
    {"widget": {
        "debug": "on",
        "window": {
            "title": "Sample Konfabulator Widget",
            "name": "main_window",
            "width": 500,
            "height": 500
        },
        "image": { 
            "src": "Images/Sun.png",
            "name": "sun1",
            "hOffset": 250,
            "vOffset": 250,
            "alignment": "center"
        },
        "text": {
            "data": "Click Here",
            "size": 36,
            "style": "bold",
            "name": "text1",
            "hOffset": 250,
            "vOffset": 100,
            "alignment": "center",
            "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
        }
    }}
"""
(**
<table border="1">
<tr><th>FParsec</th><th>FSharp.Text.Experimental.Transform</th></tr>
<tr>
<td><b>Example 1.</b><br> Suppose we want to get the widget's text size. Navigating the nested structure of the sample above is 
accomplished in FParsec by navigating the <tt>Json</tt> discriminated union.
*)
match run json sample with
| Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
| Success((JObject result), _, _) ->
    let (JObject widget) = result.["widget"]
    let (JObject text) = widget.["text"]
    let (JNumber size) = text.["size"]
    printfn "The widget's text size is %f" size
(**
</td>
<td>
Here is the equivalent task using FSharp.Text.Experimental.Transform. The provided object model can also 
be traversed directly but it is more idiomatic to use the provided <tt>TryFind</tt> methods to extract the information you seek:
*)
let program = JSON.ParseString sample
let textProperties = program.TryFindKeyValue<""" "text" : text$Object """>().Value.Text
let size = textProperties.TryFindKeyValue<""" "size" : size$Num """>().Value.Size
printfn "The widget's text size is %f" (size.ToFloat())
(**
</td>
</tr>
<tr>
<td id="e2"><b>Example 2.</b><br> Consider collecting all the values for "name" properties in the sample above (lines 6, 12, and 21).
With FParsec, one can write a recursive function to navigate the data structures in the <tt>Json</tt> 
discriminated union type:
*)
match run json sample with
| Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
| Success(result, _, _) ->
    let rec findNames = function
        | JObject map -> 
            if Map.containsKey "name" map then 
                findNames map.["name"]
            else 
                map |> Map.toList |> List.map snd |> List.collect findNames
        | JString str -> [str]
        | _ -> []
    
    printfn "Sample contains the following 'name' values: %A" (findNames result)
(**
</td>
<td>FSharp.Text.Experimental.Transform accomplishes this task with the <tt>FindAllKeyValuesMatching</tt> method:
*)
program.FindAllKeyValuesMatching<""" "name" : value$Stringlit """>()
|> List.map (fun b -> b.Value)
|> printfn "Sample contains the following 'name' values: %A"
(**
</td>
</tr>
</table>
*)
