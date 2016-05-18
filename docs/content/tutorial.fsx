(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../src/FSharp.Text.Experimental.Transform/bin/Debug/FSharp.Text.Experimental.Transform.dll"
[<Literal>]
let sample = """
    {"widget": {
        "debug": "on",
        "window": {
            "title": "Sample Konfabulator Widget",
            "name": "main_window",
            "width": 500,
            "height": 500
        }
    }}
"""

(**
Tutorial
========

This tutorial introduces the essential components of a text transformation and works through a 
simple JSON transformation task step by step. The tutorial is broken down into the following steps:

1.  [Define a grammar for the input text](#Define-a-grammar-for-the-input-text)
2.  [Identify the nonterminal target](#Identify-the-nonterminal-target)
3.  [Write the transformation function](#Write-the-transformation-function)
4.  [Write the main pipeline](#Write-the-main-pipeline)
5.  [Format the output](#Format-the-output)

We will be transforming the JSON sample below to add an empty string default property to every JSON string value.
For example, a key-value pair such as `"foo" : "bar"` will become `"foo" : { "value" : "bar", "default" : "" }`.

Here is the sample before and after the transformation:
<table>
<tr>
<th>Before</th><th>After</th>
</tr>
<td>
```
{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    }
}}
```
</td>
<td>
```
{"widget": {
    "debug": {
        "value" : "on",
        "default" : ""
    },
    "window": {
        "title": {
            "value" : "Sample Konfabulator Widget",
            "default" : ""
        },
        "name": {
            "value" : "main_window",
            "default" : ""
        },
        "width": 500,
        "height": 500
    }
}}
```
</td>
</tr>
</table>

## Define a grammar for the input text

In this section we define a general JSON grammar. We could define a schema-specific grammar, but that is what the
[JSON type provider](http://fsharp.github.io/FSharp.Data/library/JsonProvider.html) is for
so we'll stick with a general JSON grammar. A grammar is made up of multiple *productions*,
where each production has the form `Nonterminal : definition;`, and the start of the grammar is identified by the
special nonterminal 'Program'. (See more about the [grammar syntax](grammarsyntax.html)) 

We can begin our grammar construction by examining our JSON sample and noting any patterns or repetitions. One
obvious pattern is the idea of a key-value pair. We note the key is always a string literal but the value can
vary, so we capture this observation with a production: `KeyValue: Stringlit ':' Value;`. This library provides
a default definition for Stringlit, but we must provide our own definition for what is a Value. Looking back at the
sample we note that a Value can be a string literal, a number, or something enclosed in `{` `}` which we'll call
an Object. This leads to another production: `Value: Stringlit | Num | Object;`. We continue this process until
we've described all the structures in the sample. Below is the full grammar with comments describing each production.

<pre>
Program     :   Value;                  % the beginning of the program is a Value
Value       :   Stringlit               % a Value is a string literal, number, or Object
            |   Num
            |   Object;
Object      :   '{' KeyValue,* '}';     % an Object is comma-separated KeyValues surrounded by curly braces
KeyValue    :   Stringlit ':' Value;    % a KeyValue is a string literal and Value separated by a colon
</pre>

That's all that's needed to parse our JSON sample! This library contains 
[default definitions for common token classes](grammarsyntax.html#Lexer-rules)
like *Stringlit* and *Num* so you don't need to define them yourself, but you can always override them if you want. Grammar
definitions can either be placed into a standalone text file or embedded in an F# string literal, like so:
*)
open FSharp.Text.Experimental.Transform

[<Literal>]
let grammar = """
    Program     :   Value;                  % the beginning of the program is a Value
    Value       :   Stringlit               % a Value is a string literal, number, or Object
                |   Num
                |   Object;
    Object      :   '{' KeyValue,* '}';     % an Object is zero or more comma-separated KeyValues surrounded by curly braces
    KeyValue    :   Stringlit ':' Value;    % a KeyValue is a string literal and Value separated by a colon
"""

type Json = GrammarProvider<grammar>
(**
## Identify the nonterminal target

When writing a transformation it's important to think about the most basic grammar unit being transformed. Recall the general form
of our transformation is changing `"bar"` to `{ "value": "bar", "default": "" }`, representing a transformation from a 
Stringlit to an Object. **To find the appropriate nonterminal target for our transformation, we choose the nonterminal that 
is the nearest ancestor of both a Stringlit and an Object, i.e. a Value.** This means our transformation function will 
have the signature `Value -> Value`.

## Write the transformation function

As mentioned at the end of the previous step, our transformation function has the signature `Value -> Value`. Since the purpose of
our transformation is to add a default property to a string value, we will name our function "addDefault". Here is the template of
a typical transformation function, omitting parts that vary:
*)
let addDefault (inp: Json.(*nonterminal target goes here*) ) =
    inp.TryMatch< (*pattern to match goes here*) >()
    |> function 
        | Some b -> Json.(*nonterminal target*).Construct<(*replacement pattern*)>( (*replacement arguments*) )
        | None   -> inp
(**
And here is the template above filled in with our transformation details:
*)
let addDefault (inp: Json.Value) =
    inp.TryMatch<"stringValue$Stringlit">()     // target only Values that are string literals
    |> function
        | Some b -> Json.Value.Construct<""" { "value" : $Stringlit, "default" : "" } """>(b.StringValue)   // replace with an Object
        | None   -> inp
(**
This function first tries to match the input argument `inp` against the pattern `stringValue$Stringlit`. This means "try to
interpret `inp` (of type Value) as a Stringlit and, if successful, bind that Stringlit to the name `stringValue`." (For more
information about patterns, see [Patterns, Holes, and Binding Types](patterns.html)). It might be helpful to think of this as a 
downcast test, from a nonterminal higher in the grammar to a nonterminal lower in the grammar (similar to how, in object-oriented 
programming, one can downcast an Animal object to a Dog object).

If no match is found (i.e. the Value passed in is not a Stringlit) then we leave it unchanged; this is the None case. If we find a
match, then any variables that were bound in the process are available via the `b` variable. We therefore construct a new Value and
specify its form by the static parameter of the `Construct` method. This replacement pattern `{ "value" : $Stringlit, "default" : "" }`
contains `$Stringlit`, signifying a placeholder of type Stringlit. The `Construct` method's arguments provide values for such 
placeholders, in the same order they appear in the replacement pattern, therefore we provide `b.StringValue` which is the string literal
that was bound in the original pattern match.

## Write the main pipeline

This is where the main execution happens. We parse the input file, apply our transformation function to the resulting parse tree,
then finally unparse back into text and print it out:
*)  
Json.ParseString sample                     // parse sample
|> Json.Program.ApplyOnePass addDefault     // apply transformation function
|> Json.Pretty                              // unparse to string
|> printfn "%s"                             // print to stdout
(**
The `ApplyOnePass` method traverses the parse tree once, applying the `addDefault` transformation whenever appropriate. This
approach is similar to the `map` function familiar to all functional programmers, except that the transformation function 
(`addDefault`) is only applied to subtrees that match its target type (`Json.Value`). This library's API allows one to write
the same pipeline above but in a more object-oriented style:
*)
Json.ParseString(sample).ApplyOnePass(addDefault).ToString()
|> printfn "%s"
(**
You can use whichever style you prefer or even a mix of both. I prefer the first style because it's closer to idiomatic F#.

## Format the output

If we execute the code above we'll get this output:
```
{ "widget" : { "debug" : { "value" : "on", "default" : "" }, "window" : { "title" : { "value" : "Sample Konfabulator Widget", 
"default" : "" }, "name" : { "value" : "main_window", "default" : "" }, "width" : 500, "height" : 500 } } }
```
This output is correct but lacks formatting. We can annotate our grammar with formatting cues to pretty print the
output text. There are 4 [formatting cues](grammarsyntax.html#Formatting-cues) we can use in our grammar: 

*   indent `>`
*   dedent `<`
*   newline `/`
*   space `_`

In a pretty-printed JSON document each key-value pair starts on a new line -- so we annotate our KeyValue production with
a newline cue: 
<pre>KeyValue : / Stringlit ':' Value;</pre>
In addition, JSON text is indented after `{`, dedented before `}`, and a new line appears after a KeyValue list. We can use `>` and 
`<` for indent and dedent respectively, and use `/` after the KeyValue list:
<pre>Object : '{' > KeyValue,\* / < '}';</pre>
Here is the full grammar with these formatting cues:
```text
Program     :   Value;
Value       :   Stringlit
            |   Num
            |   Object;
Object      :   '{'
            >       KeyValue,*  /
            <   '}';
KeyValue    :   / Stringlit ':' Value;
```
With these formatting cues in place, the output text now appears properly:
```
{
    "widget" : {
        "debug" : {
            "value" : "on",
            "default" : ""
        },
        "window" : {
            "title" : {
                "value" : "Sample Konfabulator Widget",
                "default" : ""
            },
            "name" : {
                "value" : "main_window",
                "default" : ""
            },
            "width" : 500,
            "height" : 500
        }
    }
}
```

## Afterthoughts

This example demonstrates some of the advantages of tree-based transformation over other forms of text transformation.
For instance, using regular expressions to transform the JSON string literal value `"bar"` into the JSON object value
`{ "value" : "bar", "default" : "" }` could inadvertantly transform JSON keys named `"bar"` in the same way. But a
typed parse tree distinguishes between a key and a value, and this difference can be used to selectively target one or 
the other during the transformation.

This library also ensures that transformations will always produce well-formed output according to the given grammar.
In this JSON example, that means most attempts to produce malformed JSON will be compile errors in your program. This
is one of the core [design goals](philosophy.html) of this library.

We encourage you to read further about this library:

*   [API reference](api.html)
*   [Grammar syntax reference](grammarsyntax.html)
*   [How to use the parser](parser.html)
*   [Common transformation idioms](idioms.html)

*)