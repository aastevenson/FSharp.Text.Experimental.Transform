(*** hide ***)
#r "../../src/FSharp.Text.Experimental.Transform/bin/Debug/FSharp.Text.Experimental.Transform.dll"

(**
Patterns, Holes, and Binding Types
==================================

Static parameters for provided methods, an F# language feature introduced in F# 4.0, is used
extensively in this library to express and enforce well-formed text patterns. Wellformedness
is determined by the input grammar, for example if the grammar contains the production **Val: Int | Stringlit**
then some well-formed values for *Val* are `38`, `"abc"`, `-3`, `""` (any integer or string literal), 
and some malformed values are `38.0`, `abc`, `3-` etc. (any values that are not integers or string
literals). When such values are supplied as static parameters to provided methods, the type provider can 
determine whether they are well-formed or not at *compile-time*. 

We will use the JSON grammar from [the tutorial](tutorial.html) to demonstrate the concept of
[patterns](#Patterns), [holes](#Holes), and [binding types](#Binding-types):
*)
open FSharp.Text.Experimental.Transform

[<Literal>]
let grammar = """
    Program     :   Value;
    Value       :   Stringlit
                |   Num
                |   Object;
    Object      :   '{' KeyValue,* '}';
    KeyValue    :   Stringlit ':' Value;
"""

type Json = GrammarProvider<grammar>

(**
## Patterns

We demonstrate the use of patterns using the [TryMatch method](api.html#TryMatch), typically used inside
a transformation function. For example, here is a function that tries to match a key-value pair
with key `"size"` and value `500`:
*)
let size500 (inp: Json.KeyValue) = inp.TryMatch<""" "size" : 500 """>()
(**
And here is a function that tries to match an Object with a several key-value pairs:
*)
let circleWithRadius10 (inp: Json.Object) = 
    inp.TryMatch<"""
        {
            "shape" : "circle",
            "radius" : 10 
        } 
    """>()
(**
The type provider checks these patterns against the given grammar nonterminal defintions and raises a
compile error if they cannot be parsed as the appropriate nonterminal (KeyValue or Object, respectively).

## Holes

The patterns shown in the previous section have every part of them specified concretely, but the real power of
patterns comes from the abliity to leave "holes" in them. Holes are specified by a name and type
separated by a dollar sign (`$`).

[**_name_**]<tt>$</tt>**_Nonterminal_**[**<tt>?</tt>**|**<tt>\*</tt>**|**<tt>+</tt>**|**<tt>,\*</tt>**|**<tt>,+</tt>**]

*   *name* is an identifier to which a subtree will be bound, similar to how the F# `let` keyword is
    used to bind a value to a name. Omitting *name* represents an anonymous hole, similar to an
    anonymous variable in F#.
*   *Nonterminal* is the name of a nonterminal in the grammar, representing (along with any modifiers) the hole type.
*   The modifier `?`, `*`, `+`, `,*`, or `,+` can be applied to *Nonterminal* to modify the hole type.

For example, consider the `size500` function above with a hole named "sizeValue" instead of the value `500`:
*)
let sizeNum (inp: Json.KeyValue) = inp.TryMatch<""" "size" : sizeValue$Num """>()
(**
The `sizeNum` function above will only match key-value pairs when the key is "size" and the value is some number,
but the hole type can be changed to match not just numbers but any JSON value:
*)
let sizeValue (inp: Json.KeyValue) = inp.TryMatch<""" "size" : sizeValue$Value """>()
(**
Below is a pattern that combines named holes and anonymous holes. It matches a JSON object with exactly 3 properties:

1.  A property named "topic" with a string literal value.
2.  A property named "subtopics" whose value is a JSON object with any number of properties.
3.  A property with unknown name but with value 500.
*)
let topic (inp: Json.Object) = 
    inp.TryMatch<"""
        {
            "topic"             : topic$Stringlit,
            "subtopics"         : { $KeyValue,* },
            thirdProp$Stringlit : 500
        } 
    """>()
(**
The above pattern has two named holes (*topic* and *thirdProp*) and one anonymous hole of type *KeyValue,\**.

It's possible for two or more holes to share the same name in a pattern. In this case, a successful match means
the first hole binds a subtree to the name, and the other holes with the same name must match that subtree.
This is used when two or more pattern elements should be the same. For example, we can match a key-value pair
where the key and the value are the same thing:
*)
let sameKeyValue (inp: Json.KeyValue) = inp.TryMatch<""" x$Stringlit : x$Stringlit """>()
(**
Holes with the same name must also have the same type.

There are 3 methods in the [API](api.html) for this library that use patterns with named holes as described above:
[TryMatch](api.html#TryMatch), [TryFind](api.html#TryFind), and [FindAllMatching](api.html#TryAllMatching). However
there is one other method, [Construct](api.html#Construct), that allows only anonymous holes in its pattern. These
anonymous holes mark places in the pattern that can be filled in by the method's arguments, similar to how F#'s
`printfn` function uses '%' markers in a string pattern to identify places that can be filled in by its arguments.

The process of matching a pattern and binding pattern subparts to names is known as 
[unification](https://en.wikipedia.org/wiki/Unification_(computer_science)), and should already be familiar to
F# programmers because it is used for
[F# pattern matching](https://msdn.microsoft.com/en-us/library/dd547125.aspx) and 
[F# type inference](https://msdn.microsoft.com/en-us/library/dd233180.aspx).

## Binding types

In the previous section we introduced *named holes* as a way to indicate pattern subtrees of interest. Binding
types capture the bindings between hole values and their names, allowing one to access the value of a pattern
hole match by its name. The type provider emits a binding type for each pattern, and each member of the binding
type corresponds to a named hole in that pattern. The `TryMatch()` method returns a value of type
*BindingType option*, either `Some(bindingType)` if the match is successful or `None` if it is not.

Here is the `topic` function from above but with the returned binding type (variable `b`) in use. Notice the
hole named *topic* can be accessed with `b.Topic` and the hole named *thirdProp* can be accessed with 
`b.ThirdProp`.
*)
let topic' (inp: Json.Object) = 
    let matchResult =
        inp.TryMatch<"""
            {
                "topic"             : topic$Stringlit,
                "subtopics"         : { $KeyValue,* },
                thirdProp$Stringlit : 500
            } 
        """>()
    match matchResult with
    | Some b -> 
        printfn "'topic' is bound to %A" b.Topic            // access hole named 'topic'
        printfn "'thirdProp' is bound to %A" b.ThirdProp    // access hole named 'thirdProp'
    | None -> printfn "No match found"
(**
A more typical use of binding type members is feeding them into the arguments of the [Construct](api.html#Construct) 
method. A transformation function will typically extract grammatical structures of interest with the
`TryMatch`, `TryFind`, or `FindAllMatching` methods, and then build a replacement by combining those 
structures with the `Construct` method. An example of this is apparent in the [tutorial](tutorial.html):
*)
let addDefault (inp: Json.Value) =
    inp.TryMatch<"stringValue$Stringlit">()     // target only Values that are string literals
    |> function
        | Some b -> Json.Value.Construct<""" { "value" : $Stringlit, "default" : "" } """>(b.StringValue)
        | None   -> inp