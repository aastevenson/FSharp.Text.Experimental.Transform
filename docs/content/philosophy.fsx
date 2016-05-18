(*** hide ***)
#r "../../src/FSharp.Text.Experimental.Transform/bin/Debug/FSharp.Text.Experimental.Transform.dll"
type X = interface end
type Y = interface end

(**
Design Goals and Philosophy
===========================

All ideas are influenced by those that come before and this library is no exception. 
FSharp.Text.Experimental.Transform is heavily influenced by the source transformation language 
[TXL](txl.ca) and adopts much of its philosophy. F# is a great candidate for a reimplementation 
of TXL because they both share a functional paradigm, immutable values, and static typing. But 
*type providers* are the killer F# feature that puts it over the top for the requirements of 
this particular library.

The design goals of this library align with those of F# in many ways. In particular, parse trees 
in this library are both *immutable* and *statically-typed*. Immutability means transformations on 
parse trees do not mutate those parse trees, but rather creates new parse trees combining elements 
of existing parse trees such that common subtrees can be shared. Static typing refers to the idea
that each parse tree (and subtree) has a type known at compile-time, and more importantly has a 
*shape* consistent with that type. This property of strongly *shaped* parse trees is the key 
philosophy of this library, and we describe in the following section how this property is enforced
at compile-time.

## Strongly-shaped parse trees

When text is parsed according to a context-free grammar the resulting parse tree takes on a 
shape defined by the grammar. This library, which performs tree-based transformations, takes great 
care to ensure transformed trees maintain a shape that is still consistent with the grammar. This means
no matter how much you transform a JSON parse tree the result will be valid JSON, or no matter
how you transform a C# parse tree the result will be a valid C# program. Furthermore, a major design 
goal of this library is to enforce this constraint at compile-time making it impossible to execute 
library code producing malformed output.

These goals are achieved using F#'s type provider feature to map parse trees to types, then 
letting F#'s type system do the work of enforcing shape/type constraints. The following table shows
this mapping from grammar operator to F# type.

<table border="1" cellpadding="25">
<tr>
<th>Grammar Operator and Example</th><th>Corresponding Provided F# Type</th>
</tr>

<tr>
<td>
Concatenation
<pre>Concat: X Y</pre>
</td>
<td>
*)
type Concat(x : X, y : Y) =
    member __.X = x
    member __.Y = y
(**
</td>
</tr>

<tr>
<td>Disjunction
<pre>Disj: X | Y</pre></td>
<td>
*)
type Disj(choice : Choice<X, Y>) =
    member __.Chosen = choice
(**
</td>
</tr>

<tr>
<td>Optional
<pre>Opt: X?</pre></td>
<td>
*)
type Opt(maybeX : X option) =
    member __.Value = maybeX
(**
</td>
</tr>

<tr>
<td>Repetition
<pre>
Rep: X*
Rep: X+
Rep: X,*
Rep: X,+
</pre>
</td>
<td>
*)
type Rep(repetitions : X list) =
    member __.Repetitions = repetitions
(**
</td>
</tr>
</table>

One can see how, with this mapping, it is impossible to create a parse tree of type `Concat` with anything but
a subtree of type `X` and a subtree of type `Y`. F#'s built-in `Choice` type to represent disjunction is a 
natural fit, but leads to the unfortunate 
[limitation](limitations.html#Grammar-disjunction-is-limited-to-7-alternatives) of disallowing more than 7 
alternatives in a disjunctive clause.

The primary data structure used in this library is an abstract syntax tree, but trees are typically 
cumbersome to express and visualize in code compared with strings. For example, consider these two
expressions of the same parse tree: `AssignStatement(Id("sum"), Add(Id("a"), Const(5))` vs. `sum <- a + 5`.
The former has the advantage of being statically type-checked to ensure a valid tree shape, but the latter has 
the advantage of clarity. Luckily, type providers allow for the best of both. In particular, static parameters 
on provided methods allows for the extension of compile-time *shape* safety to 
[patterns and replacements](patterns.html), greatly reducing the possibility of bugs in pattern strings
compared to the alternative of evaluating pattern strings at runtime.

## Just do the right thing

Another design goal of this library is to **make common tasks the default behaviour** so it appears the library 
"just does the right thing". Examples include: 

*   Ignoring whitespace by default while parsing
*   Interpret alphabetic grammar literals as keywords that get parsed separately from identifiers.
*   Having common token classes already defined: identifiers (`Id`), integers (`Int`), string literals (`Stringlit`), etc.
*   Suppressing spaces around certain tokens during unparsing so output looks like `[1, 2, 3]` instead of 
    ` [ 1 , 2 , 3 ] `.

These are small, mostly invisible conveniences, but important for a good out-of-box user experience.

*)