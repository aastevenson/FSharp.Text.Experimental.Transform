(*** hide ***)
#r "../../src/FSharp.Text.Experimental.Transform/bin/Debug/FSharp.Text.Experimental.Transform.dll"
open FSharp.Text.Experimental.Transform
[<Literal>]
let grammar = """
    Program             :   Statement*;
    Class               :   'class' Type ('extends' Type)? ('implements' Type,+)? '{'   /
                        >       ClassMember*
                        <   '}' /;
    ClassMember         :   Statement;
    Statement           :   InitializeStatement
                        |   ForStatement
                        |   ForeachStatement
                        |   WhileStatement;
    InitializeStatement :   Type Variable '=' Expr ';'  /;
    ForStatement        :   'for' '(' Statement Expr ';' Statement? ')' '{'    /
                        >       Statement*
                        <   '}' /;
    ForeachStatement    :   'for' '(' Type Variable ':' Expr ')' '{'    /
                        >       Statement*
                        <   '}' /;
    WhileStatement      :   'while' '(' Expr ')' '{'  /
                        >       Statement*
                        <   '}' /;
    Type                :   Id ('<' Type,+ '>')?;
    Variable            :   Id;
    Expr                :   Variable '.' Id '(' ')'
                        |   '(' Expr ')' '.' Id '(' ')';
"""
type Java = GrammarProvider<grammar>

(**
Transformation Idioms
=====================

Figuring out which transformation function to write can be one of the trickiest parts of using
this library. The table below outlines some idioms for common transformation tasks.

<table cellpadding="10">
<tr>
<th>Transformation Idiom</th><th>Function Signature</th><th>Purpose</th><th>Sample Use</th>
</tr>

<tr>
<td><a href="#Homomorphic-idiom">Homomorphic</a></td>
<td><tt>T -> T</tt></td>
<td>Replace a grammar element with another of the same type</td>
<td>Desugar a foreach loop into a regular for loop</td>
</tr>

<tr>
<td><a href="#One-to-many-idiom">One-to-many</a></td>
<td><tt>T -> T list</tt></td>
<td>Replace a grammar element with multiple elements of the same type</td>
<td>Change a for loop into an equivalent while loop</td>
</tr>

<tr>
<td><a href="#Remove-idiom">Remove</a></td>
<td style="white-space:nowrap"><tt>T list -> T list</tt></td>
<td>Remove all grammar elements of type <em>T</em> matching some pattern</td>
<td>Remove all declaration statements</td>
</tr>

<tr>
<td><a href="#Cross-language-idiom">Cross-language</a></td>
<td style="white-space:nowrap"><tt>T -> U</tt></td>
<td>Translate from one language to another language</td>
<td>Transpiling to Javascript, compiling to an intermediate language, data format exchange, etc.</td>
</tr>

</table>

## Homomorphic idiom

This is the simplest and most common kind of transformation function. The word *homomorphic* means "same shape",
indicating that the transformed output is of the same type as the input. Transformation functions of this kind
take a parse tree of type *T* as input and return a parse tree of type *T*. For example, a transformation
function can be written to rename any variable to "x"
*)
let variableToX (inp: Java.Variable) = Java.Variable.Construct<"x">()
(**
then this function can be applied to an entire program so all variables become "x":
*)
Java.ParseFile("myprog.java").ApplyOnePass(variableToX)
(**
The above transformation could be used in a blinded code clone analysis that tries to find similar code segments 
in a code base ignoring variable name differences.

A more complex example is desugaring a foreach loop into a regular for loop:
*)
let desugarForeach (inp: Java.Statement) =
    inp.TryMatch<"for (type$Type loopVar$Id : iterable$Expr) { body$Statement* }">()
    |> function
        | Some b -> 
            let assignLoopVar = Java.Statement.Construct<"$Type $Id = iter.next();">(b.Type, b.LoopVar)
            Java.Statement.Construct<"""
                for (Iterator iter = ($Expr).iterator(); iter.hasNext(); ) {
                    $Statement*
                }
            """>(b.Iterable, assignLoopVar::b.Body)
        | None -> inp
(**
This transformation might be helpful when writing a Java compiler.

## One-to-many idiom

A one-to-many transformation is used to replace a single element in a list by multiple
elements, and has the signature `T -> T list`. This function must be combined with F#'s 
`List.collect` function at the call site. Consider an example to transform a *for*
loop into its *while* loop equivalent:

<table>
<tr><th>Before</th><th>After</th></tr>
<tr>
<td>
```
for (int i = 0; i < arr.length; i++) {
    ...
}
```
</td>
<td>
```
int i = 0;
while (i < arr.length) {
    ...
    i++;
}
```
</td>
</tr>
</table>

A one-to-many idiom is appropriate here because one statement (the *for* loop) becomes two statements
(variable `i` initialization and the *while* loop). Below is the transformation function to achieve
this:
*)
let forLoopToWhileLoop (inp: Java.Statement) =
    inp.TryMatch<"for (initialize$Statement condition$Expr; increment$Statement) { body$Statement* }">()
    |> function
        | Some b ->
            [ b.Initialize
              Java.Statement.Construct<"while ($Expr) { $Statement* }">(b.Condition, b.Body @ [b.Increment]) ]
        | None -> [inp]
(**
The `Some b` branch returns a list with two statements: (1) the statement that 
initializes the loop variable, and (2) the `while` loop. The `None` branch must return a
list of statements as well, so it simply returns the original input statement. The above 
transformation function can be used with the `ApplyOnePass` or `ApplyRecursively`
methods by partially applying F#'s `List.collect` function:
*)
Java.ParseFile("myprog.java").ApplyRecursively(List.collect forLoopToWhileLoop)
(**
This command has the effect of transforming all *for* loops in the program into *while*
loops.

## Remove idiom

The idiom to remove a grammar element from a parse tree can be achieved with F#'s
`List.filter` function, which serves a similar purpose. For example, here is a transformation
function to remove all statements that declare/initialize an integer variable:
*)
let removeIntDeclarations (inp: Java.Statement list) =
    inp |> List.filter (fun stmt -> 
                    match stmt.TryMatch<"int $Variable = $Expr;">() with 
                    | Some _ -> false 
                    | None   -> true)
(**
This idiom only works on grammar elements with one of the four repeat modifiers
(`*`, `+`, `,*`, `,+`) because repeated elements can be removed [without breaking the shape
of the parse tree](philosophy.html#Strongly-shaped-parse-trees).

## Cross-language idiom

Translating between two different languages is arguably the most complex kind of transformation.
The first step is to identify a conceptual mapping between lexical structures in each language.
This step is easy if both languages are similar, but can be difficult if there are significant
conceptual differences between the languages. The idiom involves calling a chain of transformation
functions with signature `T -> U` where *T* is a nonterminal in the source grammar and *U* is a
nonterminal in the destination grammar. Each transformation function in this chain is responsible
for translating nonterminals at a different level in the grammar tree.

For example, consider translating between Java and C#, languages that are similar both
conceptually and syntactically. The topmost transformation function translates a Java compilation
unit to a C# compilation unit. That topmost function calls another transformation function that 
translates a Java class to a C# class, which itself calls another transformation function that 
translates Java methods to C# methods, etc. At each point in the call chain Java grammar elements
are used as inputs and C# grammar elements are used as outputs. 

Let's look at an example translating a Java class to a C# class. We start by considering the
syntactical differences between them:
<table>
<tr>
<th>Java class syntax</th><th>C# class syntax</th>
</tr>
<tr>
<td>
```text
Class   :   'class' Type ('extends' Type)? ('implements' Type,+)? '{'
                ClassMember*
            '}' ;
```
</td>
<td>
```text
Class   :   'class' Type (':' Type,+)? '{'
                ClassMember*
            '}' ;
```
</td>
</tr>
</table>

Java uses the keyword `extends` for inheritance and `implements` for interface implementation, 
whereas C# combines both concepts into a comma-separated list of types. Any time there is a 
syntax mismatch like this the transformation function will need to map the possible syntax 
cases in the source language to the corresponding syntax in the destination language. These 
different cases are shown in the `translateClass` function:
*)
// Helper active pattern that matches a class with 'implements' keyword
let (|ClassImplements|_|) (inp: Java.Class) =
    match inp.``('implements' Type,+)?`` with
    | Some implementsClause -> Some implementsClause.``Type,+``
    | None -> None

// Helper active pattern that matches a class with 'extends' keyword
let (|ClassExtends|_|) (inp: Java.Class) =
    match inp.``('extends' Type)?`` with
    | Some extendsClause -> Some extendsClause.Type
    | None -> None

let translateClass (inp: Java.Class) =
    match inp with
    // the Java class both extends a superclass and implements interfaces
    | ClassExtends superclass & ClassImplements interfaces -> 
        CSharp.Class.Construct<"class $Type : $Type,+ { $ClassMember* }">(
            inp.Type                |> translateType,
            superclass::interfaces  |> List.map translateType, 
            inp.``ClassMember*``    |> List.map translateMember )
    // the Java class only extends a superclass
    | ClassExtends superclass ->
        CSharp.Class.Construct<"class $Type : $Type { $ClassMember* }">(
            inp.Type                |> translateType,
            superclass              |> translateType ,
            inp.``ClassMember*``    |> List.map translateMember )
    // the Java class only implements interfaces
    | ClassImplements interfaces -> 
        CSharp.Class.Construct<"class $Type : $Type,+ { $ClassMember* }">(
            inp.Type                |> translateType,
            interfaces              |> List.map translateType, 
            inp.``ClassMember*``    |> List.map translateMember )
    // the Java class neither extends nor implements
    | _ ->
        CSharp.Class.Construct<"class $Type { $ClassMember* }">(
            inp.Type                |> translateType, 
            inp.``ClassMember*``    |> List.map translateMember )
(**
The use of active patterns above greatly simplifies and clarifies the transformation function
because of F#'s fantastic pattern matching capabilities. For instance, line 16 tries to match both 
active patterns simultaneously, indicating the presence of both a superclass and interfaces to
implement. Line 19 is where the superclass and implementing interfaces are combined 
(`superclass::interfaces`) into a single comma-separated list.

This `translateClass` function depends on other translation functions further down the grammar
tree, namely `translateType` and `translateMember`, responsible for translating Java types to C#
types and Java class members to C# class members respectively. Let's continue to follow the
`translateType` function to the end of the call chain.

The syntax for types references in both Java and C# are defined by an identifier followed by 
optional generic type parameters (e.g. `Map<Integer, List<String>>`, `IComparable<Temperature>`, etc.):
<pre>Type    :   Id ('<' Type,+ '>')? ; </pre>
Since this production is recursive, the `translateType` function must also be recursive:
*)
let rec translateType (inp: Java.Type) =
    match inp.``('<' Type,+ '>')?`` with
    | None            -> CSharp.Type.Construct<"$Id">(translateTypeId inp.Id)
    | Some typeParams -> CSharp.Type.Construct<"$Id<$Type,+>">(translateTypeId inp.Id, List.map translateType typeParams.``Type,+``)
(**
The `translateType` function above uses the provided `CSharp.Type.Construct<"pattern">()` method
to build C# parse trees from their subtrees in the case without type parameters (line 3) and with 
type parameters (line 4). Java identifiers (`Java.Id`) cannot be used as C# identifiers 
(`CSharp.Id`) so we use the `translateTypeId` function to perform the appropriate translation:
*)
let translateTypeId (inp: Java.Id) =
    match inp.ToString() with
    | "Integer"         -> CSharp.Id("int")
    | "String"          -> CSharp.Id("string")
    | "StringBuffer"    -> CSharp.Id("StringBuilder")
    | typeName          -> CSharp.Id(typeName)
(**
The `translateTypeId` function above is the end of the translation call chain. It is responsible
for mapping Java type names to C# type names, taking into account minor differences in primitive
type names, library types, etc. The last catch-all case translates type names without changes.

This completes the example. Translating Java to C# is relatively simple because they are conceptually
similar (object-oriented, imperative) and syntactically similar (both derive from the C-family of 
languages). However, translating between languages of differing paradigms or syntax is significantly
more challenging.
*)