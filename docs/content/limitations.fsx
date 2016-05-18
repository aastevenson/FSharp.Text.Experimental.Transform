(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../src/FSharp.Text.Experimental.Transform/bin/Debug/FSharp.Text.Experimental.Transform.dll"

(**
Limitations
===========

The FSharp.Text.Experimental.Transform library contains several limitations. The sections below
describes these limitations, the reasons they exist, and possible workarounds.

1.  [Grammar disjunction is limited to 7 alternatives](#Grammar-disjunction-is-limited-to-7-alternatives)
2.  [Parsing whitespace-sensitive languages is not supported](#Parsing-whitespace-sensitive-languages-is-not-supported)
3.  [Left recursive grammar productions are not supported](#Left-recursive-grammar-productions-are-not-supported)

## Grammar disjunction is limited to 7 alternatives

Grammar disjunction refers to grammar clauses separated by `|`, and in this library are limited to
7 or less alternatives. An example of a grammar production with more than 7 alternatives is:
```text
Statement   :   DeclarationStatement
            |   AssignStatement
            |   ReadStatement
            |   PrintStatement
            |   FunctionDefinition
            |   IfStatement
            |   WhileStatement
            |   ForStatement;
```
**Reason:** Grammar disjunction is represented in the provided object model as the built-in
FSharp.Core.Choice family of types, which take at most 7 generic type parameters. The .NET
runtime supports an arbitrary number of type parameters, so hopefully this limit can be
raised in the future.

**Workaround:** If a disjunctive clause has more than 7 alternatives, they can be broken up into 
smaller clauses with 7 or less alternatives each. This can be done by identifying a subset of 
alternatives with a common characteristic and grouping them into a separate disjunctive clause.
For example, the production above can be broken up by grouping statements that control the flow
of execution:
```text
Statement               :   DeclarationStatement
                        |   AssignStatement
                        |   ReadStatement
                        |   PrintStatement
                        |   FunctionDefinition
                        |   ControlFlowStatement;

ControlFlowStatement    :   IfStatement
                        |   WhileStatement
                        |   ForStatement;
```

## Parsing whitespace-sensitive languages is not supported

The lexer in this library automatically discards all whitespace while tokenizing the input string,
making it impossible to parse languages that depend on significant whitespace or "offside lines"
such as F# or Python.

**Reason:** Simplicity. There is no external reason for this limitation, but such a lexer is more 
complex because it needs to emit indent and dedent tokens, keep track of indentation level with a
stack, etc. Support for this feature is planned for a future version.

**Workaround:** None.

## Left recursive grammar productions are not supported

Recursion in grammar productions occur when nonterminals are defined in terms of themselves, similar to
how function recursion occurs when functions are defined in terms of themselves. Left recursion refers
to the case where the self-reference occurs at the leftmost end of the nonterminal definition.
*Direct* left recursion occurs in a grammar when a nonterminal definition starts with that nonterminal itself,
for example:
```text
Expression  :   Expression Op Expression;
```
*Indirect* left recursion occurs when two or more nonterminal definitions reference each other first in a cycle:
```text
Statement   :   ... | Expression ';';
Expression  :   ... | Statement | ... ;
```
Note that left recursion also occurs whenever the self-referenced nonterminal is preceded by clauses
that can derive the empty string (including `*` or `?` clauses). For example:
```text
Expression  :   Modifier? Expression Op Expression;     % disallowed

Expression  :   Modifier* Expression Op Expression;     % disallowed

Expression  :   Modifier+ Expression Op Expression;     % allowed
Modifier    :   'const';

Expression  :   Modifier+ Expression Op Expression;     % disallowed
Modifier    :   'const'? ;
```

**Reason:** A top-down parser is used in this library, and left-recursion is a problem inherent in any
top-down parser. When the parser tries to parse with a left recursive goal it will recurse infintely
without consuming any input. It is possible to automatically refactor the grammar to eliminate 
left-recursion but this has the side-effect of altering the parse tree (and therefore the object model through 
which the library user navigates her parse tree). The object model must correspond to the grammar as written
for a suitable user experience. Automatic tree rewriting to restore the original parse tree is a possible
resolution, and is planned for a future version.

**Workaround:** There are many online articles describing how to 
[eliminate left recursion](https://www.google.ca/search?q=eliminate+left+recursion) in your grammar. As a basic
example:
```text
Expression  :   Primary | Expression Op Expression;     % with left recursion

Expression  :   Primary (Op Expression)?;               % without left recursion 
```

*)