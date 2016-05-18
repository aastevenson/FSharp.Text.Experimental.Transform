(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../src/FSharp.Text.Experimental.Transform/bin/Debug/FSharp.Text.Experimental.Transform.dll"

(**
FSharp.Text.Experimental.Transform
======================

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The FSharp.Text.Experimental.Transform library can be <a href="https://nuget.org/packages/FSharp.Text.Experimental.Transform">installed from NuGet</a>:
      <pre>PM> Install-Package FSharp.Text.Experimental.Transform</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

This library provides F# 4.0 developers with a powerful way to parse and transform structured text
documents. At a high level, the library requires three inputs: 

1.  A grammar
2.  A set of transformation functions (unnecessary for parsing only)
3.  A text file to process

The library uses the grammar to parse the text file into a syntax tree, then the transformation functions are
applied to this syntax tree yielding a new tree. Finally, this new tree is unparsed back into text representing the 
transformed document.

This library makes use of F#'s type provider feature. The GrammarProvider type provider treats the input
text file as data, and the grammar as a schema. Every nonterminal in the grammar becomes
a type emitted by the type provider.

Hello World Example
-------------------

Let's start with a simple Hello World example. The goal here is to transform the input string "welcome world" into "hello world".
We start by writing a grammar that describes our language, then parse the input string, transform the syntax tree, and finally
unparse the tree back into a string.
*)

open FSharp.Text.Experimental.Transform

// specify the grammar
[<Literal>]
let grammar = """
        Program     :   Greeting 'world';
        Greeting    :   'welcome' | 'hello';
    """ 

(** 
Grammars can be defined in a separate text file or as string literals in the F# program. Grammars in string literals
are typically enclosed in triple-quotes to make formatting easier. All grammars must contain a rule named 'Program', 
which represents the start symbol of the grammar. The 'Program' nonterminal above is defined as a greeting followed by
the literal `world`, and a greeting is defined as either `welcome` or `hello`.

With this grammar we can invoke the GrammarProvider type provider:
*)

// pass the grammar to the type provider
type HW = GrammarProvider<grammar>

(** 
Next, we define a function that takes the grammar element type we wish to change (Greeting), tries to match it against
a pattern, and specifies a replacement Greeting if the match is found.
*)

let welcomeToHello (inp: HW.Greeting) =
    match inp.TryMatch<"welcome">() with
    | Some _ -> HW.Greeting.Construct<"hello">()
    | None   -> inp

(** 
Finally, we parse our input string, apply the transformation function defined above, and pretty print the resulting
parse tree.
*)    
HW.ParseString "welcome world"                  // parse input string
|> HW.Program.ApplyOnePass welcomeToHello       // apply transformation function
|> HW.Pretty                                    // unparse to a string
|> printfn "%s"                                 // prints "hello world"

(**
This example may seem like a convoluted way to do a simple text replacement, however the power of tree-based transformations
becomes apparent in [a more complex example](tutorial.html).

Samples & documentation
-----------------------

The library comes with various documentation:

 * The [tutorial](tutorial.html) steps through a JSON transformation example, explaning the thought process at each step.
 * A discussion of [parsing](parser.html) with this library, including a comparison with FParsec.
 * An [API Reference](api.html) describing the various types and methods available through this type provider.
 * [Transformation idioms](idioms.html) for common transformation tasks.
 * How to write [grammars](grammarsyntax.html) for your custom text formats.
 * The [design goals and philosophy](philosophy.html) behind this library.
 * The current [limitations](limitations.html) of this library.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues] and fork 
the project. The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/FSharp.Text.Experimental.Transform/tree/master/docs/content
  [gh]: https://github.com/fsprojects/FSharp.Text.Experimental.Transform
  [issues]: https://github.com/fsprojects/FSharp.Text.Experimental.Transform/issues
  [readme]: https://github.com/fsprojects/FSharp.Text.Experimental.Transform/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharp.Text.Experimental.Transform/blob/master/LICENSE.txt
*)
