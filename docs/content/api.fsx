(**
API Reference
=============

This API reference is slightly different from a traditional API reference because
the methods signatures described within will change based on the grammar passed to the GrammarProvider 
type provider. The entry point to everything in this library is this type provider, 
which can be instantiated in the typical way:
```
type L = GrammarProvider<grammar>
```
Type `L`, referred to here as the *root type*, contains nested types that correspond to each 
nonterminal and token class in the grammar. For instance, the [Hello World Example](index.html) 
grammar will produce nested types `L.Program` and `L.Greeting`. These are called *nonterminal types* 
in this reference. Grammars that contain references to token classes such as 'Id' or 'Stringlit'
will result in the nested types `L.Id` and `L.Stringlit`, called *token class types*. The three sections 
below describe the methods for [root types](#Root-type-methods), [nonterminal types](#Nonterminal-type-methods),
and [token class types](#Token-class-type-methods) respectively.

## Root type methods

*   <pre>static ParseFile(<em>filePath</em>) : Program</pre>
    Parses the contents of a text file into a parse tree.
    *   Parameter *filePath* (string): The path of the input file to parse.
    *   Returns `L.Program`: The root of the parse tree.
*   <pre>static ParseString(<em>input</em>) : Program</pre>
    Parses the contents of a string into a parse tree.
    *   Parameter *input* (string): The input string to parse.
    *   Returns `L.Program`: The root of the parse tree.
*   <pre>static Pretty(<em>parseTree</em>) : string</pre>
    Converts a parse tree into a prettily-formatted string, determined by 
    [formatting cues](#grammarsyntax#Formatting-cues) in the grammar. An overload for this
    method exists for every nonterminal type in the grammar.
    *   Parameter *parseTree* (*Nonterminal Type*): The parse tree to convert.
    *   Returns string: The prettily-formatted output.

## Nonterminal type methods

The method signatures below include references to two placeholder types: *T* and *U*.
The placeholder type *T* refers to the nonterminal type of this class (for static methods)
or the type of this object (for instance methods). The placeholder type *U* refers to some other
nonterminal or token class type reachable from *T*. Overloaded methods are grouped below by name:

*   [ApplyOnePass](#ApplyOnePass): Traverse a parse tree once, applying a transformation function
*   [ApplyRecursively](#ApplyRecursively): Traverse a parse tree repeatedly, applying a 
    transformation function until no more replacements are possible.
*   [Construct](#Construct): Construct a new parse tree based on some pattern.
*   [Contains](#Contains): Determines if one tree is contained in another.
*   [FindAll](#FindAll): Find all subtrees of some type.
*   [FindAllMatching](#FindAllMatching): Find all subtrees matching some pattern.
*   [TryFind](#TryFind): Tries to find a subtree matching some pattern.
*   [TryMatch](#TryMatch): Tries to match the current tree with some pattern.

### ApplyOnePass

*   <pre><em>T</em>.ApplyOnePass(<em>transformer</em>) : <em>T</em></pre>
    Performs one pass over the parse tree of type *T*, applying the given transformation 
    function to subtrees of type *U*. Further transformations are not applied to subtree
    replacements, so this operation is guaranteed to finish.
    *   Parameter *transformer* ( *U* <tt>-></tt> *U* ): The transformation function to apply.
    *   Returns *T*: The transformed parse tree.

*   <pre><em>T</em>.ApplyOnePass(<em>transformer</em>) : <em>T</em></pre>
    Performs one pass over the parse tree of type *T*, applying the given transformation 
    function to subtrees of type *U* list. Further transformations are not applied to subtree
    replacements, so this operation is guaranteed to finish.
    *   Parameter *transformer* ( *U* list <tt>-></tt> *U* list): The transformation function to apply.
    *   Returns *T*: The transformed parse tree.

*   <pre>static ApplyOnePass(<em>transformer</em>) : <em>T</em> -> <em>T</em></pre>
    Performs one pass over the parse tree of type *T*, applying the given transformation 
    function to subtrees of type *U*. Further transformations are not applied to subtree
    replacements, so this operation is guaranteed to finish.
    *   Parameter *transformer* ( *U* <tt>-></tt> *U* ): The transformation function to apply.
    *   Returns *T* <tt>-></tt> *T*: A function that takes a tree and returns its transformed 
        tree. Useful when combined with the `|>` operator.

*   <pre>static ApplyOnePass(<em>transformer</em>) : <em>T</em> -> <em>T</em></pre>
    Performs one pass over the parse tree of type *T*, applying the given transformation 
    function to subtrees of type *U* list. Further transformations are not applied to subtree
    replacements, so this operation is guaranteed to finish.
    *   Parameter *transformer* ( *U* list <tt>-></tt> *U* list): The transformation function to apply.
    *   Returns *T* <tt>-></tt> *T*: A function that takes a tree and returns its transformed 
        tree. Useful when combined with the `|>` operator.

### ApplyRecursively

*   <pre><em>T</em>.ApplyRecursively(<em>transformer</em>) : <em>T</em></pre>
    Performs repeated passes over the parse tree of type *T*, applying the *transformer*
    function to subtrees of type *U*. *transformer* is recursively applied to subtree 
    replacements, so this operation is not guaranteed to finish.
    *   Parameter *transformer* ( *U* <tt>-></tt> *U* ): The transformation function to apply.
    *   Returns *T*: The transformed parse tree.

*   <pre><em>T</em>.ApplyRecursively(<em>transformer</em>) : <em>T</em></pre>
    Performs repeated passes over the parse tree of type *T*, applying the *transformer*
    function to subtrees of type *U* list. *transformer* is recursively applied to subtree 
    replacements, so this operation is not guaranteed to finish.
    *   Parameter *transformer* ( *U* list <tt>-></tt> *U* list): The transformation function to apply.
    *   Returns *T*: The transformed parse tree.

*   <pre>static ApplyRecursively(<em>transformer</em>) : <em>T</em> -> <em>T</em></pre>
    Performs repeated passes over the parse tree of type *T*, applying the *transformer*
    function to subtrees of type *U*. *transformer* is recursively applied to subtree 
    replacements, so this operation is not guaranteed to finish.
    *   Parameter *transformer* ( *U* <tt>-></tt> *U* ): The transformation function to apply.
    *   Returns *T* <tt>-></tt> *T*: A function that takes a tree and returns its transformed 
        tree. Useful when combined with the `|>` operator.

*   <pre>static ApplyRecursively(<em>transformer</em>) : <em>T</em> -> <em>T</em></pre>
    Performs repeated passes over the parse tree of type *T*, applying the *transformer*
    function to subtrees of type *U* list. *transformer* is recursively applied to subtree 
    replacements, so this operation is not guaranteed to finish.
    *   Parameter *transformer* ( *U* list <tt>-></tt> *U* list): The transformation function to apply.
    *   Returns *T* <tt>-></tt> *T*: A function that takes a tree and returns its transformed 
        tree. Useful when combined with the `|>` operator.

### Construct

*   <pre>static Construct< <em>replacement</em> >(<em>arg1</em>, <em>arg2</em>, ...) : <em>T</em></pre>
    Constructs a new parse tree of type *T* of the form *replacement*. The method argument values are used
    to fill holes in the *replacement*. See [patterns, holes, and binding types](patterns.html) for more information.
    *   Static parameter *replacement* (string): A replacement pattern of form *T*.
    *   Parameter *arg1*: A value of the type corresponding to the first hole in *replacement*.
    *   Parameter *arg2*: A value of the type corresponding to the second hole in *replacement*.
    *   Returns *T*: A parse tree of type *T* corresponding to the form of *replacement* with argument values
        applied to fill in the pattern holes.

### Contains

*   <pre><em>T</em>.Contains(<em>subtree</em>) : bool</pre>
    Tests whether this tree contains the given tree or not.
    *   Parameter *subtree* ( *U* ): The parse tree to test for containment.
    *   Returns bool: True if this tree contains *subtree*, false otherwise.

### FindAll

*   <pre><em>T</em>.FindAll<em>U</em>() : <em>U</em> list</pre>
    Finds all subtrees of type *U* within this tree. Returned subtrees are in depth-first order.
    *   Returns *U* list: All subtrees of type *U* within this tree.

*   <pre>static FindAll<em>U</em>() : <em>T</em> -> <em>U</em> list</pre>
    Finds all subtrees of type *U* within a tree. Returned subtrees are in depth-first order.
    *   Returns *T* <tt>-></tt> *U* list: A function that takes a tree and returns all subtrees 
        of type *U* within that tree. Useful when combined with the `|>` operator.

### FindAllMatching

*   <pre><em>T</em>.FindAll<em>U</em> Matching< <em>pattern</em> >() : <em>BindingType</em> list</pre>
    Finds all subtrees of type *U* within this tree matching *pattern*, and binds any pattern holes to
    *BindingType* members. See [patterns, holes, and binding types](patterns.html) for more information.
    *   Static parameter *pattern* (string): A pattern of form *U*.
    *   Returns *BindingType* list: All *BindingType*s corresponding to *U* matches within this tree.

### TryFind

*   <pre><em>T</em>.TryFind<em>U</em>< <em>pattern</em> >() : <em>BindingType</em> option</pre>
    Tries to find a subtree of type *U* within this tree matching *pattern* and, if found, binds any 
    pattern holes to *BindingType* members. See [patterns, holes, and binding types](patterns.html) for 
    more information.
    *   Static parameter *pattern* (string): A pattern of form *U*.
    *   Returns *BindingType* option: `Some(BindingType)` corresponding to the first *U* (in depth-first
        order) in this tree, or `None` if no subtrees of type *U* exist.

### TryMatch

*   <pre><em>T</em>.TryMatch< <em>pattern</em> >() : <em>BindingType</em> option</pre>
    Tries to unify this tree with the given *pattern* and, if successful, binds any 
    pattern holes to *BindingType* members. See [patterns, holes, and binding types](patterns.html) for 
    more information.
    *   Static parameter *pattern* (string): A pattern of form *T*.
    *   Returns *BindingType* option: `Some(BindingType)` if this tree matches *pattern*, or `None` 
        if it does not.

## Token class type methods

The only method common to all token class types is `ToString()`, but some default token classes have
special built-in methods. These methods are typically used to convert from a token class to an 
appropriate F# type.

*   The `ToInt()` method is available for the *Int*, *Nat*, and *NegInt* token classes.
    <pre>ToInt() : int</pre>
    Converts the text of this token to an integer.

*   The `ToFloat()` method is available for the *Num* and *Dec* token classes.
    <pre>ToFloat() : float</pre>
    Converts the text of this token to a floating-point number.

*   The `ToBool()` method is available for the *Bool* token class.
    <pre>ToBool() : bool</pre>
    Converts the text of this token (either `true` or `false`) to a boolean value.

*   The `Unquote()` method is available for the *Stringlit* and *Charlit* token classes.
    <pre>Unquote() : string</pre>
    Strips the surrounding quotes from the text of a string literal token, e.g. converts `"abc"` to
    `abc`.

*   The `Unique()` method is available for the *Id* token class.
    <pre>static Unique(<em>prefix</em>) : Id</pre>
    Generates a unique identifier with a given prefix. Successive calls to this method will generate
    a different identifier than any prior call in the same execution.
    *   Parameter *prefix* (string): A prefix for the generated identifier.
    *   Returns Id: A unique identifier.

*)