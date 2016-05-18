(*** hide ***)
#r "../../src/FSharp.Text.Experimental.Transform/bin/Debug/FSharp.Text.Experimental.Transform.dll"

(**
Grammar Syntax
==============

Like most parser-generator tools, this library uses a custom syntax to describe a context-free
grammar which is used to parse text files. The grammar syntax is a series of statements, of which
there are two kinds: lexer rules and parser rules (also known as productions). The lexer rules
define how consecutive characters get tokenized, and parser rules define how the stream of tokens
are parsed. This article covers the following syntax:

*   [Lexer rules](#Lexer-rules)
*   [Parser rules](#Parser-rules)
*   [Formatting cues](#Formatting-cues)
*   [Comments](#Comments)

## Lexer rules

Lexer rules have the following syntax:

[`ignore`] `terminal` *TokenClassName* `:` *QuotedRegexPattern* `;`

*   When present, the optional `ignore` keyword causes tokens of this token class to be 
    dropped from the token stream passed to the parser. This is typically used for comments
    and preprocessor directives.
*   The `terminal` keyword is required to distinguish this lexer rule from a parser rule. 
*   The *TokenClassName* should be an identifier starting with a letter (uppercase by convention) 
    followed by any combination of uppercase/lowercase letters or digits.
*   The *QuotedRegexPattern* is a double-quoted or single-quoted string literal that represents the
    regular expression for this token class. This regular expression is passed directly to the
    [System.Text.RegularExpressions.Regex](https://msdn.microsoft.com/en-us/library/system.text.regularexpressions.regex(v=vs.110).aspx) 
    class, so refer to its documentation for the appropriate regex syntax. Quote characters embedded
    in the regex string cannot be escaped, so if a double-quote (`"`) character is needed in the 
    regex pattern then surround the regex with single quotes, and likewise if a single-quote (`'`) is
    needed in the regex pattern then use double quotes for the string literal.

The following built-in lexer rules are available by default to be used in your parser rules. They can
be overridden by defining a lexer rule with the same *TokenClassName*. These default rules only take 
effect in the lexer if they are referenced in your grammar, otherwise they are discarded.

*   An identifier, used for naming something like a variable, a class, a function, etc.
```text
terminal Id : "[a-zA-Z][a-zA-Z0-9_]*";
```

*   A single-quoted string literal, used for a character literal or a string literal in some languages.
```text
terminal Charlit : "'[^'\\n]+'";
```

*   A double-quoted string literal, used for a string literal in most languages (a comment in SmallTalk).
    Supports embedded backslash-escaped double quote characters.
```text
terminal Stringlit : '"([^"]|\\")*"';
```

*   A number literal with a decimal point, used to represent floating point literals in most languages.
```text
terminal Dec : "-?[0-9]*.[0-9]+";
```

*   A natural number literal, used to represent any integer greater than or equal to zero.
```text
terminal Nat : "[0-9]+";
```

*   A negative integer literal, used to represent any integer less than zero.
```text
terminal NegInt : "-[0-9]+";
```

The order of lexer rules is significant because they are tried, in order of appearance, against the
remaining unconsumed input characters. It is wise to try to have a set of token classes that never
overlap (i.e. no two token classes contain the same string) so that order isn't an issue, but if this
isn't possible then the first matching token class will win.

## Parser rules

Parser rules have the following syntax:

*NonterminalName* `:` *Expression* `;`

*   The *NonterminalName* should be an identifier starting with a letter (uppercase by convention) 
    followed by any combination of uppercase/lowercase letters or digits.
*   The *Expression* can be one of:
    *   `'` *literal* `'`, representing a case-sensitive keyword or punctuation.
    *   `"` *literal* `"`, representing a case-insensitive keyword.
    *   *NonterminalName*, representing a reference to a grammar nonterminal or token class.
    *   *Expression* `|` *Expression* `|` ... `|` *Expression*, representing a set of alternative goals.
    *   *Expression* *Expression* ... *Expression*, representing a concatenation of goals.
    *   *Expression* `?`, representing an optional goal.
    *   *Expression* `*`, representing zero or more repetitions of a goal.
    *   *Expression* `+`, representing one or more repetitions of a goal.
    *   *Expression* `,*`, representing zero or more comma-separated repetitions of a goal.
    *   *Expression* `,+`, representing one or more comma-separated repetitions of a goal.
    *   `(` *Expression* `)`, representing an embedded subexpression.
    *   A [formatting cue](#Formatting-cues). (Must be part of a concatenation)

Quoted literals that are letters only are automatically interpreted as keywords and tokenized 
separately from identifiers, so e.g. the variable `letterSize` will never be tokenized as the keyword
`let` followed by the identifier `terSize`. It just [does the right thing by default](philosophy.html#Just-do-the-right-thing).
Quoted literals do not support escape characters, so use double quotes for `'` punctuation, and use
single quotes for `"` punctuation.

When writing a disjunctive goal (a production of the form `A : B | C | D`) the order of alternatives matters.
Each alternative will be attempted in order of appearance. If the current alternative fails, whether it has
consumed input or not, the parser will backtrack and the next alternative will be tried. The number of tokens
consumed by each alternative has no bearing on the parsing outcome.

In general the multiplicity modifiers are greedy. The optional modifier (`?`) will try to consume the
modified goal before accepting an empty result, and the repetition modifiers (`*`,`+`,`,*`,`,+`) will try to
consume as many instances of the modified goal as possible.

The following built-in parser rules are available by default to be used in your parser rules. They can
be overridden by defining a parser rule with the same *NonterminalName*. These default rules only take 
effect in the parser if they are referenced in your grammar, otherwise they are discarded.

*   An integer literal, used to represent any integer.
```text
Int : Nat | NegInt;
```

*   A number literal, used to represent any integer or decimal number.
```text
Num : Dec | Nat | NegInt;
```

*   A boolean literal, used to represent the keyword `true` or `false`.
```text
Bool : 'true' | 'false';
```

Every grammar must have a parser rule named 'Program' which is the starting rule for the parser.
See [the tutorial](tutorial.html) for a detailed example of how to construct a grammar and
apply formatting cues. The order of parser rules does not matter.

## Formatting cues

Formatting cues can be embedded in your grammar to control how the output text is pretty-printed.
There are 4 formatting cues, each represented by a character:

*   Indent `>`
*   Dedent `<`
*   Space `_`
*   Newline `/`

The most common uses of indent and dedent are around lexical scoping punctuation, for example
the following defines a Block as a pair of curly braces surrounding an indented set of statements:
```text
Block   :   '{' > Statement* < '}';
```
We recommend formatting such statements with cues to make the cue effect more apparent:
```text
Block   :   '{' 
        >       Statement* 
        <   '}';
```
The most common use of newline is after statements or anything that begins a new scope:
```text
Statement   :   'var' Id ';'            /
            |   Id '=' Expression ';'   / ;
```

Formatting cues are typically applied to the grammar after you see the output text and
notice it is not formatted quite right.

## Comments

A percent symbol (`%`) is used to signify a comment in the grammar language. Any text
following a percent symbol (`%`) until end-of-line is considered a comment and ignored
by the grammar language interpreter.
*)

