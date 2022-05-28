# scanner
A Scanner based on the book [Crafting Interpreters](https://craftinginterpreters.com) by Robert Nystrom

## Goal
Implement the [scanning](https://craftinginterpreters.com/scanning.html) portion of the interpreter described in Crafting Interpreters.
The job of the scanner is to perform [Lexical Analysis](https://en.wikipedia.org/wiki/Lexical_analysis),
which converts source code into [lexical tokens](https://en.wikipedia.org/wiki/Lexical_analysis#Token).
This is only the initial phase of the interpreter/compiler (I'm not sure if I'll implement the other parts).

## Current Status

The `scan-tokens` function can take a string of symbols and convert them to tokens, which are represented by maps:

```clojure
(scan-tokens "()*/!=*")
[{:token-type :left-paren, :lexeme "("}
 {:token-type :right-paren, :lexeme ")"}
 {:token-type :star, :lexeme "*"}
 {:token-type :bang-equal, :lexeme "!="}
 {:token-type :star, :lexeme "*"}
 {:token-type :eof, :lexeme ""}]
```
