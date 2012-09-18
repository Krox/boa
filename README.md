boa
===

Boa is my selfmade language and this is the compiler.


the language
==
It's mostly similar to D (statically typed, templates, classes and all that), but with python syntax, i.e. indendation instead of '{' '}'.

[Here should be an example source snippet. For now, you can check the 'test' folder, which isn't comprehensive at all]


the compiler
==
It is written in D and based on LLVM with a handwritten lexer/parser. Currently it is JIT-compiled, but it should be trivial to add binary/assembly output if necessary.
Many things are working already, including classes, virtual methods and some templating.
However most parts are still incomplete and do not work in all cases. Even the parser is only about 95% done, much less the advanced features.

Feel free to contact me if you are into this kind of stuff. :)