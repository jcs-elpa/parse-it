[![Build Status](https://travis-ci.com/jcs090218/parse-it.svg?branch=master)](https://travis-ci.com/jcs090218/parse-it)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# parse-it
> Basic Parser in Emacs Lisp.

For most of the `major-mode` that needed a parser to act correctly. This is
a RegExp base parser. The goal of this project is to generate AST for
target programming language.


## Supported Langauges

* ActionScript - `actionscript`
* C - `c`
* C++ - `c++`
* C# - `csharp`
* HTML - `html`
* Java - `java`
* JavaScript - `js`
* Lua - `lua`
* Python - `python`
* TypeScript - `typescript`
* XML - `xml`


## Try it yourself!

You can simply test with any script like the code under.

```el
(require 'parse-it)
(parse-it-util--print-ast-tree (parse-it 'typescript "path/to/file.ts"))
```

All tokens are listed under programming language's file itself.


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
