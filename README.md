[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/parse-it-badge.svg)](https://melpa.org/#/parse-it)
[![MELPA Stable](https://stable.melpa.org/packages/parse-it-badge.svg)](https://stable.melpa.org/#/parse-it)

# parse-it
> Basic Parser in Emacs Lisp.

![CI](https://github.com/jcs-elpa/parse-it/workflows/CI/badge.svg)

For most of the `major-mode` that needed a parser to act correctly. This is
a regular expression base parser. The goal of this project is to generate AST for
targeting programming language.

## 🔨 Supported Langauges

* ActionScript - `actionscript`
* Assembly Language - `asm`
* C - `c`
* C++ - `c++`
* C# - `csharp`
* CSS - `css`
* Emacs Lisp - `elisp`
* Go - `go`
* HTML - `html`
* JSON - `json`
* Java - `java`
* JavaScript - `js`
* Kotlin - `kotlin`
* Lisp - `lisp`
* Lua - `lua`
* Markdown - `markdown`
* Objective-C - `objc`
* PHP - `php`
* Python - `python`
* R - `r`
* SQL - `sql`
* Swift - `swift`
* TypeScript - `typescript`
* XML - `xml`

## 🔰 Try it yourself!

You can simply test with any script like the code under.

```el
(require 'parse-it)
(parse-it-util--print-ast-tree (parse-it 'typescript "path/to/file.ts"))
```

All tokens are listed under programming language's file itself.

## 💫 How to write one parser/AST for your favorite language?

There are 4 files you need to know before you write your own parser/AST.
And these 4 files are listed under below.

<p align="center">
  <img alt="arch" src="./docs/arch.png" width="532" height="202">
</p>

1. `parse-it.el` - Entry.
2. `parse-it-lex.el` - Lexer functionalities.
3. `parse-it-ast.el` - Build AST after lexing.
4. `parse-it-uitl` - Other helpers.

Other file than these files are the implementation for specific programming
language due to their filename. For instance, `parse-it-c.el` is for programming
language `C`. If you are trying to implement c-like programming language you should check
out `parse-it-c.el` and reuse those identifier regular expression within that
file.

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
eask install-deps --dev
```

To test the package's installation:

```sh
eask package
eask install
```

To test compilation:

```sh
eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
eask lint checkdoc
```

The standard `package` linter:

```sh
eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
