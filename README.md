# rackton-mode

An Emacs major mode for [Rackton](../rackton), a statically-typed
functional language (in the style of Coalton) embedded in Racket.

## Features

- Automatic mode selection for files beginning with `#lang rackton`.
- Font-lock for Rackton's surface forms (`data`, `class`, `instance`,
  `match`, `do`, …), module import/export forms and their spec
  introducers, and `(: name type)` signatures.
- Types and data constructors are distinguished by position, not just
  capitalization: names in type positions (signatures, arrows,
  declaration heads, constructor fields, GADT clause tails,
  `#:deriving` lists, export specs) get `font-lock-type-face`, while
  constructors in expressions, patterns, and `data` bodies get
  `rackton-constructor-face` (inherits `font-lock-constant-face`;
  customize it via `M-x customize-face`).
- Indentation rules for Rackton's special forms, including the `do`
  style used throughout the rackton repository: binding clauses align
  under the first binding, the trailing expression sits at body indent.
- Derives from the built-in `scheme-mode`; no third-party dependencies.

Indentation knowledge lives in a buffer-local table consulted by the
mode's own `lisp-indent-function`. Loading rackton-mode never changes
how Scheme buffers indent.

## Installation

```elisp
(add-to-list 'load-path "/path/to/rackton-mode")
(require 'rackton-mode)
```

Files whose first line is `#lang rackton` then open in `rackton-mode`
automatically.

## Development

Tests are written with ERT and run in batch:

```sh
make test
```
