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

- An inferior REPL (`rackton-repl.el`) wrapping `racket -l
  rackton/repl` with a SLIME-flavored command set.

## Installation

```elisp
(add-to-list 'load-path "/path/to/rackton-mode")
(require 'rackton-mode)
(require 'rackton-repl)   ; optional: the inferior REPL
```

Files whose first line is `#lang rackton` then open in `rackton-mode`
automatically.

## The REPL

`C-c C-z` (or `M-x rackton-repl`) starts `racket -l rackton/repl` in a
comint buffer. From any `rackton-mode` buffer:

| Key       | Command                   | Effect                                     |
|-----------|---------------------------|--------------------------------------------|
| `C-c C-z` | `rackton-repl`            | start / switch to the REPL                 |
| `C-x C-e` | `rackton-eval-last-sexp`  | send the expression before point           |
| `C-c C-c` | `rackton-eval-defun`      | send the top-level form around point       |
| `C-c C-r` | `rackton-send-region`     | send each top-level form in the region     |
| `C-c C-k` | `rackton-eval-buffer`     | send the whole buffer (skips `#lang` line) |
| `C-c C-t` | `rackton-type`            | show the inferred type (`,type`)           |
| `C-c C-d` | `rackton-describe-symbol` | describe a binding (`,info`)               |
| `C-c C-s` | `rackton-show-source`     | show the form that bound a name (`,source`)|
| `C-c C-a` | `rackton-accepts`         | search by accepted argument type (`,accepts`) |
| `C-c M-o` | `rackton-repl-clear-buffer` | clear the REPL display (keeps the session) |
| `C-c M-r` | `rackton-repl-reset`      | reset the session, discarding all definitions (`,clear`) |

When the REPL is running, eldoc shows the inferred type of the symbol
at point (cached; the cache empties whenever code is sent).

The REPL integration is layered — transport (comint), a query channel
(`rackton-repl-query`), and UI commands — so the Rackton LSP server,
debug server, and search service can each replace a backend when they
ship, without changing any command or keybinding.

## Development

Tests are written with ERT and run in batch:

```sh
make test
```
