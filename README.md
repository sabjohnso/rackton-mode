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
- Optional, dependency-free integration with Rackton's LSP server
  (eglot), debug server (dape), and signature-search tool. See
  *Tooling* below.

## Installation

```elisp
(add-to-list 'load-path "/path/to/rackton-mode")
(require 'rackton-mode)
(require 'rackton-repl)   ; optional: the inferior REPL
(require 'rackton-lsp)    ; optional: eglot/LSP integration
(require 'rackton-dap)    ; optional: dape/debug integration
(require 'rackton-search) ; optional: signature search
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
at point (cached; the cache empties whenever code is sent). When eglot
manages the buffer (see *Tooling* below), the REPL eldoc steps aside so
the LSP server's hover is the single source of type-at-point — and no
running REPL is needed for it.

The REPL integration is layered — transport (comint), a query channel
(`rackton-repl-query`), and UI commands — the seam through which the
*Tooling* integrations below take over static analysis from the REPL.

## Tooling

Rackton ships an LSP server, a DAP debug server, and a signature-search
tool. Each integration is **optional and lazy**: it activates only once
the relevant Emacs package is loaded, so it adds no hard dependency.

```elisp
(require 'rackton-lsp)     ; eglot ⇄ racket -l rackton/lsp
(require 'rackton-dap)     ; dape  ⇄ racket -l rackton/dap
(require 'rackton-search)  ; racket -l rackton/search
```

### LSP (eglot)

`require`-ing `rackton-lsp` registers the server with eglot. Run `M-x
eglot` in a `#lang rackton` buffer (or, to connect automatically, add
`(add-hook 'rackton-mode-hook #'eglot-ensure)`). You then get, from
Rackton's own analyzer: diagnostics via flymake, type hover via eldoc,
completion via completion-at-point, go-to-definition via xref, and
document symbols via imenu.

### Debugging (dape)

`require`-ing `rackton-dap` adds a `rackton` configuration to dape. `M-x
dape` offers it in Rackton buffers: breakpoints by source line,
stepping, stack frames, and locals under their source names. The DAP
server needs the gui-debugger collection at runtime (`raco pkg install
drracket`).

### Signature search

Hoogle-style search over the installed standard library, with **no
running REPL** required. Results open in `*rackton-search*` where each
`file:line` is a button that jumps to the definition.

| Command                  | Searches by                              |
|--------------------------|------------------------------------------|
| `rackton-search`         | a type pattern, e.g. `(-> (List a) Integer)` |
| `rackton-search-returns` | result type                              |
| `rackton-search-accepts` | accepted argument type                   |
| `rackton-search-name`    | name substring                           |

(The REPL's `C-c C-a` `rackton-accepts` answers the same question over
a *live session*, where it can also filter by which instances are in
scope; this shell search reads the static index and does not.)

## Development

Tests are written with ERT and run in batch:

```sh
make test
```
