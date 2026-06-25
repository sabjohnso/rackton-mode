# rackton-mode

An Emacs major mode for [Rackton](../rackton), a statically-typed
functional language (in the style of Coalton) embedded in Racket.

## Features

- Automatic mode selection for files beginning with `#lang rackton`. A
  new, empty `.rkt` file opens in the `.rkt` default mode, then switches
  to `rackton-mode` as soon as you type its `#lang rackton` line.
- Font-lock for Rackton's surface forms (`data`, `protocol`,
  `instance`, `match`, `do`, the type/data family and constraint
  declarations, …), module import/export forms and their spec
  introducers, and `(: name type)` signatures.
- Types and data constructors are distinguished by position, not just
  capitalization: names in type positions (signatures, arrows,
  declaration heads, constructor fields, GADT clause tails,
  `#:deriving` lists, export specs) get `font-lock-type-face`, while
  constructors in expressions, patterns, and `data` bodies get
  `rackton-constructor-face` (inherits `font-lock-constant-face`;
  customize it via `M-x customize-face`).
- Infix operators — a backtick-quoted identifier in operator position,
  as in ``(a `+ b)`` or the sections ``(`< 3)`` and ``(3 `<)`` — read
  with `rackton-infix-operator-face` (inherits the function-name face).
- Indentation rules for Rackton's special forms, including the `do`
  style used throughout the rackton repository: binding clauses align
  under the first binding, the trailing expression sits at body indent.
- `C-c :` (`rackton-annotate-definition`) inserts or corrects the
  `(: name type)` signature above the `define` whose name is at point.
  See *Annotating definitions* below.
- `M-x rackton-enable-paredit-curly` makes paredit treat the `{..}` map
  and `#{..}` set braces structurally, like `()` and `[]` — an opt-in
  that binds `{`, `}`, and `M-{` in `paredit-mode-map`. See *Paredit
  and braces* below.
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

## Annotating definitions

With point on the name a `define` binds, `C-c :`
(`rackton-annotate-definition`) keeps a `(: name type)` signature just
above the `define`: it inserts one when absent, rewrites it when the
type disagrees, and leaves it untouched when it already agrees.

The type comes from whichever type source can answer first:

- **the LSP**, when eglot is connected — read from the server's hover,
  so no REPL and no loaded source file are needed; or
- **a running REPL** otherwise — its inferred type, provided one is
  already running (the command never starts a REPL on its own).

With neither connected, the command says so rather than guessing. The
source is an open list, `rackton-type-functions`, that each backend
registers into; the LSP is preferred when present.

## Paredit and braces

Rackton's map literals are `{..}` and its set literals `#{..}`, so
braces are balanced delimiters on a par with `()` and `[]`. The mode's
syntax table already says so, which is all paredit needs for
navigation and slurp/barf across braces. Paredit only declines to
*bind* the brace keys by default, leaving that to you.

`M-x rackton-enable-paredit-curly` supplies the binding: it puts `{`
and `}` on paredit's curly insert/close commands and `M-{` on
`paredit-wrap-curly`, mirroring the round and square keys. The keys go
in `paredit-mode-map`, so the change applies wherever paredit runs. It
is opt-in and never loaded for you — call it once from your init:

```elisp
(with-eval-after-load 'paredit
  (rackton-enable-paredit-curly))
```

The command requires paredit and reports plainly when it is not
installed.

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

Two more session-aware queries answer over the live session:
`rackton-repl-search` (`,search` — whole-signature search; a string
searches names) and `rackton-repl-returns` (`,returns` — by result
type). They share the `C-c C-f` ("find") prefix with the stdlib search
commands (see *Tooling*), as the `Control` variants:

| Key | Command | Effect |
|-----|---------|--------|
| `C-c C-f C-s` | `rackton-repl-search`  | session signature search (`,search`) |
| `C-c C-f C-r` | `rackton-repl-returns` | session search by result type (`,returns`) |

These see the session's own definitions, where their shell counterparts
read only the installed standard library.

`M-p` recalls earlier input, and `M-n` later input. At the very start of
the input they cycle the whole history (`comint-previous-input` /
`comint-next-input`); with text before point they move to the
previous/next input beginning with that text
(`comint-{previous,next}-matching-input-from-input`). The first press of
a run chooses the mode from the cursor position and the rest of the run
keeps it, so repeated presses — in either direction — cycle the same
set.

When the REPL is running, eldoc shows the inferred type of the symbol
at point (cached; the cache empties whenever code is sent). When eglot
manages the buffer (see *Tooling* below), the REPL eldoc steps aside so
the LSP server's hover is the single source of type-at-point — and no
running REPL is needed for it.

When the REPL prints an error carrying a source location
(`error: FILE:LINE:COL: …`), its first line is shown in the error face,
and either `RET` with point on that line or a mouse click on it jumps to
the location, the way `compilation-mode` does. The file is resolved
against the REPL's working directory. The detail below the first line is
syntax-highlighted: the `expected:` / `got:` lines are type information,
so every capitalized name there is a type, while the `in:` form is
Rackton code, where the usual type/constructor/keyword distinction
applies (so the same name can read as a type in the signature and a
constructor in a pattern). The labels themselves are emphasized.

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

### Completion

`TAB` indents the line, then completes the symbol at point (the
standard `tab-always-indent` set to `complete`; flip it back with the
`rackton-tab-always-indent` option). Candidates come from whichever
backend is live:

- **eglot**, when connected — the LSP server's completion (file
  definitions, imports, prelude, keywords).
- **the REPL** otherwise, when one is running (`rackton-repl`) — the
  session's own bindings plus keywords, via the REPL's `,complete`
  command. This also drives `TAB` in the `*rackton-repl*` buffer.

When eglot manages the buffer, the REPL backend steps aside (as the
eldoc type display does), so LSP completion is authoritative.

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

| Key | Command | Searches by |
|-----|---------|-------------|
| `C-c C-f s` | `rackton-search`         | a type pattern, e.g. `(-> (List a) Integer)` |
| `C-c C-f r` | `rackton-search-returns` | result type |
| `C-c C-f a` | `rackton-search-accepts` | accepted argument type |
| `C-c C-f n` | `rackton-search-name`    | name substring |

The `C-c C-f` ("find") prefix groups all signature search: a plain
letter searches the standard library (above); the `Control` variant
searches the live session (`C-c C-f C-s`, `C-c C-f C-r` — see *The
REPL*).

(The REPL's `C-c C-a` `rackton-accepts` answers the accepts question
over a *live session*, where it can also filter by which instances are
in scope; this shell search reads the static index and does not.)

## Development

Tests are written with ERT and run in batch:

```sh
make test
```
