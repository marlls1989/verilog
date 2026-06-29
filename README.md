# verilog

A small Haskell library for the **synthesisable subset of Verilog**: a
preprocessor, lexer, parser, abstract syntax tree, and pretty-printer. It is a
fork of Tom Hawkins' [`tomahawkins/verilog`](http://github.com/tomahawkins/verilog),
maintained as `marlls1989/verilog` by Marcos Sartori for use inside the **Pulsar**
ecosystem for QDI asynchronous-circuit design.

## What it does

The library is a thin pipeline from Verilog source text to an AST and back:

```
source text
  └─ preprocess        (strip comments; handle `define/`ifdef/`ifndef/`else/`endif)
       └─ lex          (Alex)   → [Token]
            └─ parse   (Happy)  → [Module]          -- the AST
                 └─ show         → Verilog source   -- the pretty-printer
```

The parser covers the RTL subset normally found in machine-generated,
synthesisable code: module/primitive declarations, port and net declarations,
continuous and procedural assignments, `always`/`initial` blocks, `case`/`if`/
`for`, instances with named or positional bindings, and the usual expression
operators. It is deliberately **not** a full SystemVerilog front end.

A notable design choice: **the `Show` instances *are* the pretty-printer.**
`show` on an AST node produces Verilog source, and the test suite pins both a
parse→print round-trip and a golden rendering of representative nodes.

## Module map

| Module | Role |
|--------|------|
| `Language.Verilog` | Umbrella import; re-exports the AST and parser, plus the `ToVerilogModule` / `ToVerilogExpression` host-conversion classes. |
| `Language.Verilog.AST` | The AST types and their `Show`-based pretty-printer. The public contract consumers pattern-match on. |
| `Language.Verilog.Parser` | `parseFile` (the full pipeline) and a re-export of `preprocess`. |
| `Language.Verilog.Parser.Preprocess` | Comment stripping (`uncomment`) and the `` `define ``-family directive pass (`preprocess`). |
| `Language.Verilog.Parser.Lex` | The Alex lexer (`*.x` source; edit `Lex.x`, not the generated `Lex.hs`). |
| `Language.Verilog.Parser.Parse` | The Happy grammar (`*.y` source; edit `Parse.y`, not the generated `Parse.hs`). |
| `Language.Verilog.Parser.Tokens` | The token vocabulary shared by lexer and grammar. |
| `Data.BitVec` | Fixed-width unsigned bit vectors, backing numeric literals. |

`Language.Verilog.Simulator` is **dead code** (not exposed, does not compile) and
is a candidate for removal.

## How Pulsar consumes it

Two downstream tools depend on this library and pattern-match heavily on its AST:

- **`drexpander`** — dual-rail expansion of NCL circuits.
- **`hsncl`** — NCL standard-cell library development; it *builds* AST nodes
  (UDP `primitive`s, `` `celldefine `` cells, `specify` path-delays, tables,
  expressions) and emits them via `show`.

Both typically `import Verilog` (often qualified) and rely on the exact shapes
and rendered text of the AST. Because of this, the constructor set in
`Language.Verilog.AST` is effectively an API: changing a constructor's arity or
order, or the printed output, is a breaking change for those consumers.

## Building

The package uses **Stack** (pinned LTS in `stack.yaml`) and a **hand-written
`verilog.cabal`** — there is no `package.yaml`/hpack. The Alex and Happy tools
generate `Lex.hs`/`Parse.hs` under `.stack-work` during the build.

```
stack build      # compile the library
stack test       # run the Tasty suite (structural, round-trip, golden, limitations)
stack haddock --fast --no-haddock-deps   # build the API docs
```

## Licence

BSD3 (see `LICENSE`). Original author Tom Hawkins; fork maintained by Marcos
Sartori.
