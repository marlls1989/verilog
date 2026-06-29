{-|
The abstract syntax tree for the supported (synthesisable) subset of Verilog,
together with the pretty-printer that renders it back to source text.

This module is the public contract of the @verilog@ library. The downstream
Pulsar tools — @drexpander@ (dual-rail expansion) and @hsncl@ (NCL standard-cell
library development) — both @import Verilog@ and pattern-match heavily on the
constructors defined here, so the shapes below are effectively an API: changing a
constructor's arity or order is a breaking change for those consumers.

Two design points are worth flagging up front, because they are unusual:

* __The 'Show' instances /are/ the pretty-printer.__ There is no separate
  rendering pass; @show@ on a 'Module', 'ModuleItem', 'Expr', 'Stmt', etc.
  produces Verilog source. The library's round-trip property (parse then @show@)
  relies on this, and the golden test in @test\/Spec.hs@ pins the exact text.

* __'Expr' has partial 'Num' and 'Bits' instances.__ They exist so that host
  Haskell code can build expression trees with ordinary numeric\/bitwise
  operators, but several methods are unimplemented and call 'error' at runtime
  (see the instances below). Treat them as construction conveniences, not as
  total numeric types.

The renderer for bit-vector literals is parameterised (see 'showExpr'):
'showBitVecDefault' emits a sized hex literal (e.g. @8'hff@) while
'showBitVecConst' emits a plain decimal, used in positions that must be a bare
constant (ranges, parameter values, instance parameter bindings).
-}
module Language.Verilog.AST
  ( -- * Identifiers and type synonyms
    Identifier,
    PortBinding,
    Case,
    Range,
    -- * Modules
    Module (..),
    ModuleItem (..),
    -- * Statements and expressions
    Stmt (..),
    LHS (..),
    Expr (..),
    UniOp (..),
    BinOp (..),
    Sense (..),
    Call (..),
    -- * Primitive (UDP) and timing bodies
    TableLine (..),
    SpecifyItem (..),
  )
where

import Data.BitVec (BitVec, value, width)
import Data.Bits
  ( Bits
      ( bit,
        bitSize,
        bitSizeMaybe,
        complement,
        isSigned,
        popCount,
        rotate,
        shift,
        testBit,
        xor,
        (.&.),
        (.|.)
      ),
  )
import Data.List (intercalate, intersperse)
import Data.Maybe (fromJust, isJust)
import Text.Printf (printf)

-- | A Verilog identifier (module, port, net, parameter, …). Held verbatim as a
-- 'String'; escaped (@\\…@) and system (@$…@) identifiers keep their leading
-- marker.
type Identifier = String

-- | A top-level design unit: a name, an ordered port list, and a body of
-- 'ModuleItem's.
--
-- The three constructors share the same shape but differ in how they parse and,
-- crucially, how they /render/:
--
-- * 'Module' — an ordinary @module … endmodule@. Parsed and printed as such.
-- * 'Primitive' — a Verilog user-defined primitive (UDP), @primitive …
--   endprimitive@. Carries a 'Table' body. Note the parser is write-only for
--   the table body (see @test\/Spec.hs@): @hsncl@ builds these and emits them,
--   but the grammar does not read a UDP table back.
-- * 'Cell' — a standard-cell module. There is no @cell@ keyword in Verilog; this
--   constructor exists only so the printer can wrap an otherwise-ordinary
--   @module@ in the @\`celldefine@ \/ @\`endcelldefine@ compiler directives.
--   Its 'Show' instance therefore emits @module … endmodule@ bracketed by those
--   directives, /not/ a @cell@ keyword.
data Module
  = Module Identifier [Identifier] [ModuleItem]    -- ^ @module name (ports); items endmodule@
  | Cell Identifier [Identifier] [ModuleItem]      -- ^ as 'Module' but wrapped in @\`celldefine@…@\`endcelldefine@ when shown
  | Primitive Identifier [Identifier] [ModuleItem] -- ^ @primitive name (ports); items endprimitive@ (UDP)
  deriving (Eq)

-- | Renders a design unit to Verilog. 'Cell' is wrapped in
-- @\`celldefine@\/@\`endcelldefine@; 'Module' and 'Primitive' use their keyword
-- directly. This instance is the pretty-printer, not a debug 'Show'.
instance Show Module where
  show a = case a of
    (Module name ports items) -> show' "module" name ports items
    (Primitive name ports items) -> show' "primitive" name ports items
    (Cell name ports items) -> "`celldefine\n" ++ show' "module" name ports items ++ "`endcelldefine\n"
    where
      show' kind name ports items =
        unlines
          [ kind ++ " " ++ name ++ (if null ports then "" else "(" ++ commas ports ++ ")") ++ ";",
            unlines' $ map show items,
            "end" ++ kind
          ]

-- | One row of a UDP @table@: the input field (one character per input,
-- including the symbolic @0@\/@1@\/@?@\/@-@ levels and edge notations), an
-- optional current-state character for sequential UDPs, and the output
-- character.
data TableLine = TableLine [Char] (Maybe Char) Char deriving (Eq, Ord)

-- | An item inside a @specify@ block. Only the module-path delay form is
-- modelled: @(src => dst) = (trise, tfall)@.
data SpecifyItem = PathDelay Identifier Identifier Float Float deriving (Eq, Ord)

-- | A statement at the body level of a module\/primitive. Fifteen forms cover
-- the declarations, structural items, and behavioural blocks the library
-- supports. A @Maybe Range@ field is the optional bus range (@Nothing@ = scalar).
data ModuleItem
  = Comment String                                      -- ^ a @\/\/@ line comment (synthesised by tools; not produced by the parser, which strips comments)
  | Parameter (Maybe Range) Identifier Expr             -- ^ @parameter [range] name = expr;@
  | Localparam (Maybe Range) Identifier Expr            -- ^ @localparam [range] name = expr;@
  | Input (Maybe Range) [Identifier]                    -- ^ @input  [range] a, b, …;@
  | Output (Maybe Range) [Identifier]                   -- ^ @output [range] a, b, …;@
  | Inout (Maybe Range) [Identifier]                    -- ^ @inout  [range] a, b, …;@
  | Wire (Maybe Range) [(Identifier, Maybe Expr)]       -- ^ @wire [range] n = expr, …;@ — each net optionally has a continuous-assign initialiser
  | Reg (Maybe Range) [(Identifier, Maybe Range)]       -- ^ @reg [range] r [memrange], …;@ — outer range is bit width, per-name range is a memory dimension
  | Integer [Identifier]                                -- ^ @integer i, j, …;@
  | Initial Stmt                                        -- ^ @initial@ block
  | Always (Maybe Sense) Stmt                           -- ^ @always@ (@Nothing@) or @always @(sense)@ block
  | Table [TableLine]                                   -- ^ a UDP @table … endtable@ body (write-only; see 'Primitive')
  | Assign LHS Expr                                     -- ^ continuous @assign lhs = expr;@
  | Instance Identifier [PortBinding] Identifier [PortBinding]
    -- ^ a module\/cell instance: @master \#(params) name (ports);@. First binding
    -- list is the parameter overrides, second is the port connections.
  | Specify [SpecifyItem]                               -- ^ a @specify … endspecify@ block
  deriving (Eq)

-- | A single connection in an instance binding list. @(Just name, expr)@ is a
-- named connection @.name(expr)@; @(Nothing, Just expr)@ is positional;
-- @(Nothing, Nothing)@ renders as @.*@ (implicit wildcard); a named binding with
-- @Nothing@ expr renders as @.name()@ (left unconnected).
type PortBinding = (Maybe Identifier, Maybe Expr)

-- | Renders a UDP table row: @i i i : o;@ for combinational, @i i i : s : o;@
-- for sequential (with current state @s@).
instance Show TableLine where
  show (TableLine inputs Nothing output) = printf "%s : %c;" (intersperse ' ' inputs) output
  show (TableLine inputs (Just state) output) = printf "%s : %c : %c;" (intersperse ' ' inputs) state output

-- | Renders a module-path delay: @(src => dst) = (trise, tfall);@.
instance Show SpecifyItem where
  show (PathDelay src dst trise tfall) = printf "(%s => %s) = (%g, %g);" src dst trise tfall

-- | Renders a body item to Verilog. Bus ranges and parameter values are printed
-- with 'showExprConst' (bare constants), while signal-valued expressions use the
-- default sized-hex renderer.
instance Show ModuleItem where
  show a = case a of
    Comment a -> "// " ++ a
    Parameter r n e -> printf "parameter %s%s = %s;" (showRange r) n (showExprConst e)
    Localparam r n e -> printf "localparam %s%s = %s;" (showRange r) n (showExprConst e)
    Input r a -> printf "input  %s%s;" (showRange r) (commas a)
    Output r a -> printf "output %s%s;" (showRange r) (commas a)
    Inout r a -> printf "inout  %s%s;" (showRange r) (commas a)
    Wire r a -> printf "wire   %s%s;" (showRange r) (commas [a ++ showAssign r | (a, r) <- a])
    Reg r a -> printf "reg    %s%s;" (showRange r) (commas [a ++ showRange r | (a, r) <- a])
    Integer a -> printf "integer %s;" $ commas a
    Initial a -> printf "initial\n%s" $ indent $ show a
    Always Nothing b -> printf "always\n%s" $ indent $ show b
    Always (Just a) b -> printf "always @(%s)\n%s" (show a) $ indent $ show b
    Assign a b -> printf "assign %s = %s;" (show a) (show b)
    Instance m params i ports
      | null params -> printf "%s %s %s;" m i (showPorts show ports)
      | otherwise -> printf "%s #%s %s %s;" m (showPorts showExprConst params) i (showPorts show ports)
    Table ls -> printf "table\n%s\nendtable" $ indent $ unlines' $ show <$> ls
    Specify ls -> printf "specify\n%s\nendspecify" $ indent $ unlines' $ show <$> ls
    where
      showPorts :: (Expr -> String) -> [(Maybe Identifier, Maybe Expr)] -> String
      showPorts s ports = printf "(%s)" $ commas [showPort i arg | (i, arg) <- ports]
        where
          showPort Nothing (Just arg) = printf "%s" $ s arg
          showPort Nothing Nothing = ".*"
          showPort (Just i) arg = printf ".%s(%s)" i $ if isJust arg then s (fromJust arg) else ""
      showAssign :: Maybe Expr -> String
      showAssign a = case a of
        Nothing -> ""
        Just a -> printf " = %s" $ show a

-- | Renders an optional bus range as @[hi:lo] @ (with a trailing space) or the
-- empty string for a scalar. Bounds are printed as bare constants.
showRange :: Maybe Range -> String
showRange Nothing = ""
showRange (Just (h, l)) = printf "[%s:%s] " (showExprConst h) (showExprConst l)

-- | Prefixes every line of the given text with a tab, for nesting block bodies.
indent :: String -> String
indent a = '\t' : f a
  where
    f [] = []
    f (a : rest)
      | a == '\n' = "\n\t" ++ f rest
      | otherwise = a : f rest

-- | Like 'unlines' but without a trailing newline (joins with @\\n@).
unlines' :: [String] -> String
unlines' = intercalate "\n"

-- | A Verilog expression. Thirteen forms covering literals, identifier
-- references (whole net, bit-select, part-select), concatenation\/replication,
-- system\/function calls, and the unary, binary, and ternary operators.
data Expr
  = String String              -- ^ a string literal @"…"@
  | Number BitVec              -- ^ a sized numeric literal; the 'BitVec' carries width and value
  | ConstBool Bool             -- ^ a one-bit constant, rendered @1'b1@ \/ @1'b0@
  | Ident Identifier           -- ^ a whole-net reference
  | IdentRange Identifier Range -- ^ a part-select @name[hi:lo]@
  | IdentBit Identifier Expr   -- ^ a bit-select @name[idx]@
  | Repeat Expr [Expr]         -- ^ a replication @{count {exprs}}@
  | Concat [Expr]              -- ^ a concatenation @{a, b, …}@
  | ExprCall Call              -- ^ a function\/system-task call used as an expression
  | UniOp UniOp Expr           -- ^ a unary operation
  | BinOp BinOp Expr Expr      -- ^ a binary operation
  | Mux Expr Expr Expr         -- ^ the ternary conditional @c ? t : f@
  | Bit Expr Int               -- ^ select bit @n@ of an expression, rendered @(expr [n])@ (an internal\/host construct, not standard parse output)
  deriving (Eq)

-- | Unary operators: logical not, bitwise not, unary plus, unary minus.
data UniOp = Not | BWNot | UAdd | USub deriving (Eq)

-- | Renders a unary operator to its Verilog symbol.
instance Show UniOp where
  show a = case a of
    Not -> "!"
    BWNot -> "~"
    UAdd -> "+"
    USub -> "-"

-- | Binary operators: logical (@&&@ @||@), bitwise (@&@ @^@ @|@), arithmetic
-- (@* \/ % + -@), shifts (@\<\< \>\>@), and comparisons (@== != \< \<= \> \>=@).
data BinOp
  = And      -- ^ @&&@
  | Or       -- ^ @||@
  | BWAnd    -- ^ @&@
  | BWXor    -- ^ @^@
  | BWOr     -- ^ @|@
  | Mul      -- ^ @*@
  | Div      -- ^ @\/@
  | Mod      -- ^ @%@
  | Add      -- ^ @+@
  | Sub      -- ^ @-@
  | ShiftL   -- ^ @\<\<@
  | ShiftR   -- ^ @\>\>@
  | Eq       -- ^ @==@
  | Ne       -- ^ @!=@
  | Lt       -- ^ @\<@
  | Le       -- ^ @\<=@
  | Gt       -- ^ @\>@
  | Ge       -- ^ @\>=@
  deriving (Eq)

-- | Renders a binary operator to its Verilog symbol.
instance Show BinOp where
  show a = case a of
    And -> "&&"
    Or -> "||"
    BWAnd -> "&"
    BWXor -> "^"
    BWOr -> "|"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    Add -> "+"
    Sub -> "-"
    ShiftL -> "<<"
    ShiftR -> ">>"
    Eq -> "=="
    Ne -> "!="
    Lt -> "<"
    Le -> "<="
    Gt -> ">"
    Ge -> ">="

-- | The default literal renderer: a sized hexadecimal literal, e.g. @8'hff@.
-- Used by the 'Show' instance for 'Expr'.
showBitVecDefault :: BitVec -> String
showBitVecDefault a = printf "%d'h%x" (width a) (value a)

-- | The constant-position literal renderer: a bare decimal value, dropping the
-- width. Used wherever Verilog requires a plain constant rather than a sized
-- literal (ranges, parameter values, instance parameter overrides).
showBitVecConst :: BitVec -> String
showBitVecConst a = show $ value a

-- | Renders an expression using the default sized-hex literal renderer.
instance Show Expr where show = showExpr showBitVecDefault

-- | Renders an expression in a constant position, using 'showBitVecConst' so
-- numeric literals appear as bare decimals.
showExprConst :: Expr -> String
showExprConst = showExpr showBitVecConst

-- | The expression pretty-printer, parameterised over how to render a 'BitVec'
-- literal (see 'showBitVecDefault' and 'showBitVecConst'). The chosen renderer
-- is threaded recursively through sub-expressions. Operators are fully
-- parenthesised so the printed form is unambiguous regardless of precedence.
showExpr :: (BitVec -> String) -> Expr -> String
showExpr bv a = case a of
  String a -> printf "\"%s\"" a
  Number a -> bv a
  ConstBool a -> printf "1'b%s" (if a then "1" else "0")
  Ident a -> a
  IdentBit a b -> printf "%s[%s]" a (showExprConst b)
  IdentRange a (b, c) -> printf "%s[%s:%s]" a (showExprConst b) (showExprConst c)
  Repeat a b -> printf "{%s {%s}}" (showExprConst a) (commas $ map s b)
  Concat a -> printf "{%s}" (commas $ map show a)
  ExprCall a -> show a
  UniOp a b -> printf "(%s %s)" (show a) (s b)
  BinOp a b c -> printf "(%s %s %s)" (s b) (show a) (s c)
  Mux a b c -> printf "(%s ? %s : %s)" (s a) (s b) (s c)
  Bit a b -> printf "(%s [%d])" (s a) b
  where
    s = showExpr bv

-- | A __partial__ 'Num' instance, provided so host code can build expression
-- trees with @+@, @-@, @*@, @negate@, and numeric literals. @abs@ and @signum@
-- are 'undefined' and will diverge\/throw if forced — a caller trap. @fromInteger@
-- produces a 'Number' whose width is inferred by 'Data.BitVec.fromInteger'.
instance Num Expr where
  (+) = BinOp Add
  (-) = BinOp Sub
  (*) = BinOp Mul
  negate = UniOp USub
  abs = undefined
  signum = undefined
  fromInteger = Number . fromInteger

-- | A __partial__ 'Bits' instance for building expression trees. Only the
-- bitwise combinators (@.&.@, @.|.@, @xor@, @complement@) and @isSigned@ are
-- meaningful. Every other method ('shift', 'rotate', 'bitSize',
-- 'bitSizeMaybe', 'testBit', 'bit', 'popCount') calls 'error' at runtime —
-- a caller trap: do not use 'Expr' where a total 'Bits' instance is expected.
instance Bits Expr where
  (.&.) = BinOp BWAnd
  (.|.) = BinOp BWOr
  xor = BinOp BWXor
  complement = UniOp BWNot
  isSigned _ = False
  shift = error "Not supported: shift"
  rotate = error "Not supported: rotate"
  bitSize = error "Not supported: bitSize"
  bitSizeMaybe = error "Not supported: bitSizeMaybe"
  testBit = error "Not supported: testBit"
  bit = error "Not supported: bit"
  popCount = error "Not supported: popCount"

-- | @(<>)@ builds a two-element 'Concat', so @<>@ on expressions is Verilog
-- concatenation.
instance Semigroup Expr where
  a <> b = Concat [a, b]

-- | 'mempty' is the numeric zero literal and 'mconcat' is n-ary 'Concat'.
instance Monoid Expr where
  mempty = 0
  mconcat = Concat

-- | The left-hand side of an assignment (procedural or continuous): a whole
-- net, a bit-select, a part-select, or a concatenation of such targets.
data LHS
  = LHS Identifier        -- ^ @name@
  | LHSBit Identifier Expr -- ^ @name[idx]@
  | LHSRange Identifier Range -- ^ @name[hi:lo]@
  | LHSConcat [LHS]       -- ^ @{a, b, …}@
  deriving (Eq)

-- | Renders an assignment target to Verilog.
instance Show LHS where
  show a = case a of
    LHS a -> a
    LHSBit a b -> printf "%s[%s]" a (showExprConst b)
    LHSRange a (b, c) -> printf "%s[%s:%s]" a (showExprConst b) (showExprConst c)
    LHSConcat a -> printf "{%s}" (commas $ map show a)

-- | A procedural statement, the body of an @initial@\/@always@ block. 'Null'
-- (an empty statement, @;@) doubles as the absent @else@ branch in 'If'.
data Stmt
  = Block (Maybe Identifier) [Stmt]                  -- ^ @begin … end@, optionally named @begin : label … end@
  | StmtReg (Maybe Range) [(Identifier, Maybe Range)] -- ^ a local @reg@ declaration
  | StmtInteger [Identifier]                          -- ^ a local @integer@ declaration
  | Case Expr [Case] (Maybe Stmt)                    -- ^ @case (e) … [default: …] endcase@
  | BlockingAssignment LHS Expr                      -- ^ @lhs = expr;@
  | NonBlockingAssignment LHS Expr                   -- ^ @lhs \<= expr;@
  | For (Identifier, Expr) Expr (Identifier, Expr) Stmt -- ^ @for (i = init; cond; i = step) body@
  | If Expr Stmt Stmt                                -- ^ @if (c) then else else@; a 'Null' else branch prints as a bare @if@
  | StmtCall Call                                    -- ^ a task\/system-task call statement
  | Delay Expr Stmt                                  -- ^ a delay control @#d stmt@
  | Null                                             -- ^ the empty statement @;@ (also the missing @else@)
  deriving (Eq)

-- | Joins strings with @", "@ — used throughout the printer for comma lists.
commas :: [String] -> String
commas = intercalate ", "

-- | Renders a procedural statement to Verilog, indenting nested blocks.
instance Show Stmt where
  show a = case a of
    Block Nothing b -> printf "begin\n%s\nend" $ indent $ unlines' $ map show b
    Block (Just a) b -> printf "begin : %s\n%s\nend" a $ indent $ unlines' $ map show b
    StmtReg a b -> printf "reg    %s%s;" (showRange a) (commas [a ++ showRange r | (a, r) <- b])
    StmtInteger a -> printf "integer %s;" $ commas a
    Case a b Nothing -> printf "case (%s)\n%s\nendcase" (show a) (indent $ unlines' $ map showCase b)
    Case a b (Just c) -> printf "case (%s)\n%s\n\tdefault:\n%s\nendcase" (show a) (indent $ unlines' $ map showCase b) (indent $ indent $ show c)
    BlockingAssignment a b -> printf "%s = %s;" (show a) (show b)
    NonBlockingAssignment a b -> printf "%s <= %s;" (show a) (show b)
    For (a, b) c (d, e) f -> printf "for (%s = %s; %s; %s = %s)\n%s" a (show b) (show c) d (show e) $ indent $ show f
    If a b Null -> printf "if (%s)\n%s" (show a) (indent $ show b)
    If a b c -> printf "if (%s)\n%s\nelse\n%s" (show a) (indent $ show b) (indent $ show c)
    StmtCall a -> printf "%s;" (show a)
    Delay a b -> printf "#%s %s" (showExprConst a) (show b)
    Null -> ";"

-- | One arm of a @case@: a list of match expressions (the labels) and the
-- statement to run. Multiple labels separated by commas share one statement.
type Case = ([Expr], Stmt)

-- | Renders a single @case@ arm: comma-separated labels, a colon, then the
-- indented body.
showCase :: Case -> String
showCase (a, b) = printf "%s:\n%s" (commas $ map show a) (indent $ show b)

-- | A function or system-task invocation: a name and its argument expressions.
data Call = Call Identifier [Expr] deriving (Eq)

-- | Renders a call as @name(arg, …)@.
instance Show Call where
  show (Call a b) = printf "%s(%s)" a (commas $ map show b)

-- | An event-control sensitivity expression for an @always @(…)@ block: a
-- level-sensitive signal, an edge (@posedge@\/@negedge@), or an @or@ of senses.
data Sense
  = Sense LHS          -- ^ a plain level-sensitive signal
  | SenseOr Sense Sense -- ^ @a or b@
  | SensePosedge LHS   -- ^ @posedge sig@
  | SenseNegedge LHS   -- ^ @negedge sig@
  deriving (Eq)

-- | Renders a sensitivity list.
instance Show Sense where
  show a = case a of
    Sense a -> show a
    SenseOr a b -> printf "%s or %s" (show a) (show b)
    SensePosedge a -> printf "posedge %s" (show a)
    SenseNegedge a -> printf "negedge %s" (show a)

-- | A bus\/part-select range as a @(msb, lsb)@ pair of expressions, i.e.
-- @[msb:lsb]@. Bounds are usually constant expressions.
type Range = (Expr, Expr)
