{-|
The umbrella import for the @verilog@ library. Re-exports the AST
("Language.Verilog.AST") and the parser entry points
("Language.Verilog.Parser"), so downstream Pulsar tools (@drexpander@, @hsncl@)
simply @import Verilog@ (often qualified) to get the constructors they
pattern-match on and @parseFile@\/@preprocess@.

It also adds two conversion classes, used by those tools to lift host Haskell
values into the Verilog AST so they can be pretty-printed: 'ToVerilogModule' for
whole design units and 'ToVerilogExpression' for expressions.
-}
module Language.Verilog
  ( module Language.Verilog.AST
  , module Language.Verilog.Parser
  , ToVerilogModule(..)
  , ToVerilogExpression(..)
  ) where

import Language.Verilog.AST
import Language.Verilog.Parser

-- | Host types that can be rendered as one or more Verilog design units.
-- Define either method; the other is derived. 'toVerilogModule' on a value whose
-- list form is empty aborts with 'error'.
class ToVerilogModule a where
  {-# MINIMAL toVerilogModule | toVerilogModuleList #-}
  -- | Convert to a list of modules (e.g. a cell plus its primitives).
  toVerilogModuleList :: a -> [Module]
  toVerilogModuleList a = toVerilogModule a : []
  -- | Convert to a single module; defaults to the head of 'toVerilogModuleList'.
  toVerilogModule :: a -> Module
  toVerilogModule a = case toVerilogModuleList a of
    (m : _) -> m
    []      -> error "toVerilogModule: empty module list"

-- | Host types that can be rendered as a Verilog expression.
class ToVerilogExpression a where
  -- | Convert the value into an 'Expr'.
  toVerilogExpression :: a -> Expr
