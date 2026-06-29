-- | A parser for Verilog.
module Language.Verilog
  ( module Language.Verilog.AST
  , module Language.Verilog.Parser
  , ToVerilogModule(..)
  , ToVerilogExpression(..)
  ) where

import Language.Verilog.AST
import Language.Verilog.Parser

class ToVerilogModule a where
  {-# MINIMAL toVerilogModule | toVerilogModuleList #-}
  toVerilogModuleList :: a -> [Module]
  toVerilogModuleList a = toVerilogModule a : []
  toVerilogModule :: a -> Module
  toVerilogModule a = case toVerilogModuleList a of
    (m : _) -> m
    []      -> error "toVerilogModule: empty module list"

class ToVerilogExpression a where
  toVerilogExpression :: a -> Expr
