{-|
Fixed-width unsigned bit vectors.

A 'BitVec' pairs a width (in bits) with a value, and every operation keeps the
value masked to that width. It backs the 'Language.Verilog.AST.Number'
constructor: the parser's literal decoder produces a 'BitVec' (see @toNumber@ in
the grammar) and the pretty-printer renders one as a sized hex literal.

The 'Num' and 'Bits' instances let host code compute on literals; binary
operators take the @max@ of the two widths. The 'Semigroup' instance is bit
concatenation (most-significant operand first), matching Verilog's @{a, b}@.
-}
module Data.BitVec
  ( BitVec,
    bitVec,
    select,
    width,
    value,
  )
where

import Data.Bits

-- | A bit vector: @BitVec width value@. The value is always masked to @width@
-- bits (see 'bitVec'); @width@ may be zero.
data BitVec = BitVec Int Integer deriving (Show, Eq)

-- | Arithmetic on bit vectors. Binary operators widen to the larger of the two
-- operand widths, then re-mask. 'fromInteger' infers the minimum width needed to
-- hold the magnitude (0 for @0@, 1 for @-1@, otherwise one bit per significant
-- binary digit).
instance Num BitVec where
  BitVec w1 v1 + BitVec w2 v2 = bitVec (max w1 w2) (v1 + v2)
  BitVec w1 v1 - BitVec w2 v2 = bitVec (max w1 w2) (v1 - v2)
  BitVec w1 v1 * BitVec w2 v2 = bitVec (max w1 w2) (v1 * v2)
  abs = id
  signum (BitVec _ v) = if v == 0 then bitVec 1 0 else bitVec 1 1
  fromInteger i = bitVec (width i) i
    where
      width :: Integer -> Int
      width a
        | a == 0 = 0
        | a == -1 = 1
        | otherwise = 1 + width (shiftR a 1)

-- | Bitwise operations. Like 'Num', binary operators widen to the larger width.
-- 'rotate' is unimplemented ('undefined').
instance Bits BitVec where
  BitVec w1 v1 .&. BitVec w2 v2 = bitVec (max w1 w2) (v1 .&. v2)
  BitVec w1 v1 .|. BitVec w2 v2 = bitVec (max w1 w2) (v1 .|. v2)
  BitVec w1 v1 `xor` BitVec w2 v2 = bitVec (max w1 w2) (v1 `xor` v2)
  complement (BitVec w v) = bitVec w $ complement v
  shift (BitVec w v) i = bitVec w $ shift v i
  rotate _ _ = undefined -- XXX  To lazy to implemented it now.
  bit i = fromInteger $ bit i
  testBit (BitVec _ v) = testBit v
  bitSize (BitVec w _) = w
  bitSizeMaybe (BitVec w _) = Just w
  isSigned _ = False
  popCount (BitVec _ v) = popCount v

-- | Concatenation: @a <> b@ places @a@ in the high bits and @b@ in the low bits
-- (@shiftL v1 w2 .|. v2@), giving a vector of width @w1 + w2@ — as Verilog's
-- @{a, b}@.
instance Semigroup BitVec where
  BitVec w1 v1 <> BitVec w2 v2 = BitVec (w1 + w2) (shiftL v1 w2 .|. v2)

-- | The identity for concatenation is the zero-width vector.
instance Monoid BitVec where
  mempty = BitVec 0 0

-- | Construct a 'BitVec' of the given width and value, masking the value to that
-- width. A negative width is clamped to zero.
bitVec :: Int -> Integer -> BitVec
bitVec w v = BitVec w' $ v .&. ((2 ^ fromIntegral w') - 1)
  where
    w' = max w 0

-- | Bit selection (part-select). @select v (msb, lsb)@ extracts bits @lsb@
-- through @msb@ inclusive, returning a vector of width @msb - lsb + 1@. The LSB
-- is index 0. The bounds are themselves 'BitVec's (their 'value's are used).
select :: BitVec -> (BitVec, BitVec) -> BitVec
select (BitVec _ v) (msb, lsb) = bitVec (fromIntegral $ value $ msb - lsb + 1) $ shiftR v (fromIntegral $ value $ lsb)

-- | Width of a 'BitVec'.
width :: BitVec -> Int
width (BitVec w _) = w

-- | Value of a 'BitVec'.
value :: BitVec -> Integer
value (BitVec _ v) = v
