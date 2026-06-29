module Main (main) where

import           Control.Exception          (SomeException, evaluate, try)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Language.Verilog
import           Test.Tasty
import           Test.Tasty.Golden          (goldenVsString)
import           Test.Tasty.HUnit

-- | Parse a Verilog source string with an empty preprocessor environment.
parse :: String -> [Module]
parse = parseFile [] "<test>"

-- | Re-render a list of modules back to Verilog text.
reshow :: [Module] -> String
reshow = concatMap show

main :: IO ()
main = defaultMain $ testGroup "verilog"
  [ structuralTests
  , roundtripTests
  , printerTests
  , limitationTests
  ]

--------------------------------------------------------------------------------
-- Structural unit tests: parse a snippet, assert on the resulting AST.
--------------------------------------------------------------------------------

moduleName :: Module -> String
moduleName (Module n _ _)    = n
moduleName (Cell n _ _)      = n
moduleName (Primitive n _ _) = n

modulePorts :: Module -> [Identifier]
modulePorts (Module _ ps _)    = ps
modulePorts (Cell _ ps _)      = ps
modulePorts (Primitive _ ps _) = ps

moduleItems :: Module -> [ModuleItem]
moduleItems (Module _ _ is)    = is
moduleItems (Cell _ _ is)      = is
moduleItems (Primitive _ _ is) = is

structuralTests :: TestTree
structuralTests = testGroup "structural"
  [ testCase "module name and ports" $ do
      let [m] = parse "module m (a, b, y); input a, b; output y; endmodule"
      moduleName m  @?= "m"
      modulePorts m @?= ["a", "b", "y"]

  , testCase "scalar input/output declarations" $ do
      let [m] = parse "module m (a, y); input a; output y; endmodule"
          ins  = [xs | Input  Nothing xs <- moduleItems m]
          outs = [xs | Output Nothing xs <- moduleItems m]
      concat ins  @?= ["a"]
      concat outs @?= ["y"]

  , testCase "bus range on input is parsed" $ do
      let [m] = parse "module m (d); input [3:0] d; endmodule"
          ranged = [() | Input (Just _) _ <- moduleItems m]
      length ranged @?= 1

  , testCase "instance with named port map" $ do
      let [m] = parse "module top (a, y); inv i0 (.a(a), .y(y)); endmodule"
          insts = [(c, nm) | Instance c _ nm _ <- moduleItems m]
      insts @?= [("inv", "i0")]

  , testCase "continuous assign" $ do
      let [m] = parse "module m (a, y); output y; assign y = a; endmodule"
          assigns = [() | Assign _ _ <- moduleItems m]
      length assigns @?= 1
  ]

--------------------------------------------------------------------------------
-- Roundtrip fixpoint: show . parse . show . parse == show . parse
-- Catches parser/printer drift on the supported (RTL) subset without pinning
-- exact formatting.
--------------------------------------------------------------------------------

roundtripFixpoint :: FilePath -> TestTree
roundtripFixpoint f = testCase f $ do
  s <- readFile f
  let out1 = reshow (parse s)
      out2 = reshow (parse out1)
  assertBool "parsed at least one module / non-empty render" (not (null out1))
  out2 @?= out1

roundtripTests :: TestTree
roundtripTests = testGroup "roundtrip fixpoint"
  [ roundtripFixpoint "test/fixtures/scalar.v"
  , roundtripFixpoint "test/fixtures/bus.v"
  , roundtripFixpoint "test/fixtures/inst.v"
  ]

--------------------------------------------------------------------------------
-- Printer golden: lock the Show output for a representative spread of AST
-- nodes. hsncl builds these nodes (UDP primitives, celldefine cells, specify
-- path-delays, tables, expressions) and emits them via `show`, so this golden
-- guards the exact text the rest of the ecosystem consumes.
-- Generate/update with:  stack test --test-arguments=--accept
--------------------------------------------------------------------------------

sampleItems :: [ModuleItem]
sampleItems =
  [ Input  (Just (7, 0)) ["d"]
  , Output Nothing        ["y"]
  , Wire   Nothing        [("w", Nothing)]
  , Assign (LHS "y") (BinOp BWAnd (Ident "a") (UniOp BWNot (Ident "b")))
  , Instance "inv" [] "i0" [(Just "a", Just (Ident "a")), (Just "y", Just (Ident "y"))]
  , Table  [ TableLine "00?" (Just '?') '0', TableLine "11" Nothing '1' ]
  , Specify [ PathDelay "A" "Q" 0.1 0.1 ]
  ]

sampleModules :: [Module]
sampleModules =
  [ Module "m" ["d", "y"]
      [ Input (Just (7, 0)) ["d"], Output Nothing ["y"] ]
  , Primitive "p_udp" ["Q", "A", "B"]
      [ Output Nothing ["Q"]
      , Input  Nothing ["A", "B"]
      , Reg    Nothing [("Q", Nothing)]
      , Table  [ TableLine "0?" (Just '?') '0', TableLine "11" (Just '?') '1' ]
      ]
  , Cell "the_cell" ["Q", "A"]
      [ Output Nothing ["Q"]
      , Input  Nothing ["A"]
      , Specify [ PathDelay "A" "Q" 0.1 0.1 ]
      ]
  ]

printerTests :: TestTree
printerTests = testGroup "printer golden"
  [ goldenVsString "ast-show" "test/golden/printer.golden" $
      pure $ BL.pack $ concatMap show sampleModules ++ concatMap show sampleItems
  ]

--------------------------------------------------------------------------------
-- Known limitations (characterisation): document the asymmetry that the parser
-- is write-only for UDP tables and the `timescale directive. If anyone makes
-- these parse, the test flips and prompts enabling a real roundtrip fixture.
--------------------------------------------------------------------------------

parseThrows :: String -> Assertion
parseThrows src = do
  r <- try (evaluate (length (reshow (parse src)))) :: IO (Either SomeException Int)
  case r of
    Left _  -> pure ()
    Right _ -> assertFailure "expected parse to fail, but it succeeded"

limitationTests :: TestTree
limitationTests = testGroup "known limitations"
  [ testCase "UDP table body is not parseable (parser is write-only for tables)" $
      parseThrows "primitive p (Q, A); output Q; input A; reg Q; table\n0 : ? : 0;\nendtable\nendprimitive"
  , testCase "`timescale directive is rejected by the preprocessor" $
      parseThrows "`timescale 1ns/10ps\nmodule m (a); input a; endmodule"
  ]
