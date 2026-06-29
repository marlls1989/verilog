{-|
The Verilog source preprocessor: comment stripping and a minimal compiler-
directive (@\`define@ family) pass.

'preprocess' is the first stage of the parse pipeline (see
"Language.Verilog.Parser"): it runs before the lexer. It first strips comments
with 'uncomment', then walks the result line by line handling
@\`define@\/@\`ifdef@\/@\`ifndef@\/@\`else@\/@\`endif@ and expanding macro uses.
Only this small directive set is understood; anything else (e.g. @\`timescale@)
is left in place and will be rejected downstream — see the limitation test in
@test\/Spec.hs@.

Comment and directive lines are replaced by blank lines rather than removed, so
source line numbers are preserved for error reporting.
-}
module Language.Verilog.Parser.Preprocess
  ( uncomment
  , preprocess
  ) where

-- | Strip @\/\/@ and @\/* … *\/@ comments, replacing comment characters with
-- spaces (and keeping newlines\/tabs) so column and line positions are
-- preserved. A small state machine tracks whether it is in code, a line comment,
-- a block comment, or a string literal — string contents (which may contain
-- comment-like characters or escaped quotes) are left untouched. An unterminated
-- block comment or string aborts with 'error', naming the file.
uncomment :: FilePath -> String -> String
uncomment file a = uncomment a
  where
  uncomment a = case a of
    ""               -> ""
    '/' : '/' : rest -> "  " ++ removeEOL rest
    '/' : '*' : rest -> "  " ++ remove rest
    '"'       : rest -> '"' : ignoreString rest
    a         : rest -> a   : uncomment rest

  removeEOL a = case a of
    ""          -> ""
    '\n' : rest -> '\n' : uncomment rest 
    '\t' : rest -> '\t' : removeEOL rest
    _    : rest -> ' '  : removeEOL rest

  remove a = case a of
    ""               -> error $ "File ended without closing comment (*/): " ++ file
    '"' : rest       -> removeString rest
    '\n' : rest      -> '\n' : remove rest
    '\t' : rest      -> '\t' : remove rest
    '*' : '/' : rest -> "  " ++ uncomment rest
    _ : rest         -> " "  ++ remove rest

  removeString a = case a of
    ""                -> error $ "File ended without closing string: " ++ file
    '"' : rest        -> " "  ++ remove       rest
    '\\' : '"' : rest -> "  " ++ removeString rest
    '\n' : rest       -> '\n' :  removeString rest
    '\t' : rest       -> '\t' :  removeString rest
    _    : rest       -> ' '  :  removeString rest

  ignoreString a = case a of
    ""                -> error $ "File ended without closing string: " ++ file
    '"' : rest        -> '"' : uncomment rest
    '\\' : '"' : rest -> "\\\"" ++ ignoreString rest
    a : rest          -> a : ignoreString rest

-- | Apply the @\`define@-family directives after 'uncomment'ing the source.
--
-- The first argument seeds the macro environment (name\/replacement pairs), as
-- passed through from @parseFile@. The worker @pp@ threads two pieces of state:
-- @on@, whether the current line is inside an active conditional branch, and a
-- @stack@ of the enclosing branches' @on@ flags so @\`else@\/@\`endif@ can
-- restore the parent state. @\`ifdef@\/@\`ifndef@ test macro membership;
-- @\`define@ extends the environment (only when active). Active lines have their
-- macro uses expanded by 'ppLine'; inactive lines and directive lines become
-- blank. Unbalanced @\`else@\/@\`endif@ abort with 'error'.
preprocess :: [(String, String)] -> FilePath -> String -> String
preprocess env file content = unlines $ pp True [] env $ lines $ uncomment file content
  where
  pp :: Bool -> [Bool] -> [(String, String)] -> [String] -> [String]
  pp _ _ _ [] = []
  pp on stack env (a : rest) = case words a of
    "`define" : name : value -> "" : pp on stack (if on then (name, ppLine env $ unwords value) : env else env) rest
    "`ifdef"  : name : _     -> "" : pp (on && (elem    name $ fst $ unzip env)) (on : stack) env rest 
    "`ifndef" : name : _     -> "" : pp (on && (notElem name $ fst $ unzip env)) (on : stack) env rest 
    "`else" : _ -> case stack of
      (s : _)                -> "" : pp (s && not on) stack env rest
      []                     -> error $ "`else  without associated `ifdef/`ifndef: " ++ file
    "`endif" : _ -> case stack of
      (s : ss)               -> "" : pp s ss env rest
      []                     -> error $ "`endif  without associated `ifdef/`ifndef: " ++ file
    _                        -> (if on then ppLine env a else "") : pp on stack env rest

-- | Expand macro uses within a single line. A @\`@ followed by an identifier is
-- looked up in the environment and replaced by its value; the rest of the line is
-- scanned recursively. An unknown macro aborts with 'error'.
ppLine :: [(String, String)] -> String -> String
ppLine _ "" = ""
ppLine env ('`' : a) = case lookup name env of
  Just value -> value ++ ppLine env rest
  Nothing    -> error $ "Undefined macro: `" ++ name ++ "  Env: " ++ show env
  where
  name = takeWhile (flip elem $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']) a
  rest = drop (length name) a
ppLine env (a : b) = a : ppLine env b

