{-|
The public parser entry point, wiring together the pipeline stages:

@
preprocess  →  alexScanTokens (lex)  →  relocate  →  modules (parse)
@

'parseFile' drives the whole flow; 'preprocess' (re-exported from
"Language.Verilog.Parser.Preprocess") is also exposed for callers that want to
expand directives without parsing.
-}
module Language.Verilog.Parser
  ( parseFile,
    preprocess,
  )
where

import Language.Verilog.AST
import Language.Verilog.Parser.Lex
import Language.Verilog.Parser.Parse
import Language.Verilog.Parser.Preprocess
import Language.Verilog.Parser.Tokens

-- | Parse Verilog source into a list of 'Module's. Given a table of predefined
-- macros, a file name (used only for error positions), and the file contents, it
-- preprocesses, lexes, stamps each token's 'Position' with the supplied file
-- name (the lexer does not know it), then runs the grammar. Parse and
-- preprocessing failures surface as 'error' calls.
parseFile :: [(String, String)] -> FilePath -> String -> [Module]
parseFile env file content = modules tokens
  where
    tokens = map relocate $ alexScanTokens $ preprocess env file content
    -- The lexer emits a blank file name; rewrite it to the real one for errors.
    relocate :: Token -> Token
    relocate (Token t s (Position _ l c)) = Token t s $ Position file l c
