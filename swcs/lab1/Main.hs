import qualified Data.Text as T
import Prelude hiding (lex)

import Lexer
import Syntaxer

testCase :: String -> IO ()
testCase str = do
  let (lexemes, errs1) = lex $ T.pack str
      (expr, errs2) = syntax lexemes
      errs = errs1 ++ errs2
  putStrLn str
  print expr
  mapM_ (putStrLn . T.unpack) errs
  putStrLn ""

main :: IO ()
main = mapM_ testCase 
  [ "2.0 + 4"
  , "4x"
  , "x+4"
  , "(1+2)"
  , "()"
  , "5*(x+4*y)"
  , "**"
  , "1*2*3"
  , "(*)"
  , "(2*y)+z"
  ]
