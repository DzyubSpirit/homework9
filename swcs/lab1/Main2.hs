import qualified Data.Text as T
import Prelude hiding (lex)
import Control.Monad (when)

import Lexer
import Syntaxer
import Eval

testCase :: String -> IO ()
testCase str = do
  let (lexemes, errs1) = lex $ T.pack str
      (expr, errs2) = syntax lexemes
      errs = errs1 ++ errs2
  putStrLn str
  when (null errs) $ mapM_ (putStrLn . T.unpack) $ pretty $ evalExpr expr
  mapM_ (putStrLn . T.unpack) errs
  putStrLn ""

main :: IO ()
main = mapM_ testCase 
  [ "1 * 2 * 3 * 4"
  , "5 + a/b/c/d+m*(r+p*2*x*y) + 6"
  , "a/b/c/d+m*(r+p*2*x*y)"
  , "1 * 2 * 3 * 4 * 5"
  , "1 * (2 / 3 * 4)"
  , "1 / 2 / 3 / 4 / 5"
  , "1 - 2 - 3 - 4 - 5 - 6"
  ]
