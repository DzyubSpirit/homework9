module Syntaxer where

import Text.Printf (printf)
import qualified Data.Text as T
import Data.Text (Text)

import Lexer (Lexeme(..))

data Expr = Expr
  deriving (Show, Eq)

data SyntaxState = SyntaxState 
  { _lexemes :: [(Lexeme, Int)]
  , _stack :: [(Either Lexeme Expr, Int)]
  , _errs :: [Text]
  } deriving (Show)

syntax :: [(Lexeme, Int)] -> [Text]
syntax lexemes = _errs $ head $ dropWhile (not . null . _lexemes) $ iterate nextState
               $ SyntaxState { _lexemes = lexemes
                             , _stack = []
                             , _errs = []
                             }

nextState :: SyntaxState -> SyntaxState
nextState st@(SyntaxState [] _ _) = st
nextState st@(SyntaxState lexemes stack errs) = case lexeme of
  LOpenBr -> if null stack then st'' else case head stack of
    (Left (LOperator _), _) -> st''
    (_, bpos) -> st' { _errs = T.pack (printf "Expected operator before open bracket at position %d" bpos) : errs }
  LCloseBr -> case break ((== (Left LOpenBr)) . fst) stack of
    (_, []) -> st' { _errs = T.pack (printf "Close bracket without open bracket at position %d" pos) : errs }
    ([], _) -> st' { _errs = T.pack (printf "Close bracket right after open bracket at position %d" pos) : errs }
    (f:_, _:before) -> case fst f of
      Right _ -> st'NewStack
      Left (LOperator ch) -> st'NewStack { _errs = T.pack (printf "Operator '%c' right before close bracket at position %d" ch pos) : errs }
      Left _ -> st'NewStack
      where st'NewStack = st' { _stack = (Right Expr, pos) : before }
  LOperator ch -> case stack of
    [] -> st' { _errs = T.pack (printf "Unexpected operator %c at position %d" ch pos) : errs }
    (Left LOpenBr, _):_ -> st' { _errs = T.pack (printf "Unexpected operator %c at position %d" ch pos) : errs }
    (Left (LOperator _), _):_ -> st' { _errs = T.pack (printf "Unexpected operator %c at position %d" ch pos) : errs }
    _ -> st''
  _ -> if null stack then st'' else case fst (head stack) of 
    (Right _) -> st''
    Left LOpenBr -> st''
    Left (LOperator _) -> st''
    _ -> st'' { _errs = T.pack (printf "Unexpected expression at position %d" pos) : errs }
  where st' = st { _lexemes = tail lexemes }
        (lexeme, pos) = head lexemes
        st'' = st' { _stack = (Left lexeme, pos) : stack }
