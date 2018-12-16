module Syntaxer where

import Text.Printf (printf)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.List (groupBy)

import Lexer (Lexeme(..), Operator(..))

data Expr = EWrong | EConst Double | EVar Text | ELevel Expr [(Operator, Expr)]
  deriving (Show, Eq)

data SyntaxState = SyntaxState 
  { _lexemes :: [(Lexeme, Int)]
  , _stack :: [(Either Lexeme Expr, Int)]
  , _errs :: [Text]
  } deriving (Show)

syntax :: [(Lexeme, Int)] -> (Expr, [Text])
syntax lexemes = (expr, errs)
  where expr = fromMaybe EWrong $ exprList $ map fst stack
        (SyntaxState _ stack errs) = head $ dropWhile (not . null . _lexemes) $ iterate nextState
                                   $ SyntaxState { _lexemes = lexemes
                                                 , _stack = []
                                                 , _errs = []
                                                 }

toExpr :: [(Either Lexeme Expr)] -> Maybe (Expr, [(Operator, Expr)])
toExpr ls = do
  when (even $length ls) Nothing
  exprs <- mapM (\l -> case l of
              Left (LConst d) -> Just $ EConst d
              Left (LVar v) -> Just $ EVar v
              Right expr -> Just expr
              _ -> Nothing
          ) $ [l | (i, l) <- zip [(0::Int)..] ls, even i]
  ops <- mapM (\l -> case l of
              Left (LOperator op) -> Just op
              _ -> Nothing
          ) $ [l | (i, l) <- zip [(0::Int)..] ls, odd i]
  return (head exprs, zip ops $ tail exprs)
  

exprList :: [(Either Lexeme Expr)] -> Maybe Expr
exprList eles = do
  (lastExpr, oes) <- toExpr eles
  return $ ELevel lastExpr
         $ map (\((op1, expr1):xs) -> (op1, ELevel expr1 xs))
         $ groupBy (\_ (op2, _) -> op2 `elem` [Mult, Divide]) oes 

eleToE :: Either Lexeme Expr -> Maybe Expr
eleToE = either lToE Just

lToE :: Lexeme -> Maybe Expr
lToE (LConst d) = Just $ EConst d
lToE (LVar t) = Just $ EVar t
lToE _ = Nothing

nextState :: SyntaxState -> SyntaxState
nextState st@(SyntaxState [] _ _) = st
nextState st@(SyntaxState lexemes stack errs) = case lexeme of
  LOpenBr -> if null stack then st'' else case head stack of
    (Left (LOperator _), _) -> st''
    (_, bpos) -> st' { _errs = T.pack (printf "Expected operator before open bracket at position %d" bpos) : errs }
  LCloseBr -> case break ((== (Left LOpenBr)) . fst) stack of
    (_, []) -> st' { _errs = T.pack (printf "Close bracket without open bracket at position %d" pos) : errs }
    ([], _) -> st' { _errs = T.pack (printf "Close bracket right after open bracket at position %d" pos) : errs }
    (lxms, _:before) -> case fst $ head lxms of
      Left (LOperator ch) -> (shrink EWrong) { _errs = T.pack (printf "Operator '%s' right before close bracket at position %d" (show ch) pos) : errs }
      _ -> maybe ((shrink EWrong) { _errs = T.pack (printf "Internal error, expected expr [op expr]*, lexemes: %s" (show lxms)) : errs })
                 shrink $ exprList $ map fst lxms
      where shrink expr = st' { _stack = (Right expr, pos) : before }
  LOperator ch -> case stack of
    [] -> st' { _errs = T.pack (printf "Unexpected operator '%s' at position %d" (show ch) pos) : errs }
    (Left LOpenBr, _):_ -> st' { _errs = T.pack (printf "Unexpected operator '%s' at position %d" (show ch) pos) : errs }
    (Left (LOperator _), _):_ -> st' { _errs = T.pack (printf "Unexpected operator '%s' at position %d" (show ch) pos) : errs }
    _ -> st''
  _ -> if null stack then st'' else case fst (head stack) of 
    (Right _) -> st''
    Left LOpenBr -> st''
    Left (LOperator _) -> st''
    _ -> st'' { _errs = T.pack (printf "Unexpected expression at position %d" pos) : errs }
  where st' = st { _lexemes = tail lexemes }
        (lexeme, pos) = head lexemes
        st'' = st' { _stack = (Left lexeme, pos) : stack }
