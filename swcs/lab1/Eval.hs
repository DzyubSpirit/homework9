module Eval where

import Data.Text (Text)

import Lexer (Operator(..))
import Syntaxer (Expr(..), invOps)

data EvalExpr = EvConst Double | EvVar Text | EvOperator Operator EvalExpr EvalExpr
  deriving (Show)

evalExpr :: Expr -> EvalExpr
evalExpr EWrong = error "syntax expression is wrong"
evalExpr (EConst d) = EvConst d
evalExpr (EVar v) = EvVar v
evalExpr (ELevel lastExpr []) = evalExpr lastExpr
evalExpr (ELevel lastExpr oes) = EvOperator op le re
  where (roes, (op, lastExpr2):loes) = splitAt (length oes `div` 2) oes
        le = evalExpr (ELevel lastExpr2 loes)
        re = evalExpr
           $ (if op `elem` [Plus, Mult] then id else invOps) 
           $ ELevel lastExpr roes
