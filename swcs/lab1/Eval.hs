module Eval where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Bifunctor (first)

import Lexer (Operator(..), Sum(..), Prod(..), Inverse(..), inv)
import Syntaxer (Expr(..))

import Prelude hiding (negate)

data EvalExpr = EvConst Double | EvVar Text | EvOperator Operator EvalExpr EvalExpr
  deriving (Show)

evalExpr :: Expr -> EvalExpr
evalExpr = evalExpr' . openBrackets

negate :: Expr -> Expr
negate EWrong = EWrong
negate (EBrackets e) = negate e
negate (ENegative e) = e
negate (EConst ch) = EConst (-ch)
negate (EVar var) = ELevel (EConst (-1)) $ Right [(Mult, EVar var)]
negate (ELevel fstExpr (Left sums)) = ELevel (EConst 0) $ Left $ (Minus, fstExpr) : map (first inv) sums
negate (ELevel fstExpr (Right prods)) = ELevel (EConst (-1)) $ Right $ (Mult, fstExpr) : prods

evalExpr' :: Expr -> EvalExpr
evalExpr' EWrong = error "syntax expression is wrong"
evalExpr' (ENegative e) = evalExpr' $ negate e
evalExpr' (EConst d) = EvConst d
evalExpr' (EVar v) = EvVar v
evalExpr' (EBrackets e) = evalExpr' e
evalExpr' (ELevel fstExpr (Left [])) = evalExpr' fstExpr
evalExpr' (ELevel fstExpr (Right [])) = evalExpr' fstExpr
evalExpr' (ELevel fstExpr oes) = either
  (\sums -> rec Sum $ splitLevel (\fe ss -> ELevel fe $ Left ss) fstExpr sums)
  (\prods -> rec Prod $ splitLevel (\fe ps -> ELevel fe $ Right ps) fstExpr prods)
  oes
  where rec con (le, op, re) = EvOperator (con op) (evalExpr' le) (evalExpr' re)
        

type OpExpr op = [(op, Expr)]
splitLevel :: Inverse op => (Expr -> OpExpr op -> Expr) -> Expr -> OpExpr op -> (Expr, op, Expr)
splitLevel _ fstExpr [(op, sndExpr)] = (fstExpr, op, sndExpr)
splitLevel con fstExpr oes = (le, op, re)
  where (loes, (op, fstExpr2):roes) = splitAt (length oes `div` 2) oes
        roes' = if isStraight op then roes else map (first inv) roes
        le = con fstExpr loes
        re = con fstExpr2 roes'

openBrackets :: Expr -> Expr
openBrackets (EBrackets e) = openBrackets e
openBrackets (ELevel fstExpr oes) = case oes of
  Left sums -> ELevel fstExpr' $ Left sums'
    where (_, fstExpr'):sums' = concatMap sumMerge $ (Plus, fstExpr):sums
  Right prods -> ELevel fstExpr' $ Right prods'
    where (_, fstExpr'):prods' = concatMap prodMerge $ (Mult, fstExpr):prods
openBrackets e = e

sumMerge :: (Sum, Expr) -> [(Sum, Expr)]
sumMerge (op, exprb) = case expr of
  (ELevel fstExpr (Left sums)) -> (op, fstExpr):sums'
    where sums' = if op == Plus then sums
                  else map (\(o, e) -> (inv o, e)) sums
  _ -> [(op, expr)]
  where expr = openBrackets exprb

tEmpty :: Int -> Text
tEmpty n = T.replicate n $ T.pack " "

pretty :: EvalExpr -> [Text]
pretty (EvConst ch) = [T.pack $ show ch]
pretty (EvVar name) = [name]
pretty (EvOperator op e1 e2) = (header :) $ take h $ zipWith (\t1 t2 -> T.concat [t1, T.pack" ", t2]) 
  (e1t ++ repeat (tEmpty w1)) 
  (e2t ++ repeat (tEmpty w2))
  where e1t = pretty e1
        e2t = pretty e2
        w1 = T.length $ head e1t
        h1 = length e1t
        w2 = T.length $ head e2t
        h2 = length e2t
        h = max h1 h2
        header = T.concat [T.replicate w1 (T.pack " "), T.pack $ show op, T.replicate w2 (T.pack " ")]

prodMerge :: (Prod, Expr) -> [(Prod, Expr)]
prodMerge (op, exprb) = case expr of
  (ELevel fstExpr (Right prods)) -> (op, fstExpr):prods'
    where prods' = if op == Mult then prods
                  else map (\(o, e) -> (inv o, e)) prods
  _ -> [(op, expr)]
  where expr = openBrackets exprb
