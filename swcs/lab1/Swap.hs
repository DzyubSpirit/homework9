module Swap where

import qualified Data.Text as T
import Data.Bifunctor (second)
import Data.List (permutations)

import Lexer (Sum(..), Prod(..), Inverse(..))
import Syntaxer

swap :: Expr -> [Expr]
swap (EBrackets e) = swap e
swap (ELevel fstExpr oes) = either
    (\sums -> map (res Left) $ swapLevel $ (Plus, fstExpr):sums)
    (\prods -> map (res Right) $ swapLevel $ (Mult, fstExpr):prods)
    oes
  where res con ((_, fstExpr'):oes') = ELevel fstExpr' (con oes')
        res _ _ = error "unexpected empty list"
swap e = [e]

extract :: Int -> [a] -> (a, [a])
extract i xs = (x, bef ++ after)
  where (bef, x:after) = splitAt i xs

swapLevel :: Inverse op => [(op, Expr)] -> [[(op, Expr)]]
swapLevel oes = do
  oes' <- sequence $ map (sequence . second swap) oes
  let s = filter (isStraight . fst) oes'
      ns = filter (not . isStraight . fst) oes'
  (fstOp, s') <- [extract i s | i <- [0..length s-1]]
  rest <- permutations $ s' ++ ns
  return $ fstOp : rest

pretty :: Expr -> T.Text
pretty EWrong = T.pack "EWrong"
pretty (EVar name) = name
pretty (EConst ch) = T.pack $ show ch
pretty (ENegative e) = T.concat [T.pack "-", pretty $ EBrackets e]
pretty (EBrackets e) = T.concat [T.pack "(", pretty e, T.pack ")"]
pretty (ELevel fstExpr (Left sums)) = T.concat $ pretty fstExpr : map (\(op, e) -> T.concat [T.pack $ show op, pretty e]) sums
pretty (ELevel fstExpr (Right prods)) = T.concat $ pretty (wrap fstExpr) : map (\(op, e) -> T.concat [T.pack $ show op, pretty $ wrap e]) prods
  where wrap expr@(ELevel _ _) = EBrackets expr
        wrap expr = expr
