module Syntaxer where

import Text.Printf (printf)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (when)
import Data.List (groupBy)
import Data.Bifunctor (first, second)

import Lexer (Lexeme(..), Operator(..), Sum(..), Prod(..))

type ESameOp = Either [(Sum, Expr)] [(Prod, Expr)]
data Expr = EWrong | EConst Double | EVar Text | EBrackets Expr
          | ENegative Expr | ELevel Expr ESameOp
  deriving (Show, Eq)

data SyntaxState = SyntaxState 
  { _lexemes :: [(Lexeme, Int)]
  , _stack :: [(Either Lexeme Expr, Int)]
  , _errs :: [Text]
  } deriving (Show)

{-
invOps :: Expr -> Expr
invOps (ELevel unOp fstExpr oes) = ELevel unOp' fstExpr 
  $ bimap invOe invOe oes
  where invOe :: Inverse a => [(a, Expr)] -> [(a, Expr)]
        invOe = map $ \(op, expr) -> (inv op, expr)
        unOp' = if isLeft oes then inv unOp else unOp
invOps expr = expr
-}

syntax :: [(Lexeme, Int)] -> (Expr, [Text])
syntax lexemes = either (\err -> (EWrong, (T.pack err):errs))
                        (\expr -> (expr, errs)) 
               $ exprList $ reverse stack
  where (SyntaxState _ stack errs) = head $ dropWhile (not . null . _lexemes) $ iterate nextState
                                   $ SyntaxState { _lexemes = lexemes
                                                 , _stack = []
                                                 , _errs = []
                                                 }

toExpr :: [(Either Lexeme Expr, Int)] -> Either String (Sum, Expr, [(Operator, Expr)])
toExpr [] = Left "empty expression"
toExpr ls@((unOp, unOpPos):xs) = do
  let isProd (Left (LOperator (Prod _))) = True
      isProd _ = False
  when (isProd unOp) $ Left $ printf "unexpected product operator at position %d" unOpPos
  let (ls', unOp') = case unOp of
        Left (LOperator (Sum sop)) -> (xs, sop)
        _ -> (ls, Plus)
  when (even $ length ls') $ Left $ printf "expected '(+|-)? expr [op expr]*'"
  (fstExpr:restExprs) <- mapM (\(l, pos) -> case l of
              Left (LConst d) -> Right $ EConst d
              Left (LVar v) -> Right $ EVar v
              Right expr -> Right expr
              _ -> Left $ printf "expected const, var or expression at position %d" pos
          ) $ [l | (i, l) <- zip [(0::Int)..] ls', even i]
  ops <- mapM (\(l, pos) -> case l of
              Left (LOperator op) -> Right op
              _ -> Left $ printf "expected operator at position %d" pos
          ) $ [l | (i, l) <- zip [(0::Int)..] ls', odd i]
  return (unOp', fstExpr, zip ops $ restExprs)

exprList :: [(Either Lexeme Expr, Int)] -> Either String Expr
exprList eles = do
  (unOp, fstExpr, oes) <- toExpr eles
  let isProd (Prod _) = True
      isProd _ = False
      grps = groupBy (\_ (op2, _) -> isProd op2) oes 
      toProd = Right . map (\(Prod p, expr) -> (p, expr))
      toSum = Left . map (\(Sum p, expr) -> (p, expr))
      fstOp = fst $ head $ head grps
      (fstExpr', grps') = 
          if isProd fstOp
          then (ELevel fstExpr $ toProd $ head grps, tail grps)
          else (fstExpr, grps)
      onlyProd = ELevel fstExpr
               $ if unOp == Plus then toProd $ head grps
                 else second ((Mult, EConst (-1.0)) :) $ toProd $ head grps
      sumProdGrps = toSum 
                  $ map (\((op1, expr1):xs) -> (op1, ELevel expr1 $ toProd xs))
                  $ grps'
      sumProd = if unOp == Plus then ELevel fstExpr' sumProdGrps
                else ELevel (EConst 0.0) $ first ((Minus, fstExpr') :) sumProdGrps
      result
        | length grps == 0 = if unOp == Plus then fstExpr else ENegative fstExpr
        | length grps == 1 && isProd fstOp = onlyProd
        | otherwise = sumProd
  return result

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
    (lxms, opb:before) -> case fst $ head lxms of
      Left (LOperator ch) -> (shrink EWrong) { _errs = T.pack (printf "Operator '%s' right before close bracket at position %d" (show ch) pos) : errs }
      _ -> either (\err -> (shrink EWrong) { _errs = T.pack (printf "Expression (%d-%d), err: %s" (snd $ opb) pos err) : errs })
                 (shrink . EBrackets) $ exprList $ reverse lxms
      where shrink expr = st' { _stack = (Right expr, pos) : before }
  LOperator op -> case stack of
    (Left (LOperator _), _):_ -> st' { _errs = T.pack (printf "Unexpected operator '%s' at position %d" (show op) pos) : errs }
    _ -> case op of
      Sum _ -> st''
      Prod _ -> case stack of
        [] -> st' { _errs = T.pack (printf "Unexpected operator '%s' at position %d" (show op) pos) : errs }
        (Left LOpenBr, _):_ -> st' { _errs = T.pack (printf "Unexpected operator '%s' at position %d" (show op) pos) : errs }
        _ ->st''
  _ -> if null stack then st'' else case fst (head stack) of 
    Left LOpenBr -> st''
    Left (LOperator _) -> st''
    _ -> st' { _errs = T.pack (printf "Unexpected expression at position %d" pos) : errs }
  where st' = st { _lexemes = tail lexemes }
        (lexeme, pos) = head lexemes
        st'' = st' { _stack = (Left lexeme, pos) : stack }
