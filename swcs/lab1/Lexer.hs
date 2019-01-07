module Lexer where

import Text.Printf (printf)
import Text.Read (readEither)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isDigit, isAlpha, isSpace)
import Data.Bifunctor (bimap)

import Automata

data Sum = Plus | Minus
  deriving (Eq)
data Prod = Mult | Divide
  deriving (Eq)
data Operator = Sum Sum | Prod Prod
  deriving (Eq)
data Lexeme = LOpenBr | LCloseBr | LConst Double | LVar Text | LOperator Operator
  deriving (Show, Eq)

data LexerState = LexerState 
  { _str :: Text
  , _lexemes :: [(Lexeme, Int)]
  , _pos :: Int
  , _errs :: [Text]
  }

instance Semigroup Sum where
  Plus <> x = x
  Minus <> Plus = Minus
  Minus <> Minus = Plus

instance Monoid Sum where
  mempty = Plus

class Inverse a where
  inv :: a -> a
  isStraight :: a -> Bool
  toOp :: a -> Operator

instance Inverse Sum where
  inv Plus = Minus
  inv Minus = Plus

  isStraight Plus = True
  isStraight Minus = False

  toOp = Sum

instance Inverse Prod where
  inv Mult = Divide
  inv Divide = Mult

  isStraight Mult = True
  isStraight Divide = False

  toOp = Prod

instance Inverse Operator where
  inv (Sum x) = Sum $ inv x
  inv (Prod x) = Prod $ inv x

  isStraight (Sum x) = isStraight x
  isStraight (Prod x) = isStraight x

  toOp = id

instance Show Sum where
  show Plus = "+"
  show Minus = "-"

instance Show Prod where
  show Mult = "*"
  show Divide = "/"

instance Show Operator where
  show (Sum x) = show x
  show (Prod x) = show x

instance Automata LexerState where
  nextState st@(LexerState str lexemes pos errs)
    | T.null str = st
    | isSpace ch = st'
    | ch == '(' = st' { _lexemes = putLexeme LOpenBr }
    | ch == ')' = st' { _lexemes = putLexeme LCloseBr }
    | ch == '+' = st' { _lexemes = putLexeme $ LOperator (Sum Plus) }
    | ch == '-' = st' { _lexemes = putLexeme $ LOperator (Sum Minus) }
    | ch == '*' = st' { _lexemes = putLexeme $ LOperator (Prod Mult) }
    | ch == '/' = st' { _lexemes = putLexeme $ LOperator (Prod Divide) }
    | isConst ch = extract isConst $ bimap (const $ printf "incorrect format number at position %d" pos) 
                                           LConst . readEither . T.unpack
    | isAlpha ch = extract isAlpha $ Right . LVar
    | otherwise = st' { _errs = T.pack (printf "Unknown lexem %c at position %d" ch pos) : errs }
    where ch = T.head str
          isConst c = isDigit c || c == '.'
          st' = st { _str = T.tail str, _pos = pos + 1 }
          putLexeme l = (l, pos) : lexemes
          extract p con = case con d of 
            Left err -> stA { _errs = T.pack err : errs }
            Right d' -> stA { _lexemes = (d', pos) : lexemes }
            where (d, str') = T.span p str
                  stA = st { _str = str'
                           , _pos = pos + (T.length d)
                           }

lex :: Text -> ([(Lexeme, Int)], [Text])
lex str = (reverse $ _lexemes st, reverse $ _errs st)
  where st = head $ dropWhile (not . T.null . _str) 
           $ iterate nextState $ LexerState str [] 1 []
