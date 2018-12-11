module Lexer where

import Text.Printf (printf)
import Text.Read (readEither)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isDigit, isAlpha, isSpace)

import Automata

data Lexeme = LOpenBr | LCloseBr | LConst Double | LVar Text | LOperator Char
  deriving (Show, Eq)

data LexerState = LexerState 
  { _str :: Text
  , _lexemes :: [(Lexeme, Int)]
  , _pos :: Int
  , _errs :: [Text]
  }

instance Automata LexerState where
  nextState st@(LexerState str lexemes pos errs)
    | T.null str = st
    | isSpace ch = st'
    | ch == '(' = st' { _lexemes = putLexeme LOpenBr }
    | ch == ')' = st' { _lexemes = putLexeme LCloseBr }
    | ch `elem` "*/+-" = st' { _lexemes = putLexeme $ LOperator ch }
    | isConst ch = extract isConst $ fmap LConst . readEither . T.unpack
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
