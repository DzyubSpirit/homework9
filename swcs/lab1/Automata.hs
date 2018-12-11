module Automata where

class Automata a where
  nextState :: a -> a
