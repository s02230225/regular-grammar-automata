-- Automaton.hs
module Automaton
    ( State(..)
    , Symbol(..)
    , NFA(..)
    , DFA(..)
    , Edge
    , nfaToEdges
    , dfaToEdges
    , edgesToString
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

newtype State = State String deriving (Eq, Ord)

instance Show State where
    show (State s) = s

newtype Symbol = Symbol Char deriving (Eq, Ord, Show)

data NFA = NFA
    { nfaStates      :: Set.Set State
    , nfaStart       :: State
    , nfaAccept      :: Set.Set State
    , nfaTransitions :: Map.Map (State, Symbol) (Set.Set State)
    } deriving (Show)

data DFA = DFA
    { dfaStates      :: Set.Set State
    , dfaStart       :: State
    , dfaAccept      :: Set.Set State
    , dfaTransitions :: Map.Map (State, Symbol) State
    } deriving (Show)

type Edge = (State, Symbol, State)

nfaToEdges :: NFA -> [Edge]
nfaToEdges nfa = 
    [ (from, sym, to)
    | ((from, sym), toSet) <- Map.toList (nfaTransitions nfa)
    , to <- Set.toList toSet
    ]

dfaToEdges :: DFA -> [Edge]
dfaToEdges dfa =
    [ (from, sym, to)
    | ((from, sym), to) <- Map.toList (dfaTransitions dfa)
    ]

edgesToString :: [Edge] -> String
edgesToString = unlines . map formatEdge
  where
    formatEdge (from, Symbol sym, to) = show from ++ " -> " ++ [sym] ++ " -> " ++ show to