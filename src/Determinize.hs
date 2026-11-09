-- Determinize.hs
module Determinize
    ( isDeterministic
    , nfaToDFA
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl', intercalate, sort)
import Automaton

isDeterministic :: NFA -> Bool
isDeterministic nfa = all ((<= 1) . Set.size) (Map.elems $ nfaTransitions nfa)

nfaToDFA :: NFA -> DFA
nfaToDFA nfa =
    let startSet = Set.singleton (nfaStart nfa)
        startName = setName startSet

        (allSets, transitions) = buildDFA Set.empty [startSet] Map.empty

        acceptDFA = Set.fromList
            [ setName stateSet
            | stateSet <- Set.toList allSets
            , not (Set.null (Set.intersection stateSet (nfaAccept nfa)))
            ]
        statesDFA = Set.map setName allSets
    in DFA
        { dfaStates = statesDFA
        , dfaStart = startName
        , dfaAccept = acceptDFA
        , dfaTransitions = transitions
        }
  where
    alphabet :: [Symbol]
    alphabet = Set.toList (Set.map snd (Map.keysSet (nfaTransitions nfa)))

    move :: Set.Set State -> Symbol -> Set.Set State
    move states symbol =
        Set.unions
            [ Map.findWithDefault Set.empty (state, symbol) (nfaTransitions nfa)
            | state <- Set.toList states
            ]

    setName :: Set.Set State -> State
    setName stateSet =
        case Set.toList stateSet of
            [] -> State "{}"
            [single] -> single
            many ->
                let names = [name | State name <- many]
                    sortedNames = sort names
                in State ("{" ++ intercalate "," sortedNames ++ "}")

    buildDFA
        :: Set.Set (Set.Set State)
        -> [Set.Set State]
        -> Map.Map (State, Symbol) State
        -> (Set.Set (Set.Set State), Map.Map (State, Symbol) State)
    buildDFA processed [] trans = (processed, trans)
    buildDFA processed (current:queue) trans
        | current `Set.member` processed = buildDFA processed queue trans
        | otherwise =
            let fromState = setName current
                step acc symbol =
                    let nextSet = move current symbol
                    in case Set.null nextSet of
                        True -> acc
                        False -> (symbol, nextSet) : acc
                nonEmptyMoves = foldl' step [] alphabet
                insertTransition acc (symbol, nextSet) =
                    Map.insert (fromState, symbol) (setName nextSet) acc
                trans' = foldl' insertTransition trans nonEmptyMoves
                processed' = Set.insert current processed
                nextQueue = queue ++ [nextSet | (_, nextSet) <- reverse nonEmptyMoves]
            in buildDFA processed' nextQueue trans'
