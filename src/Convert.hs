-- Convert.hs
module Convert
    ( grammarToNFA
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Grammar
import Automaton

grammarToNFA :: Grammar -> GrammarType -> NFA
grammarToNFA (Grammar start grammar) gtype =
    case gtype of
        RightLinear -> buildRightLinear
        LeftLinear  -> buildLeftLinear
  where
    stateFromNT (NonTerminal c) = State [c]

    buildRightLinear =
        let startState = stateFromNT start
            finalState = State "#"
            ntStates = Set.map stateFromNT (Map.keysSet grammar)
            allStates = Set.insert finalState ntStates
            trans = Map.fromListWith Set.union $
                [ ((stateFromNT nt, Symbol t), Set.singleton finalState)
                | (nt, alts) <- Map.toList grammar
                , TerminalOnly (Terminal t) <- alts
                ] ++
                [ ((stateFromNT nt, Symbol t), Set.singleton (stateFromNT nt2))
                | (nt, alts) <- Map.toList grammar
                , RightLinearAlt (Terminal t) nt2 <- alts
                ]
        in NFA
            { nfaStates = allStates
            , nfaStart = startState
            , nfaAccept = Set.singleton finalState
            , nfaTransitions = trans
            }

    buildLeftLinear =
        let startState = State "@"
            ntStates = Set.map stateFromNT (Map.keysSet grammar)
            acceptStates = Set.singleton (stateFromNT start)
            allStates = Set.insert startState ntStates
            trans = Map.fromListWith Set.union $
                [ ((startState, Symbol t), Set.singleton (stateFromNT nt))
                | (nt, alts) <- Map.toList grammar
                , TerminalOnly (Terminal t) <- alts
                ] ++
                [ ((stateFromNT nt1, Symbol t), Set.singleton (stateFromNT nt2))
                | (nt2, alts) <- Map.toList grammar
                , LeftLinearAlt nt1 (Terminal t) <- alts
                ]
        in NFA
            { nfaStates = allStates
            , nfaStart = startState
            , nfaAccept = acceptStates
            , nfaTransitions = trans
            }
