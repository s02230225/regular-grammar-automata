module Main where

import Control.Monad (unless)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Grammar
import Automaton
import Convert
import Determinize

build :: String -> NFA
build input = case parseGrammar input of
    Left err -> error err
    Right grammar -> case getGrammarType grammar of
        Left err -> error err
        Right kind -> case validateGrammar grammar kind of
            Left err -> error err
            Right () -> grammarToNFA grammar kind

acceptNFA :: NFA -> String -> Bool
acceptNFA nfa word = not (Set.null (Set.intersection lastStates (nfaAccept nfa)))
  where
    lastStates = foldl step (Set.singleton (nfaStart nfa)) word
    step states c = Set.unions
        [Map.findWithDefault Set.empty (state, Symbol c) (nfaTransitions nfa)
        | state <- Set.toList states]

acceptDFA :: DFA -> String -> Bool
acceptDFA dfa word = maybe False (`Set.member` dfaAccept dfa) lastState
  where
    lastState = foldl step (Just (dfaStart dfa)) word
    step Nothing _ = Nothing
    step (Just state) c = Map.lookup (state, Symbol c) (dfaTransitions dfa)

wordsOfLength :: Int -> [String]
wordsOfLength 0 = [""]
wordsOfLength n = [c:rest | c <- "abc", rest <- wordsOfLength (n-1)]

checkLanguage :: String -> [String] -> IO ()
checkLanguage input accepted = do
    let nfa = build input
        dfa = nfaToDFA nfa
        wordsToCheck = concatMap wordsOfLength [0..4]
    mapM_ (\word -> unless (acceptNFA nfa word == (word `elem` accepted)
                         && acceptDFA dfa word == (word `elem` accepted))
                         (error ("Language mismatch: " ++ input ++ " / " ++ show word))) wordsToCheck

main :: IO ()
main = do
    checkLanguage "H=aA|aB;A=b;B=c" ["ab", "ac"]
    checkLanguage "Z=aA;A=b" ["ab"]
    checkLanguage "H=aA;H=aB;A=b;B=c" ["ab", "ac"]
    checkLanguage "S=Ab|Bc;A=a;B=b" ["ab", "bc"]
    checkLanguage "S=aS|b" ["b", "ab", "aab", "aaab"]
    mapM_ (\input -> case parseGrammar input of
        Left _ -> pure ()
        Right _ -> error ("Expected parse failure: " ++ input)) ["", ";;", "S=", "s=a"]
    let undefinedGrammar = parseGrammar "S=aA"
    case undefinedGrammar of
        Left err -> error err
        Right grammar -> case validateGrammar grammar RightLinear of
            Left _ -> pure ()
            Right () -> error "Expected undefined nonterminal rejection"
    putStrLn "Automata language tests passed"
