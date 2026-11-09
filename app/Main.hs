-- app/Main.hs
module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)

import Grammar
import Automaton
import Convert
import Determinize

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents
        (file:_) -> readFile file
    let trimmed = filter (/= '\n') input
    putStrLn "################################################################################"
    putStrLn $ "[1] Входная грамматика:"
    putStrLn $ "    \"" ++ trimmed ++ "\""

    case parseGrammar trimmed of
        Left err -> hPutStrLn stderr ("Ошибка разбора: " ++ err) >> exitFailure
        Right grammar ->
            case getGrammarType grammar of
                Left err -> hPutStrLn stderr ("Ошибка типа грамматики: " ++ err) >> exitFailure
                Right gtype ->
                    case validateGrammar grammar gtype of
                        Left err -> hPutStrLn stderr ("Ошибка валидации: " ++ err) >> exitFailure
                        Right () -> do
                            putStrLn ""
                            putStrLn "[2] Результат парсинга:"
                            putStrLn $ "    Тип: " ++ show gtype
                            let NonTerminal startSym = grammarStart grammar
                            putStrLn $ "    Начальный символ: " ++ [startSym]
                            putStrLn "    Продукции:"
                            mapM_ (putStrLn . ("        " ++)) (formatProductions grammar)

                            let nfa = grammarToNFA grammar gtype
                            putStrLn ""
                            putStrLn "[3] Недетерминированный конечный автомат (НКА):"
                            putStrLn $ "    Начальное состояние: " ++ show (nfaStart nfa)
                            putStrLn $ "    Финальные состояния: " ++ formatStateSet (nfaAccept nfa)
                            putStrLn "    Переходы:"
                            mapM_ (putStrLn . ("        " ++)) (formatNFATransitions nfa)

                            let det = isDeterministic nfa
                            putStrLn ""
                            putStrLn $ "[4] Проверка детерминированности: " ++ detMessage det

                            let dfa = nfaToDFA nfa
                            let resultEdges = dfaToEdges dfa

                            putStrLn ""
                            putStrLn "[5] Детерминированный конечный автомат (ДКА):"
                            putStrLn $ "    Начальное состояние: " ++ show (dfaStart dfa)
                            putStrLn $ "    Финальные состояния:"
                            mapM_ (putStrLn . ("        " ++)) (map show $ Set.toList $ dfaAccept dfa)
                            putStrLn "    Переходы:"
                            mapM_ (putStrLn . ("        " ++)) (formatDFATransitions dfa)

                            putStrLn ""
                            putStrLn "[6] Результат (список рёбер):"
                            putStr $ edgesToString resultEdges
                            putStrLn "################################################################################"

detMessage :: Bool -> String
detMessage True  = "ДА"
detMessage False = "НЕТ (требуется детерминизация)"

formatProductions :: Grammar -> [String]
formatProductions (Grammar _ grammar) =   -- распаковываем Grammar
    [ formatNT nt ++ " -> " ++ intercalate " | " (map formatAlt alts)
    | (nt, alts) <- Map.toList grammar
    ]
  where
    formatNT (NonTerminal c) = [c]
    formatAlt (TerminalOnly (Terminal t)) = [t]
    formatAlt (RightLinearAlt (Terminal t) (NonTerminal nt)) = [t] ++ [nt]
    formatAlt (LeftLinearAlt (NonTerminal nt) (Terminal t)) = [nt] ++ [t]

formatStateSet :: Set.Set State -> String
formatStateSet s = "{" ++ intercalate "," (map show (Set.toList s)) ++ "}"

formatNFATransitions :: NFA -> [String]
formatNFATransitions nfa =
    [ show from ++ " -" ++ [c] ++ "-> " ++ formatStateSet toSet
    | ((from, Symbol c), toSet) <- Map.toList (nfaTransitions nfa)
    ]

formatDFATransitions :: DFA -> [String]
formatDFATransitions dfa =
    [ show from ++ " -" ++ [c] ++ "-> " ++ show to
    | ((from, Symbol c), to) <- Map.toList (dfaTransitions dfa)
    ]
