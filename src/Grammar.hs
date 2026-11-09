-- Grammar.hs
module Grammar
    ( NonTerminal(..)
    , Terminal(..)
    , Alternative(..)
    , Grammar(..)
    , GrammarType(..)
    , parseGrammar
    , getGrammarType
    , validateGrammar
    , grammarStart
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.Char (isUpper, isLower, isSpace)

newtype NonTerminal = NonTerminal Char deriving (Eq, Ord, Show)
newtype Terminal = Terminal Char deriving (Eq, Ord, Show)

data Alternative
    = TerminalOnly Terminal
    | RightLinearAlt Terminal NonTerminal
    | LeftLinearAlt NonTerminal Terminal
    deriving (Eq, Show)

data Grammar = Grammar NonTerminal (Map.Map NonTerminal [Alternative])
    deriving (Show)

data GrammarType
    = RightLinear
    | LeftLinear
    deriving (Eq, Show)

trim :: String -> String
trim = filter (not . isSpace)

parseAlternative :: String -> Either String Alternative
parseAlternative alt = case alt of
    [c]
        | isLower c -> Right $ TerminalOnly (Terminal c)
        | otherwise -> Left $ "Invalid terminal: " ++ [c]
    [c1, c2]
        | isLower c1 && isUpper c2 -> Right $ RightLinearAlt (Terminal c1) (NonTerminal c2)
        | isUpper c1 && isLower c2 -> Right $ LeftLinearAlt (NonTerminal c1) (Terminal c2)
        | otherwise -> Left $ "Invalid alternative: " ++ alt
    _ -> Left $ "Invalid alternative length: " ++ alt

parseGrammar :: String -> Either String Grammar
parseGrammar str = do
    rules <- mapM parseRule ruleTexts
    case rules of
        [] -> Left "Grammar must contain at least one rule"
        ((start, _):_) -> Right (Grammar start (Map.fromListWith (flip (++)) rules))
  where
    trimmed = trim str
    ruleStrs = filter (not . null) $ splitOn ";" trimmed
    ruleTexts = ruleStrs

    parseRule :: String -> Either String (NonTerminal, [Alternative])
    parseRule ruleText =
        case splitOn "=" ruleText of
            [leftPart, rightPart] -> do
                leftNT <- parseNonTerminal leftPart
                rightAlts <- parseRightPart rightPart
                return (leftNT, rightAlts)
            _ -> Left $ "Invalid rule format: " ++ ruleText

    parseNonTerminal :: String -> Either String NonTerminal
    parseNonTerminal [c]
        | isUpper c = Right (NonTerminal c)
    parseNonTerminal bad = Left $ "Invalid nonterminal: " ++ bad

    parseRightPart :: String -> Either String [Alternative]
    parseRightPart "" = Left "Expected a nonempty right-hand side"
    parseRightPart rightPart = mapM parseAlternative (splitOn "|" rightPart)

getGrammarType :: Grammar -> Either String GrammarType
getGrammarType (Grammar _ grammar) =
    let allAlts = concat $ Map.elems grammar
        hasRight = any isRightLinear allAlts
        hasLeft  = any isLeftLinear allAlts
    in case (hasRight, hasLeft) of
        (True, False) -> Right RightLinear
        (False, True) -> Right LeftLinear
        (False, False) -> Right RightLinear
        (True, True)  -> Left "Mixed alternatives detected"

isRightLinear :: Alternative -> Bool
isRightLinear (RightLinearAlt _ _) = True
isRightLinear _ = False

isLeftLinear :: Alternative -> Bool
isLeftLinear (LeftLinearAlt _ _) = True
isLeftLinear _ = False

validateGrammar :: Grammar -> GrammarType -> Either String ()
validateGrammar (Grammar start grammar) gtype
    | Map.null grammar || Set.notMember start (Map.keysSet grammar) =
        Left "Start symbol must be defined"
    | not (null invalidAlts) =
        Left ("Alternatives do not match grammar type: " ++ show invalidAlts)
    | not (Set.null undefinedNTs) =
        Left ("Undefined nonterminals: " ++ show (Set.toList undefinedNTs))
    | not (null emptyNTs) =
        Left ("Every rule must contain an alternative: " ++ show emptyNTs)
    | otherwise = Right ()
  where
    allAlternatives = concat (Map.elems grammar)
    invalidAlts = filter (not . matchesType gtype) allAlternatives
    defined = Map.keysSet grammar
    used = Set.unions (map alternativesUsed allAlternatives)
    undefinedNTs = Set.difference used defined
    emptyNTs = [nt | (nt, alts) <- Map.toList grammar, null alts]

    matchesType RightLinear (RightLinearAlt _ _) = True
    matchesType RightLinear (TerminalOnly _)     = True
    matchesType LeftLinear  (LeftLinearAlt _ _)  = True
    matchesType LeftLinear  (TerminalOnly _)     = True
    matchesType _ _ = False

    alternativesUsed (RightLinearAlt _ nt) = Set.singleton nt
    alternativesUsed (LeftLinearAlt nt _)  = Set.singleton nt
    alternativesUsed (TerminalOnly _)      = Set.empty

grammarStart :: Grammar -> NonTerminal
grammarStart (Grammar start _) = start
