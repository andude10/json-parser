{-# LANGUAGE GADTs         #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module Lexical
    ( getTokens,
      Tokens,
      Token (..)
    ) where

import           Control.Monad (ap, liftM)
import           Data.Char     (isSpace)
import           Data.List     (isPrefixOf)

data Token where
  TNumber :: Double -> Token
  TString :: String -> Token
  TSyntax :: Char -> Token
  TBool :: Bool -> Token
  TNull :: Token
  deriving (Show, Eq)

type Tokens = [Token]

-- | Monad representing extracted Token, and the remaining content after that Token
newtype WithRemaining a = WithRemaining { runWithRemaining :: String -> (String, Maybe a) }

instance Functor WithRemaining where
    fmap :: (a -> b) -> WithRemaining a -> WithRemaining b
    fmap = liftM

instance Applicative WithRemaining where
    pure :: a -> WithRemaining a
    pure x = WithRemaining (, Just x)

    (<*>) :: WithRemaining (a -> b) -> WithRemaining a -> WithRemaining b
    (<*>) = ap

instance Monad WithRemaining where
    return :: a -> WithRemaining a
    return = pure

    (>>=) :: WithRemaining a -> (a -> WithRemaining b) -> WithRemaining b
    WithRemaining tokenWithRemaining >>= transform = WithRemaining $ \input ->
        let (remainingStr, token) = tokenWithRemaining input
        in case token of
            Nothing    -> (remainingStr, Nothing)
            Just value -> runWithRemaining (transform value) remainingStr

-- | Turn Json content into tokens.
-- Returns Nothing if couldn't extract any token
getTokens :: String -> Maybe Tokens
getTokens content =
    let (remainingStr, token) = runWithRemaining (tryExtractToken cleanedContent) cleanedContent
    in case token of
        Just value -> case getTokens remainingStr of
                        Just rest -> Just (value : rest)
                        Nothing   -> Nothing
        Nothing    -> if null remainingStr
                      then Just []
                      else error remainingStr
    where cleanedContent = removeSpecialCharacters $ removeFileSpaces content

-- | Remove spaces outside json strings
-- >>> removeFileSpaces "name : \"Name Surname\", number : \"+44 XXX 2345678\""
-- "name:\"Name Surname\",number:\"+44 XXX 2345678\""
removeFileSpaces :: String -> String
removeFileSpaces = snd . foldr processChar (False, [])
    where processChar chr (insideQuotes, acc)
            | chr == '\"' = (not insideQuotes, chr : acc)
            | isSpace chr && not insideQuotes = (insideQuotes, acc)
            | otherwise = (insideQuotes, chr : acc)

removeSpecialCharacters :: String -> String
removeSpecialCharacters = filter (`notElem` "\n\t")

-- | Chain extract functions.
-- Returns Just if some function in the chain was able to extract token,
-- otherwise returns Nothing
tryExtractToken :: String -> WithRemaining Token
tryExtractToken str = tryExtractTSyntax str
    `orRunNext` tryExtractTString str
    `orRunNext` tryExtractTNull str
    `orRunNext` tryExtractTBool str
    `orRunNext` tryExtractTNumber str

-- | Return Token if it was extracted,
-- otherwise run next with remaining string
orRunNext :: WithRemaining Token -> WithRemaining Token -> WithRemaining Token
orRunNext (WithRemaining current) (WithRemaining next) = WithRemaining $ \input ->
    let (remainingStr, token) = current input
    in case token of
        Just value -> (remainingStr, Just value)
        Nothing    -> next remainingStr

tryExtractTNumber :: String -> WithRemaining Token
tryExtractTNumber str = WithRemaining $ \input ->
    case reads str of
        [(num, remainingStr)] -> (remainingStr, Just $ TNumber num)
        _                     -> (input, Nothing)

tryExtractTString :: String -> WithRemaining Token
tryExtractTString _ = WithRemaining $ \input ->
    if (length input > 1) && (head input == '"')
    then let (readStr, remainingStr) = span (/= '"') (tail input)
        in (tail remainingStr, Just $ TString readStr)
    else (input, Nothing)

tryExtractTSyntax :: String -> WithRemaining Token
tryExtractTSyntax (firstChar:rest) | firstChar `elem` "{}[]:," = WithRemaining $ const (rest, Just $ TSyntax firstChar)
tryExtractTSyntax str = WithRemaining $ const (str, Nothing)

tryExtractTBool :: String -> WithRemaining Token
tryExtractTBool _ = WithRemaining $ \input ->
    if "true" `isPrefixOf` input
    then (drop 4 input, Just $ TBool True)
    else if "false" `isPrefixOf` input
    then (drop 5 input, Just $ TBool False)
    else (input, Nothing)

tryExtractTNull :: String -> WithRemaining Token
tryExtractTNull str | "null" `isPrefixOf` str = WithRemaining $ const (drop 4 str, Just TNull)
tryExtractTNull str = WithRemaining $ const (str, Nothing)
