{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module Lexical
    ( getTokens
    ) where

import           Control.Monad (ap, liftM)
import           Data.List     (isPrefixOf)
-- import           Data.Maybe    (catMaybes, isNothing)

data Token = TNumber Double
            | TString String
            | TSyntax Char
            | TBool Bool
            | TNull
            deriving Show

type Tokens = [Token]

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


getTokens :: String -> Maybe Tokens
getTokens str =
    let (remainingStr, token) = runWithRemaining (tryGetToken trimmedStr) trimmedStr
    in case token of
        Nothing    -> if null remainingStr
                      then Just []
                      else error remainingStr
        Just value -> case getTokens remainingStr of
                        Just rest -> Just (value : rest)
                        Nothing   -> Nothing
    where trimmedStr = filter (\x -> x /= ' ' && x /= '\t' && x /= '\n') str


tryGetToken :: String -> WithRemaining Token
tryGetToken str = getTSyntax str
    `orRunNext` getTString str
    `orRunNext` getTNull str
    `orRunNext` getTBool str
    `orRunNext` getTNumber str

orRunNext :: WithRemaining Token -> WithRemaining Token -> WithRemaining Token
orRunNext (WithRemaining current) (WithRemaining next) = WithRemaining $ \input ->
    let (remainingStr, token) = current input
    in case token of
        Nothing    -> next remainingStr
        Just value -> (remainingStr, Just value)

getTNumber :: String -> WithRemaining Token
getTNumber str = WithRemaining $ \input ->
    case reads str of
        [(num, remainingStr)] -> (remainingStr, Just $ TNumber num)
        _                     -> (input, Nothing)

getTString :: String -> WithRemaining Token
getTString str = WithRemaining $ \input ->
    if (length input > 1) && (head input == '"')
    then let (readStr, remainingStr) = span (/= '"') (tail input)
        in (tail remainingStr, Just $ TString readStr)
    else (input, Nothing)

getTSyntax :: String -> WithRemaining Token
getTSyntax (firstChar:rest) | firstChar `elem` "{}[]:," = WithRemaining $ const (rest, Just $ TSyntax firstChar)
getTSyntax str = WithRemaining $ const (str, Nothing)

getTBool :: String -> WithRemaining Token
getTBool str = WithRemaining $ \input ->
    if "true" `isPrefixOf` input
    then (drop 4 input, Just $ TBool True)
    else if "false" `isPrefixOf` input
    then (drop 5 input, Just $ TBool False)
    else (input, Nothing)

getTNull :: String -> WithRemaining Token
getTNull str | "null" `isPrefixOf` str = WithRemaining $ const (drop 4 str, Just TNull)
getTNull str = WithRemaining $ const (str, Nothing)
