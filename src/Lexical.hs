{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lexical
    ( getTokens
    ) where

-- import           Control.Monad (ap, liftM)
import           Data.Maybe (catMaybes, isNothing)

data Token = TNumber Double
            | TString String
            | TSyntax Char
            | TBool Bool
            | TNull
            deriving Show

type Tokens = [Token]

-- newtype TokenWithRemaining a = TokenWithRemaining { runTokenWithRemaining :: String -> (String, Maybe a) }

-- instance Functor TokenWithRemaining where
--     fmap = liftM

-- instance Applicative TokenWithRemaining where
--     pure x = TokenWithRemaining $ (\input -> (input, Just x))
--     (<*>) = ap

-- instance Monad TokenWithRemaining where
--     return = pure
--     TokenWithRemaining l >>= f = TokenWithRemaining $ \input ->
--         let (newInput, result) = l(input)
--         in case result of
--             Nothing    -> (newInput, Nothing)
--             Just value -> runTokenWithRemaining (f value) newInput


getTokens :: String -> Maybe Tokens
getTokens str = if any isNothing tokensWithResult
                then Nothing
                else Just $ catMaybes tokensWithResult
    where tokensWithResult = [tryGetToken t | t <- words str]

tryGetToken :: String -> Maybe Token
tryGetToken str = getTString str
    `orNext` getTBool str
    `orNext` getTNumber str
    `orNext` getTSyntax str

orNext :: Maybe a -> Maybe a -> Maybe a
orNext (Just x) _       = Just x
orNext Nothing nextFunc = nextFunc

getTNumber :: String -> Maybe Token
getTNumber str = Nothing

getTString :: String -> Maybe Token
getTString str = Just $ TString str

getTSyntax :: String -> Maybe Token
getTSyntax str = Nothing

getTBool :: String -> Maybe Token
getTBool str = Nothing

getTNull :: String -> Maybe Token
getTNull str = Nothing
