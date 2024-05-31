{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- todo: JsonArrayItems, clean things up + write docs, test

module Lib
    ( parseJson
    ) where

import           Lexical (Token (TBool, TNull, TNumber, TString, TSyntax),
                          Tokens, getTokens)
-- import           Data.List (groupBy, isInfixOf)
-- import           Text.Read (readMaybe)

data JsonValue = JsonObject [(String, [JsonValue])]
                 -- | JsonArrayItem [JsonValue]
                 | JsonNumber Double
                 | JsonString String
                 | JsonBool Bool
                 | JsonNull
                 deriving Show

-- getNestedJsonObject :: String -> Tokens -> JsonValue
-- getNestedJsonObject name tokens = JsonObject (name, getJsonValue tokens)

-- getJsonArray :: String -> Tokens -> JsonValue
-- getJsonArray name tokens = JsonArray (name, [getJsonValue fieldTokens | fieldTokens <- getFieldsTokens tokens])

-- getFieldsTokens :: Tokens -> [Tokens]
-- getFieldsTokens [] = []
-- getFieldsTokens tokens = fieldTokens : getFieldsTokens restOfTokens
--     where fieldTokens = takeWhile (/= TSyntax ',') tokens
--           restOfTokens = drop (length fieldTokens + 1) tokens

getJsonValue :: Tokens -> [JsonValue]

getJsonValue ((TString name):(TSyntax ':'):TNull:rest) = JsonObject [(name, [JsonNull])] : getJsonValue rest
getJsonValue ((TString name):(TSyntax ':'):(TNumber num):rest) = JsonObject [(name, [JsonNumber num])] : getJsonValue rest
getJsonValue ((TString name):(TSyntax ':'):(TString str):rest) = JsonObject [(name, [JsonString str])] : getJsonValue rest
getJsonValue ((TString name):(TSyntax ':'):(TBool val):rest) = JsonObject [(name, [JsonBool val])] : getJsonValue rest

getJsonValue ((TString name):(TSyntax ':'):(TSyntax '{'):rest) = JsonObject [(name, getJsonValue objectTokens)] : getJsonValue remainingTokens
    where (objectTokens, remainingTokens) = breakNestedTokens (TSyntax '{', TSyntax '}') rest
getJsonValue ((TString name):(TSyntax ':'):(TSyntax '['):rest) = JsonObject [(name, getJsonValue arrayTokens)] : getJsonValue remainingTokens
    where (arrayTokens, remainingTokens) = breakNestedTokens (TSyntax '[', TSyntax ']') rest

getJsonValue ((TString str):rest) = JsonString str : getJsonValue rest
getJsonValue ((TNumber num):rest) = JsonNumber num : getJsonValue rest
getJsonValue ((TBool val):rest) = JsonBool val : getJsonValue rest
getJsonValue (TNull:rest) = JsonNull : getJsonValue rest

getJsonValue (TSyntax '{':rest) = getJsonValue rest
getJsonValue (TSyntax '}':rest) = getJsonValue rest

getJsonValue (TSyntax '[':rest) = getJsonValue rest
getJsonValue (TSyntax ']':rest) = getJsonValue rest

getJsonValue [] = []

getJsonValue tokens = error $ "Invalid match in getJsonValue " ++ show tokens

-- | Split tokens into two parts: nested tokens, and the rest
breakNestedTokens :: (Token, Token) -> Tokens -> (Tokens, Tokens)
breakNestedTokens brackets tokens = (nestedTokens, drop (length nestedTokens) tokens)
    where nestedTokens = getNestedTokens brackets tokens

getNestedTokens :: (Token, Token) -> Tokens -> Tokens
getNestedTokens brackets = getNestedTokensFromLevel brackets 1

getNestedTokensFromLevel :: (Token, Token) -> Int -> Tokens -> Tokens
getNestedTokensFromLevel (_, _) _ [] = []
getNestedTokensFromLevel (start, end) level (x:xs)
    | level == 0 = []
    | x == start = x : getNestedTokensFromLevel (start, end) (level + 1) xs
    | x == end = x : getNestedTokensFromLevel (start, end) (level - 1) xs
    | otherwise = x : getNestedTokensFromLevel (start, end) level xs


parseJson :: String -> Maybe [JsonValue]
parseJson ""      = Nothing
parseJson content = getJsonValue . filter (/=  TSyntax ',') <$> getTokens content
