{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- todo: JsonArrayItems, clean things up + write docs, test

module Parse
    ( parseJson,
      JsonValue (..)
    ) where

import           Lexical (Token (TBool, TNull, TNumber, TString, TSyntax),
                          Tokens, getTokens)

data JsonValue = JsonObject [(String, [JsonValue])]
                 | JsonNumber Double
                 | JsonString String
                 | JsonBool Bool
                 | JsonNull
                 deriving Show

-- | Convert a string containing Json content into [JsonValue].
-- Returns Nothing if an internal error occurred
parseJson :: String -> Maybe [JsonValue]
parseJson ""      = Nothing
parseJson content = getJsonValue . filter (/=  TSyntax ',') <$> getTokens content

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

-- todo: replace runtime error with Maybe
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
