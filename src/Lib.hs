module Lib
    ( -- parseJson
    ) where

import Data.List (isInfixOf, groupBy)
import Text.Read (readMaybe)

data JsonValue = JsonNumber Double
                 | JsonString String
                 | JsonObject [(String, JsonValue)]

-- getJsonValue :: String -> JsonValue
-- getJsonValue content
--     | length content > 1 = JsonObject (map getJsonValue sections)
--     | otherwise = JsonNumber 5

    
-- extract :: String -> JsonValue
-- extract line = JsonNumber 5
    
-- isObject :: String -> Bool
-- isObject line = (splitExpression line)!!1 

-- hasBrackets :: String -> String
-- hasBrackets = any ""

-- splitExpression :: String -> [String]
-- splitExpression = groupBy (const (/= ':'))

-- splitSections :: String -> [String]
-- splitSections = groupBy (const (/= ','))


isValidJson :: String -> Bool
isValidJson json = all ($ json) jsonCriterions
    where jsonCriterions = [everyBracketIsClosed]

everyBracketIsClosed :: String -> Bool
everyBracketIsClosed str = all isClosed [('(', ')'), ('{', '}'), ('[', ']')]
    where isClosed (opening, closing) = foldr (\x acc -> if x == opening then acc + 1 else if x == closing then acc - 1 else acc) 0 str == 0

-- parseJson :: String -> Maybe JsonValue
-- parseJson "" = Nothing
-- parseJson content = if isValidJson content then Just $ getJsonValue content else Nothing
