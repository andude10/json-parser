module Lib
    ( parseJson
    ) where

import System.Directory ( doesFileExist )
import Data.List ( isInfixOf )
import Data.Text (splitOn)
import Text.Read (readMaybe)

data JsonValue = JsonNumber Double
                 | JsonString String
                 | JsonObject [(String, JsonValue)]

isNumber :: String -> Bool
isNumber str = case readMaybe str :: Maybe Double of
    Just _  -> True
    Nothing -> False

getJsonValue :: String -> JsonValue
getJsonValue content = JsonNumber 5
-- | length sections > 0 = getJsonValue content
-- = content case of 
--     where sections = splitOn "," content

isValidJson :: String -> Bool
isValidJson json = all ($ json) jsonCriterions
    where jsonCriterions = [everyBracketIsClosed]
          everyBracketIsClosed str = and [isClosed opening closing str | opening <- ['(', '{', '['], closing <- [')', '}', ']']]
          isClosed opening closing = (== 0) . foldr (\x acc -> if x == opening then acc + 1 else if x == closing then acc - 1 else acc) 0

parseJson :: String -> Maybe JsonValue
parseJson "" = Nothing
parseJson content = if isValidJson content then Just $ getJsonValue content else Nothing
