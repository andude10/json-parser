module Main (main) where

import           Data.List        (isInfixOf)
import           Lib              (parseJson)
import           System.Directory (doesFileExist)

requestInput :: IO (Maybe String)
requestInput = do
    putStrLn "Please provide filename:"
    filename <- getLine
    fileExists <- doesFileExist filename
    if fileExists && isJsonFile filename
        then return (Just filename)
        else return Nothing
    where isJsonFile = isInfixOf ".json"

main :: IO ()
main = do
    -- input <- requestInput
    content <- readFile "t.json"
    case parseJson content of
        Just tokens -> print tokens
        Nothing     -> error "getTokens returns Nothing"
    -- in case input of
    --     Just filename -> do
    --         content <- readFile filename
    --         let tokens = getTokens content
    --         in print tokens
    --     Nothing -> error "Filename is not valid"
