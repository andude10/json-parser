module Main (main) where

import           Lexical          (getTokens)

import           Data.List        (isInfixOf)
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
    input <- requestInput
    case input of
        Just filename -> do
            content <- readFile filename
            let res = getTokens content
            case res of
                Just tokens -> print tokens
                Nothing     -> error "Error while parsing file"
        Nothing -> error "Filename is not valid"
