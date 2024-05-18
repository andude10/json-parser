module Main (main) where

import Lib

import System.Directory ( doesFileExist )
import Data.List ( isInfixOf )

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
            print content
        Nothing -> putStrLn "Filename is not valid"
