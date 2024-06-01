module Main (main) where

import           Data.List        (isInfixOf)
import           Parse            (parseJson)
import           System.Directory (doesFileExist)

checkFileExist :: String -> IO String
checkFileExist filename = do
     fileExists <- doesFileExist filename
     if fileExists
     then return filename
     else error $ "File doesn't exist: " ++ show filename

checkFileIsJson :: String -> IO String
checkFileIsJson filename = do
    if ".json" `isInfixOf` filename
    then return filename
    else error $ "File is not a json: " ++ show filename

requestContent :: IO String
requestContent = do
    putStrLn "Please provide filename:"
    filename <- getLine
    _ <- checkFileExist filename >>= checkFileIsJson
    readFile filename

main :: IO ()
main = do
    content <- requestContent
    let parseResult = parseJson content
    case parseResult of
        Just jsonValues -> do
            putStrLn "Parse result: "
            print jsonValues
        Nothing -> error "Internal error while parsing the file: parseJson returned Nothing"

