module Main where

import HAheui.CommandLine as HAheui

import Data.Either (isLeft, fromLeft)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    -- Parsing command lines
    parsedData <- HAheui.parseCommandLine <$> getArgs
    if isLeft parsedData
        then do
            putStrLn (fromLeft [] parsedData)
            exitFailure
        else do return ()
    putStrLn "Hello, world!"