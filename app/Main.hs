module Main where

import HAheui.CommandLine as HAheui

import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    -- Parsing command lines
    parsedData <- HAheui.parseCommandLine <$> getArgs
    if not $ HAheui.isDone parsedData
        then do
            putStrLn (HAheui.output parsedData)
            exitSuccess
        else do
            let checkedData = HAheui.checkCommandLine parsedData
            if not $ HAheui.isDone checkedData
                then do
                    putStrLn (HAheui.output checkedData)
                    exitSuccess
                else return ()
    putStrLn "Hello, world!"