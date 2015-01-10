module Main (main)
where

import System.IO

import Test.HUnit

import Tests.Data
import Tests.Evaluator
import Tests.Parser

-- import Tests.Scope

main :: IO ()
main = do
    let sum = 0
    putStrLn "Data:"
    hFlush stdout
    a <- runTestTT dataTests

    putStrLn "\nEvaluator:"
    hFlush stdout
    b <- runTestTT evaluatorTests

    putStrLn "\nParser:"
    hFlush stdout
    c <- runTestTT parserTests
    -- TODO: runTestTT scopeTests
    putStrLn $ "\nTotal tests: " ++ show (cases a + cases b + cases c)
    putStrLn $ "Total tried: " ++ show (tried a + tried b + tried c)
    putStrLn $ "Total errors: " ++ show (errors a + errors b + errors c)
    putStrLn $ "Total failures: " ++ show (failures a + failures b + failures c)
    putStrLn ""
    hFlush stdout
    return ()
