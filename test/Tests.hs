module Main (main)
where

import System.IO

import Test.HUnit

import Tests.Data
import Tests.Evaluator
import Tests.Parser

red = "\x1b[31m"
green = "\x1b[32m"
clear = "\x1b[0m"

main :: IO ()
main = do
    let sum = 0

    putStrLn "Data:"
    hFlush stdout
    a <- runTestTT dataTests

    putStrLn "\nParser:"
    hFlush stdout
    b <- runTestTT parserTests

    putStrLn "\nEvaluator:"
    hFlush stdout
    c <- runTestTT evaluatorTests

    let sum_err = errors a + errors b + errors c
    let sum_f = failures a + failures b + failures c

    putStrLn $ "\nTotal tests: " ++ show (cases a + cases b + cases c)
    putStrLn $ "Total tried: " ++ show (tried a + tried b + tried c)

    if sum_err > 0 then putStr red else putStr green
    putStrLn $ "Total errors: " ++ show sum_err
    putStr clear

    if sum_f > 0 then putStr red else putStr green
    putStrLn $ "Total failures: " ++ show sum_f
    putStr clear

    putStrLn ""
    hFlush stdout
    return ()
