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
    putStrLn "Data:"
    hFlush stdout
    runTestTT dataTests

    putStrLn "\nEvaluator:"
    hFlush stdout
    runTestTT evaluatorTests

    putStrLn "\nParser:"
    hFlush stdout
    runTestTT parserTests
    -- TODO: runTestTT scopeTests
    return ()
