module Main (main)
where

import Test.HUnit

import Tests.Data
import Tests.Evaluator
import Tests.Parser

-- import Tests.Scope

main :: IO ()
main = do
    runTestTT dataTests
    runTestTT evaluatorTests
    runTestTT parserTests
    -- TODO: runTestTT scopeTests
    return ()
