module Main (main)
where

import Test.HUnit

import Tests.Parser
import Tests.Scope

main :: IO ()
main = do
    runTestTT parserTests
    runTestTT scopeTests
    return ()
