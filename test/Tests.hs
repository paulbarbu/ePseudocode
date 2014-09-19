module Main (main)
where

import Test.HUnit

import Tests.Parser

main :: IO ()
main = do
    runTestTT parserTests
    return ()
