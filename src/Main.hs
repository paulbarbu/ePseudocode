import System.Environment

import System.Console.Haskeline

import qualified EPseudocode.Parser as EPP


help :: String
help = "TODO: One or zero arguments"


-- TODO: multiline input
runRepl :: IO ()
runRepl = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            line <- getInputLine "epc> "
            case line of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do
                    outputStrLn $ EPP.runParser input
                    loop


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> readFile (head args) >>= \contents -> putStrLn $ EPP.runParser contents -- TODO: cleanly check if file exists
        _ -> putStrLn help
