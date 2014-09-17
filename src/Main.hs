import System.Environment

import System.Console.Haskeline

import EPseudocode.Data as EPD
import EPseudocode.Parser as EPP


help :: String
help = "TODO: One or zero arguments"


-- TODO: multiline input
runRepl :: IO ()
runRepl = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            input <- getInputLine "epc> "
            case input of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do
                    outputStrLn $ EPP.runLex EPP.mainParser input
                    loop


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> readFile (head args) >>= \contents -> putStrLn $ EPP.runLex EPP.mainParser contents -- TODO: cleanly check if file exists
        _ -> putStrLn help
