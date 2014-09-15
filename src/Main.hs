import System.Environment

import EPseudocode.Data as EPD
import EPseudocode.Parser as EPP


help :: String
help = "TODO: One or zero arguments"


runRepl :: IO ()
runRepl = putStrLn "TODO: running repl"

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> readFile (head args) >>= (EPP.runLex EPP.mainParser) -- TODO: cleanly check if file exists
        _ -> putStrLn help
