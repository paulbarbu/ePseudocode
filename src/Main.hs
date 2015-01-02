import System.Environment

import System.Console.Haskeline

import EPseudocode.Parser
import EPseudocode.Scope
import EPseudocode.Evaluator


help :: String
help = "TODO: One or zero arguments"

-- TODO: cleanup

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
                    case eParse mainParser input >>= eval . head of
                        Left err -> outputStrLn err
                        Right output -> outputStrLn $ show output

                    loop


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        -- 1 -> readFile (head args) >>= \contents -> putStrLn $ runParser toplevelParser contents -- TODO: cleanly check if file exists
        1 -> do
            contents <- readFile (head args)
            let prog = eParse toplevelParser contents

            case prog of
                Left err -> putStrLn $ "failed: " ++ err
                Right p -> do let e = isValidScope p
                              case e of
                               Left err -> putStrLn $ "failed: " ++ err
                               Right n -> putStrLn $ "ok: " ++ show n
            return ()
        _ -> putStrLn help
