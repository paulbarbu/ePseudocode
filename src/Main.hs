import System.Environment

import System.Console.Haskeline

import EPseudocode.Data
import EPseudocode.Evaluator
import EPseudocode.Parser
-- TODO: import EPseudocode.Scope


help :: String
help = "TODO: One or zero arguments"

-- TODO: cleanup

-- TODO: multiline input
runRepl :: IO ()
runRepl = runInputT defaultSettings $ loop [(":stopiteration:", Bool False)]
    where
        loop :: Env -> InputT IO ()
        loop env = do
            line <- getInputLine "epc> "
            case line of
                Nothing -> return ()
                Just ":quit" -> return ()
                Just ":env" -> do
                    outputStrLn $ "env: " ++ show env
                    loop env
                Just input ->
                    case interpret env input of
                        Left err -> outputStrLn err >> loop env
                        Right res -> do
                            x <- outputStrLn . showExpr . snd $ res
                            loop $ fst res
                            return x


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl >> putStrLn "Goodbye!"
        -- 1 -> readFile (head args) >>= \contents -> putStrLn $ runParser toplevelParser contents -- TODO: cleanly check if file exists
        1 -> do
            contents <- readFile (head args)
            let prog = eParse toplevelParser contents

            -- TODO: finish this
            --case prog of
            --    Left err -> putStrLn $ "failed: " ++ err
            --    Right p -> do let e = isValidScope p
            --                  case e of
            --                   Left err -> putStrLn $ "failed: " ++ err
            --                   Right n -> putStrLn $ "ok: " ++ show n
            return ()
        _ -> putStrLn help
