import System.Directory
import System.Environment

import System.Console.Haskeline

import EPseudocode.Data
import EPseudocode.Evaluator
import EPseudocode.Parser


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


runFile :: String -> IO ()
runFile filePath = do
    contents <- readFile filePath
    case eParse toplevelParser contents  of
        Left err -> putStrLn $ "failed: " ++ err
        Right p -> case interpretProgram [(":stopiteration:", Bool False)] p of
                      Left err -> putStrLn $ "failed: " ++ err
                      Right _ -> putStrLn $ "ok"
    return ()


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl >> putStrLn "Goodbye!"
        1 -> do
                let filePath = head args
                fileExists <- doesFileExist filePath
                if fileExists
                    then runFile filePath
                    else putStrLn $ "File " ++ filePath ++ " doesn't exist"
        _ -> putStrLn help
