import System.Directory
import System.Environment

import System.Console.Haskeline

import EPseudocode.Data
import EPseudocode.Evaluator
import EPseudocode.Parser


help :: String
help = "ePseudocode, A small programming language (with pseudocode appearance) implemented in Haskell\
\\n\n* Pass zero arguments to start the REPL \n\n\
\* When passing one or more arguments the first one should be the file \n\
\  to be interpreted, with optional arguments for its main function\n\n\
\* Pass \"--help\" or \"-h\" as a single argument to display this help"


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


runFile :: String -> [String] -> IO ()
runFile filePath argv = do
    contents <- readFile filePath
    case eParse toplevelParser contents  of
        Left err -> putStrLn $ "failed: " ++ err
        Right p -> case interpretProgram [(":stopiteration:", Bool False)] p argv of
                      Left err -> putStrLn $ "failed: " ++ err
                      Right _ -> putStrLn $ "ok"
    return ()


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl >> putStrLn "Goodbye!"
        _ -> let filePath = head args in
            if (filePath == "--help" || filePath == "-h") && length args == 1
                then putStrLn help
                else do fileExists <- doesFileExist filePath
                        if fileExists
                           then runFile filePath $ tail args
                           else putStrLn $ "File " ++ filePath ++ " doesn't exist"
