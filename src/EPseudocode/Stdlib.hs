module EPseudocode.Stdlib (getStdLibEnv)
where

import Control.Monad.Except
import System.Directory

import EPseudocode.Builtins
import EPseudocode.Data
import EPseudocode.Evaluator
import EPseudocode.Parser


getStdLibEnv :: String -> ErrorWithIO Env
getStdLibEnv fileName = do
    fileExists <- liftIO $ doesFileExist fileName
    if fileExists
       then do
           contents <- liftIO $ readFile fileName
           (env, _) <- interpret toplevelParser builtinEnv contents
           return env
       else throwError $ "File " ++ fileName ++ " doesn't exist"
