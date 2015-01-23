module EPseudocode.Stdlib (getStdLibEnv)
where

import Control.Monad.Except
import System.Directory

import EPseudocode.Builtins
import EPseudocode.Data
import EPseudocode.Evaluator


getStdLibEnv :: ErrorWithIO Env
getStdLibEnv = do
    fileExists <- liftIO $ doesFileExist ".stdlib.epc"
    if fileExists
       then do
           contents <- liftIO $ readFile ".stdlib.epc"
           (env, _) <- interpret builtinEnv contents
           return env
       else throwError "File \".stdlib.epc\" doesn't exist"
