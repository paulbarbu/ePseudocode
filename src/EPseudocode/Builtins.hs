module EPseudocode.Builtins (builtinEnv)
where

import Control.Concurrent
import Control.Exception
import Control.Monad.Except
import Data.Maybe
import Data.List (intercalate)
import System.Directory
import System.IO
import System.IO.Error


import EPseudocode.Data
import EPseudocode.Evaluator

builtinEnv :: Env
builtinEnv = [(name, BuiltinIOFunc f) | (name, f) <- ioBuiltins] ++
    [(name, BuiltinFunc f) | (name, f) <- builtins] ++
    [(":stopiteration:", Bool False)]


ioBuiltins :: [(String, [[Expr]] -> ErrorWithIO Expr)]
ioBuiltins = [
    ("scrie", write)
   ,("citeste", readLine)
   ,("deschide", open)
   ,("inchide", close)
   ,("fscrie", fwrite)
   ,("fciteste", fread)
   ,("apply", applyFunc)
   ,("sleep", sleep)
 ]


builtins :: [(String, [[Expr]] -> Error Expr)]
builtins = [
    ("int", strToInt)
   ,("float", strToFloat)
   ,("lung", listLen)
   ,("floor", floorEpc)
   ,("ceiling", ceilingEpc)
 ]


strToInt :: [[Expr]] -> Error Expr
strToInt [[String arg]] = case listToMaybe (reads arg :: [(Integer, String)]) of
    Nothing -> return $ Bool False
    Just (parsed, remaining) ->
        if remaining == ""
            then return $ Int parsed
            else return $ Bool False
strToInt _ = throwError "int takes a single String argument"


strToFloat :: [[Expr]] -> Error Expr
strToFloat [[String arg]] = case listToMaybe (reads arg :: [(Double, String)]) of
    Nothing -> return $ Bool False
    Just (parsed, remaining) ->
        if remaining == ""
            then return $ Float parsed
            else return $ Bool False
strToFloat _ = throwError "float takes a single String argument"


listLen :: [[Expr]] -> Error Expr
listLen [[List arg]] = return . Int . fromIntegral $ length arg
listLen [[String arg]] = return . Int . fromIntegral $ length arg
listLen _ = throwError "lung takes a single List argument"


write :: [[Expr]] -> ErrorWithIO Expr
write [args] = do
    liftIO . putStr $ intercalate "" $ map writeExpr args
    liftIO $ hFlush stdout
    return Void
    where writeExpr (String s) = s
          writeExpr x = showExpr x
write _ = throwError "scrie takes a variable number of arguments"


readLine :: [[Expr]] -> ErrorWithIO Expr
readLine [[]] = liftIO getLine >>= return . String
readLine _ = throwError "citeste takes zero arguments"


applyFunc :: [[Expr]] -> ErrorWithIO Expr
applyFunc [[Func (argName:argNames) body closure, arg]] = do
    (_, val) <- eval closure (E arg)
    return . Func argNames body $ (argName,val) : closure
applyFunc _ = throwError "apply takes two arguments, a function with at least one parameter and a value to partially apply the fucntion to"


-- WARNING: corner cases are not handled!

open :: [[Expr]] -> ErrorWithIO Expr
open [[String filePath, String "r"]] = do
    fileExists <- liftIO $  doesFileExist filePath
    if fileExists
       then open' filePath ReadMode
       else throwError $ "File " ++ filePath ++ " doesn't exist"
open [[String filePath, String "w"]] = open' filePath WriteMode
open [[String filePath, String "a"]] = open' filePath AppendMode
open _ = throwError "deschide takes two arguments, a String as the file path and a mode describing how to open the file"


open' :: String -> IOMode -> ErrorWithIO Expr
open' filePath mode = liftM File . liftIO $ openBinaryFile filePath mode


close :: [[Expr]] -> ErrorWithIO Expr
close [[File file]] = do
    liftIO $ hClose file
    return Void
close _ = throwError "inchide takes a single argument, the result of deschide"


fwrite :: [[Expr]] -> ErrorWithIO Expr
fwrite [File f:args] = do
    liftIO . hPutStr f . intercalate "" $ map writeExpr args
    liftIO $ hFlush f
    return Void
    where writeExpr (String s) = s
          writeExpr x = showExpr x
fwrite _ = throwError "fscrie takes a variable number of arguments, the first one should be the file to write to"


fread :: [[Expr]] -> ErrorWithIO Expr
fread [[File f]] = do
    i <- liftIO $ try $ hGetLine f
    case i of
        Left err -> if isEOFError err then return (Bool False) else throwError "Unexpected error"
        Right val -> return $ String val
fread _ = throwError "fciteste takes a single argument, the result of deschide"


sleep :: [[Expr]] -> ErrorWithIO Expr
sleep [[Int t]] = (liftIO . threadDelay $ fromIntegral t * 1000) >> return Void
sleep _ = throwError "sleep takes a single int argument"


floorEpc :: [[Expr]] -> Error Expr
floorEpc [[Int i]] = return $ Int i
floorEpc [[Float f]] = return . Int $ floor f
floorEpc _ = throwError "floor takes a single int/float argument"

ceilingEpc :: [[Expr]] -> Error Expr
ceilingEpc [[Int i]] = return $ Int i
ceilingEpc [[Float f]] = return . Int $ ceiling f
ceilingEpc _ = throwError "ceiling takes a single int/float argument"
