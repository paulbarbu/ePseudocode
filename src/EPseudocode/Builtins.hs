module EPseudocode.Builtins (builtinEnv)
where

import Control.Monad.Except
import Data.Maybe
import Data.List (intercalate)
import System.IO

import EPseudocode.Data

builtinEnv :: Env
builtinEnv = [(name, BuiltinIOFunc f) | (name, f) <- ioBuiltins] ++
    [(name, BuiltinFunc f) | (name, f) <- builtins] ++
    [(":stopiteration:", Bool False)]


ioBuiltins :: [(String, [[Expr]] -> ErrorWithIO Expr)]
ioBuiltins = [
    ("scrie", write)
   ,("citeste", readLine)
 ]


builtins :: [(String, [[Expr]] -> Error Expr)]
builtins = [
    ("int", strToInt)
   ,("lung", listLen)
 ]


strToInt :: [[Expr]] -> Error Expr
strToInt [[String arg]] = case listToMaybe $ reads arg of
    Nothing -> return $ List [Bool False, String $ "int cannot parse " ++ arg]
    Just (parsed, remaining) ->
        if remaining == ""
            then return $ List [Bool True, Int parsed]
            else return $ List [Bool False, String $ "int cannot parse " ++ arg]
strToInt _ = throwError "int takes a single String argument"


listLen :: [[Expr]] -> Error Expr
listLen [[List arg]] = return . Int . fromIntegral $ length arg
listLen _ = throwError "lung takes a single List argument"


write :: [[Expr]] -> ErrorWithIO Expr
write [args] = do
    liftIO . putStr $ intercalate "" $ map writeExpr args
    liftIO $ hFlush stdout
    return Void
    where writeExpr (String s) = s
          writeExpr x = showExpr x
write _ = throwError "scrie takes a single argument list: scrie(1, 2, 3, 4)"


readLine :: [[Expr]] -> ErrorWithIO Expr
readLine [[]] = liftIO getLine >>= return . String
readLine _ = throwError "citeste takes zero arguments"
