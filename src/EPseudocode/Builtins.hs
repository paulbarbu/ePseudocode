module EPseudocode.Builtins (builtinEnv)
where

import Control.Monad.Except
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
strToInt [[String arg]] = return $ Int (read arg :: Integer)
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
readLine [[]] = (liftIO $ getLine) >>= return . String
readLine _ = throwError "citeste takes zero arguments"
