module EPseudocode.Builtins (builtinEnv)
where

import Control.Monad.Except
import Data.List (intercalate)
import System.IO

import EPseudocode.Data

builtinEnv :: Env
builtinEnv = [(name, PrimitiveIOFunc f) | (name, f) <- ioBuiltins] ++
    [(":stopiteration:", Bool False)]


ioBuiltins :: [(String, [[Expr]] -> ErrorWithIO Expr)]
ioBuiltins = [
    ("scrie", write)
   ,("citeste", readLine)
 ]


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
