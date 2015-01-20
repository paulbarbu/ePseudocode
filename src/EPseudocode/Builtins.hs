module EPseudocode.Builtins (builtinEnv)
where

import Control.Monad.Except
import Debug.Trace

import EPseudocode.Data


builtinEnv :: Env
builtinEnv = [(name, PrimitiveIOFunc f) | (name, f) <- ioBuiltins] ++
    [(":stopiteration:", Bool False)]


ioBuiltins :: [(String, [[Expr]] -> Error Expr)]
ioBuiltins = [
    ("scrie", write)
 ]


write :: [[Expr]] -> Error Expr
write [[args]] = trace (show args) $ return Void
write [_:_] = throwError "scrie takes a single argument list: scrie(1, 2, 3, 4)"
