{-# LANGUAGE TupleSections #-}
module EPseudocode.Evaluator (interpret)
where

import Control.Monad.Except
import Data.Foldable (foldlM)
import Data.List ((\\))
import Debug.Trace

import EPseudocode.Data
import EPseudocode.Lexer
import EPseudocode.Helpers
import EPseudocode.Parser


interpret :: Env -> String -> Error (Env, Expr)
interpret env input = eParse mainParser input >>= interpret' env


interpret' :: Env -> [Stmt] -> Error (Env, Expr)
interpret' env stmts = foldlM runUntilBreak (env, undefined) stmts
    where
    runUntilBreak :: (Env, Expr) -> Stmt -> Error (Env, Expr)
    runUntilBreak (e, _) stmt = case stmt of
        Break -> return ((":stopiteration:", Bool True):env, Void)
        _ -> if continueIteration e then eval e stmt else return (e, Void)


applyToNamedList :: Env -> Expr -> Maybe Expr -> Error (Env, Expr)
applyToNamedList env (Index name [e]) newVal =
    case lookup name env of
        Nothing -> throwError $ "Unbound variable name " ++ name
        Just (List list) -> do
            (_, index) <- eval env $ E e
            case index of
                (Int i) ->
                    if fromIntegral i < length list && i >= 0 then
                        maybe (return (env, list !! fromIntegral i))
                            (\v -> return ((name, List $ replace (fromIntegral i) v list):env, v))
                            newVal
                    else
                        throwError $ invalidListIndex name i
                _ -> throwError "List can be indexed only with Integer evaluating expressions"
        Just (String str) -> do
            (_, index) <- eval env $ E e
            case index of
                (Int i) ->
                    if fromIntegral i < length str && i >= 0 then
                        maybe (return (env, String [str !! fromIntegral i]))
                            (\(String v) -> return ((name, String $ replaceString (fromIntegral i) v str):env, String v))
                            newVal
                    else
                        throwError $ invalidStrIndex name i
                _ -> throwError "Strings can be indexed only with Integer evaluating expressions"
        _ -> throwError "Only Lists and Strings can be indexed"
applyToNamedList env (Index name (e:es)) newVal =
    case lookup name env of
        Nothing -> throwError $ "Unbound variable name " ++ name
        Just (List list) -> do
            (_, index) <- eval env $ E e
            case index of
                (Int i) ->
                    if fromIntegral i < length list && i >= 0 then
                        case list !! fromIntegral i of
                            (List l) ->
                                maybe (liftM (env,) $ applyToAnonList env es l Nothing)
                                    (\v -> do val <- applyToAnonList env es l (Just v)
                                              return ((name, List $ replace (fromIntegral i) val list):env, v))
                                    newVal
                            (String str) ->
                                maybe (liftM (env,) $ applyToAnonString env es str Nothing)
                                    (\v -> do val <- applyToAnonString env es str (Just v)
                                              return ((name, List $ replace (fromIntegral i) val list):env, v))
                                    newVal
                            _ -> throwError "Only Lists and Strings can be indexed"

                    else
                        throwError $ invalidListIndex name i
                _ -> throwError "List can be indexed only with Integer evaluating expressions"
        _ -> throwError "Multiple indexing can be applied only to Lists"


applyToAnonString :: Env -> IndexingListExpr -> String -> Maybe Expr -> Error Expr
applyToAnonString env [e] str newVal = do
    (_, index) <- eval env $ E e
    case index of
        (Int i) ->
            if fromIntegral i < length str && i >= 0 then
                maybe (return $ String [str !! fromIntegral i])
                    (\(String v) -> return . String $ replaceString (fromIntegral i) v str)
                    newVal
            else
                throwError $ invalidNestedStrIndex i
        _ -> throwError "Strings can be indexed only with Integer evaluating expressions"


applyToAnonList :: Env -> IndexingListExpr -> IndexedList -> Maybe Expr -> Error Expr
applyToAnonList env [e] list newVal = do
    (_, index) <- eval env $ E e
    case index of
        (Int i) ->
            if fromIntegral i < length list && i >= 0 then
                maybe (return $ list !! fromIntegral i)
                    (\v -> return . List $ replace (fromIntegral i) v list)
                    newVal
            else
                throwError $ invalidNestedListIndex i
        _ -> throwError "List can be indexed only with Integer evaluating expressions"
applyToAnonList env (e:es) list newVal = do
    (_, index) <- eval env $ E e
    case index of
        (Int i) ->
            if fromIntegral i < length list && i >= 0 then
                case list !! fromIntegral i of
                    (List l) ->
                        maybe (applyToAnonList env es l Nothing)
                            (\v -> do val <- applyToAnonList env es l (Just v)
                                      return $ List $ replace (fromIntegral i) val list)
                            newVal
                    (String str) ->
                        maybe (applyToAnonString env es str Nothing)
                            (\v -> do val <- applyToAnonString env es str (Just v)
                                      return $ List $ replace (fromIntegral i) val list)
                            newVal
                    _ -> throwError "Only Lists and Strings can be indexed"
            else
                throwError $ invalidNestedListIndex i
        _ -> throwError "List can be indexed only with Integer evaluating expressions"


eval :: Env -> Stmt -> Error (Env, Expr)
--TODO: everything in the AST, both Expr and Stmt
eval env Break = return (env, Void)
eval _ (E Void) = throwError "Cannot evaluate Void. Statement used in expression?"
eval env (E a@(Int _)) = return (env, a)
eval env (E a@(Float _)) = return (env, a)
eval env (E a@(String _)) = return (env, a)
eval env (E a@(Bool _)) = return (env, a)
eval env (E (List a)) = do
    x <- mapM (eval env . E) a
    return (env, List $ map snd x)
eval env (E (Var name)) = maybe (throwError $ "Unbound variable name " ++ name)
    (\val -> return (env, val))
    $ lookup name env
eval env (E index@(Index _ _)) = applyToNamedList env index Nothing
eval env (E unExpr@(UnExpr _ _)) = evalUnExpr env unExpr >>= eval env . E
eval env (E binExpr@BinExpr{}) = evalBinExpr env binExpr >>= eval env . E
eval env (Ret expr) = eval env (E expr) >>= \(e, val) -> return ((":ret:", val):e, val) -- I can do this since Ret may appear only inside functions and the environment is not propagated outside the functions
eval env (Assign (Var name) s) = do
    (newEnv, val) <- eval env $ E s
    return ((name,val) : newEnv, val)
eval env (Assign index@(Index _ _) s) = do
    (_, val) <- eval env $ E s
    applyToNamedList env index $ Just val
eval env (SimpleIf cond stmts) = case eval env (E cond) of
    Left err -> throwError err
    Right (newEnv, Bool val) -> if val
        then evalStmtBody newEnv stmts
        else return (newEnv, Void)
    _ -> throwError "An If's condition should evaluate to Bool"
eval env (CompleteIf cond trueStmts falseStmts) = case eval env (E cond) of
    Left err -> throwError err
    Right (newEnv, Bool val) -> evalStmtBody newEnv (if val then trueStmts else falseStmts)
    _ -> throwError "An If's condition should evaluate to Bool"
eval env (While cond stmts) = repeatWhile env cond stmts
eval env (For initial cond it stmts) =
    case initial of
        Nothing -> repeatForBody env cond it stmts
        Just a -> eval env a >>= (\(e, _) -> repeatForBody e cond it stmts)
    where
        repeatForBody bodyEnv bodyCond _ bodyStmts = do
            let stmts' = bodyStmts ++ (case it of
                                    Nothing -> []
                                    Just s -> [s])
            case bodyCond of
                Nothing -> repeatWhile bodyEnv (Bool True) stmts'
                Just a -> repeatWhile bodyEnv a stmts'
eval env (E f@(FuncDef name _ _)) = case name of
    "" -> return (env, f)
    _ -> return ((name, f):env, Void)
eval env (E (FuncCall nameExpr args)) = eval env (E nameExpr) >>= \(e, f) ->
    case f of
        FuncDef _ _ _ -> applyFunc e f args >>= return . (e,)
        _ -> throwError "Only functions are callable"


applyFunc :: Env -> Expr -> [[Expr]] -> Error Expr
applyFunc env (FuncDef _ argNames body) [args] = getEvaledExprList env args >>=
    argsToEnv argNames >>= \e ->
    evalFuncBody (e++env) body
applyFunc _ (FuncDef _ _ _) (_:_) = do
    trace "()()()()() not yet implemented" $ return Void
    return Void


evalFuncBody :: Env -> [Stmt] -> Error Expr
evalFuncBody env (Ret expr:_) = eval env (E expr) >>= \(_, val) -> return val
evalFuncBody env [stmt] = eval env stmt >>= \(e, val) -> case lookup ":ret:" e of
    Nothing -> return val
    Just v -> return v
evalFuncBody env (stmt:stmts) = eval env stmt >>= \(e, _) -> case lookup ":ret:" e of
    Nothing -> evalFuncBody e stmts
    Just v -> return v


argsToEnv :: [String] -> [Expr] -> Error Env
argsToEnv argNames args = if length argNames == length args
    then return $ [(name, arg) | name <- argNames, arg <- args]
    else throwError $ "Trying to pass " ++ show (length args) ++ " args to a function that takes " ++ show (length argNames)


repeatWhile :: Env -> Expr -> [Stmt] -> Error (Env, Expr)
repeatWhile env cond stmts = case eval env (E cond) of
    Left err -> throwError err
    Right (newEnv, Bool val) -> if val && continueIteration env
        then liftM fst (evalStmtBody newEnv stmts) >>= (\e -> repeatWhile e cond stmts)
        else return ((":stopiteration:",Bool False):newEnv, Void)
    _ -> throwError "A loop's condition should evaluate to Bool"


evalStmtBody :: Env -> [Stmt] -> Error (Env, Expr)
evalStmtBody env stmts = liftM fst (interpret' env stmts) >>= (\e -> return (e, Void))


getEvaledExprList :: Env -> [Expr] -> Error [Expr]
getEvaledExprList env l = do
    a' <- mapM (eval env . E) l
    return $ map snd a'


evalBinExpr :: Env -> Expr -> Error Expr
evalBinExpr _ (BinExpr And _ (List _)) = throwError "Cannot apply and to a list"
evalBinExpr _ (BinExpr And (List _) _) = throwError "Cannot apply and to a list"
evalBinExpr _ (BinExpr Or _ (List _)) = throwError "Cannot apply or to a list"
evalBinExpr _ (BinExpr Or (List _) _) = throwError "Cannot apply or to a list"
evalBinExpr _ (BinExpr Mul _ (List _)) = throwError "Cannot multiply a list"
evalBinExpr _ (BinExpr Mul (List _) _) = throwError "Cannot multiply to a list"
evalBinExpr _ (BinExpr Div _ (List _)) = throwError "Cannot divide to a list"
evalBinExpr _ (BinExpr Div (List _) _) = throwError "Cannot divide a list"
evalBinExpr _ (BinExpr Mod _ (List _)) = throwError "Cannot apply mod to a list"
evalBinExpr _ (BinExpr Mod (List _) _) = throwError "Cannot apply mod to a list"
evalBinExpr _ (BinExpr Pow _ (List _)) = throwError "Cannot raise to a list power"
evalBinExpr _ (BinExpr Pow (List _) _) = throwError "Cannot raise a list to power"
evalBinExpr env (BinExpr Lt (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    return . Bool $ a < b
evalBinExpr _ (BinExpr Lt _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Lt (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Le (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    return . Bool $ a <= b
evalBinExpr _ (BinExpr Le _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Le (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Ge (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    return . Bool $ a >= b
evalBinExpr _ (BinExpr Ge _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Ge (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Gt (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    return . Bool $ a > b
evalBinExpr _ (BinExpr Gt _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Gt (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Neq (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    return . Bool $ a /= b
evalBinExpr _ (BinExpr Neq _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Neq (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Eq (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    return . Bool $ a == b
evalBinExpr _ (BinExpr Eq _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Eq (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Plus (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    return . List $ a ++ b
evalBinExpr env (BinExpr Plus l (List r)) = do
    (_, val) <- eval env $ E l
    return . List $ val : r
evalBinExpr env (BinExpr Plus (List l) r) = do
    (_, val) <- eval env $ E r
    return . List $ l ++ [val]
evalBinExpr _ (BinExpr Minus _ (List _)) = throwError "Cannot subtract a list from a value"
evalBinExpr env (BinExpr Minus (List l) r) = do
    (_, val) <- eval env (E r)
    lst <- getEvaledExprList env l
    return . List $ filter (/= val) lst

evalBinExpr _ (BinExpr And (Int _) (Int _)) = throwError "And is invalid on Ints"
evalBinExpr _ (BinExpr And (Int _) (Float _)) = throwError "And is invalid on Int and Float"
evalBinExpr _ (BinExpr And (Float _) (Int _)) = throwError "And is invalid on Float and Int"
evalBinExpr _ (BinExpr And (String _) (Int _)) = throwError "And is invalid on String and Int"
evalBinExpr _ (BinExpr And (Int _) (String _)) = throwError "And is invalid on Int and String"
evalBinExpr _ (BinExpr And (Bool _) (Int _)) = throwError "And is invalid on Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr And (Int _) (Bool _)) = throwError "And is invalid on Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr And (Float _) (Float _)) = throwError "And is invalid on Floats"
evalBinExpr _ (BinExpr And (String _) (Float _)) = throwError "And is invalid on String and Float"
evalBinExpr _ (BinExpr And (Float _) (String _)) = throwError "And is invalid on Float and String"
evalBinExpr _ (BinExpr And (Bool _) (Float _)) = throwError "And is invalid on Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr And (Float _) (Bool _)) = throwError "And is invalid on Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr And (String _) (String _)) = throwError "And is invalid on Strings"
evalBinExpr _ (BinExpr And (Bool _) (String _)) = throwError "And is invalid on Bool and String"
evalBinExpr _ (BinExpr And (String _) (Bool _)) = throwError "And is invalid on String and Bool"
evalBinExpr _ (BinExpr And (Bool l) (Bool r)) = return . Bool $ l && r

evalBinExpr _ (BinExpr Or (Int _) (Int _)) = throwError "Or is invalid on Ints"
evalBinExpr _ (BinExpr Or (Int _) (Float _)) = throwError "Or is invalid on Int and Float"
evalBinExpr _ (BinExpr Or (Float _) (Int _)) = throwError "Or is invalid on Float and Int"
evalBinExpr _ (BinExpr Or (String _) (Int _)) = throwError "Or is invalid on String and Int"
evalBinExpr _ (BinExpr Or (Int _) (String _)) = throwError "Or is invalid on Int and String"
evalBinExpr _ (BinExpr Or (Bool _) (Int _)) = throwError "Or is invalid on Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Or (Int _) (Bool _)) = throwError "Or is invalid on Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Or (Float _) (Float _)) = throwError "Or is invalid on Floats"
evalBinExpr _ (BinExpr Or (String _) (Float _)) = throwError "Or is invalid on String and Float"
evalBinExpr _ (BinExpr Or (Float _) (String _)) = throwError "Or is invalid on Float and String"
evalBinExpr _ (BinExpr Or (Bool _) (Float _)) = throwError "Or is invalid on Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Or (Float _) (Bool _)) = throwError "Or is invalid on Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Or (String _) (String _)) = throwError "Or is invalid on Strings"
evalBinExpr _ (BinExpr Or (Bool _) (String _)) = throwError "Or is invalid on Bool and String"
evalBinExpr _ (BinExpr Or (String _) (Bool _)) = throwError "Or is invalid on String and Bool"
evalBinExpr _ (BinExpr Or (Bool l) (Bool r)) = return . Bool $ l || r

evalBinExpr _ (BinExpr Plus (Int l) (Int r)) = return . Int $ l + r
evalBinExpr _ (BinExpr Plus (Int l) (Float r)) = return . Float $ fromIntegral l + r
evalBinExpr _ (BinExpr Plus (Float l) (Int r)) = return . Float $ l + fromIntegral r
evalBinExpr _ (BinExpr Plus (String l) (Int r)) = return . String $ l ++ show r
evalBinExpr _ (BinExpr Plus (Int l) (String r)) = return . String $ show l ++ r
evalBinExpr _ (BinExpr Plus (Bool _) (Int _)) = throwError "Cannot add together Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Plus (Int _) (Bool _)) = throwError "Cannot add together Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Plus (Float l) (Float r)) = return . Float $ l + r
evalBinExpr _ (BinExpr Plus (String l) (Float r)) = return . String $ l ++ show r
evalBinExpr _ (BinExpr Plus (Float l) (String r)) = return . String $ show l ++ r
evalBinExpr _ (BinExpr Plus (Bool _) (Float _)) = throwError "Cannot add together Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Plus (Float _) (Bool _)) = throwError "Cannot add together Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Plus (String l) (String r)) = return . String $ l ++ r
evalBinExpr _ (BinExpr Plus (Bool l) (String r)) = return . String $ (if l then tTrue else tFalse) ++ r
evalBinExpr _ (BinExpr Plus (String l) (Bool r)) = return . String $ l ++ if r then tTrue else tFalse
evalBinExpr _ (BinExpr Plus (Bool _) (Bool _)) = throwError "Cannot add together Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Minus (Int l) (Int r)) = return . Int $ l - r
evalBinExpr _ (BinExpr Minus (Int l) (Float r)) = return . Float $ fromIntegral l - r
evalBinExpr _ (BinExpr Minus (Float l) (Int r)) = return . Float $ l - fromIntegral r
evalBinExpr _ (BinExpr Minus (String _) (Int _)) = throwError "Cannot subtract Int from String" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Int _) (String _)) = throwError "Cannot subtract String from Int" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Bool _) (Int _)) = throwError "Cannot subtract Int from Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Int _) (Bool _)) = throwError "Cannot subtract Bool from Int" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Float l) (Float r)) = return . Float $ l - r
evalBinExpr _ (BinExpr Minus (String _) (Float _)) = throwError "Cannot subtract Float from String" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Float _) (String _)) = throwError "Cannot subtract String from Float" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Bool _) (Float _)) = throwError "Cannot subtract Float from Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Float _) (Bool _)) = throwError "Cannot subtract Bool from Float" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (String l) (String r)) = return . String $ l \\ r
evalBinExpr _ (BinExpr Minus (Bool _) (String _)) = throwError "Cannot subtract String from Bool"
evalBinExpr _ (BinExpr Minus (String _) (Bool _)) = throwError "Cannot subtract Bool from String"
evalBinExpr _ (BinExpr Minus (Bool _) (Bool _)) = throwError "Cannot subtract Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Mul (Int l) (Int r)) = return . Int $ l * r
evalBinExpr _ (BinExpr Mul (Int l) (Float r)) = return . Float $ fromIntegral l * r
evalBinExpr _ (BinExpr Mul (Float l) (Int r)) = return . Float $ l * fromIntegral r
evalBinExpr _ (BinExpr Mul (String _) (Int _)) = throwError "Cannot multiply String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Int _) (String _)) = throwError "Cannot multiply Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Bool _) (Int _)) = throwError "Cannot multiply Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Int _) (Bool _)) = throwError "Cannot multiply Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Float l) (Float r)) = return . Float $ l * r
evalBinExpr _ (BinExpr Mul (String _) (Float _)) = throwError "Cannot multiply String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Float _) (String _)) = throwError "Cannot multiply Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Bool _) (Float _)) = throwError "Cannot multiply Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Float _) (Bool _)) = throwError "Cannot multiply Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (String _) (String _)) = throwError "Cannot multiply Strings"
evalBinExpr _ (BinExpr Mul (Bool _) (String _)) = throwError "Cannot multiply Bool and String"
evalBinExpr _ (BinExpr Mul (String _) (Bool _)) = throwError "Cannot multiply String and Bool"
evalBinExpr _ (BinExpr Mul (Bool _) (Bool _)) = throwError "Cannot multiply Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Div (Int l) (Int r)) = return . Float $ fromIntegral l / fromIntegral r
evalBinExpr _ (BinExpr Div (Int l) (Float r)) = return . Float $ fromIntegral l / r
evalBinExpr _ (BinExpr Div (Float l) (Int r)) = return . Float $ l / fromIntegral r
evalBinExpr _ (BinExpr Div (String _) (Int _)) = throwError "Cannot divide String by Int" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Int _) (String _)) = throwError "Cannot divide Int by String" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Bool _) (Int _)) = throwError "Cannot divide Bool by Int" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Int _) (Bool _)) = throwError "Cannot divide Int by Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Float l) (Float r)) = return . Float $ l / r
evalBinExpr _ (BinExpr Div (String _) (Float _)) = throwError "Cannot divide String by Float" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Float _) (String _)) = throwError "Cannot divide Float by String" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Bool _) (Float _)) = throwError "Cannot divide Bool by Float" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Float _) (Bool _)) = throwError "Cannot divide Float by Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Div (String _) (String _)) = throwError "Cannot divide Strings"
evalBinExpr _ (BinExpr Div (Bool _) (String _)) = throwError "Cannot divide Bool by String"
evalBinExpr _ (BinExpr Div (String _) (Bool _)) = throwError "Cannot divide String by Bool"
evalBinExpr _ (BinExpr Div (Bool _) (Bool _)) = throwError "Cannot divide Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Mod (Int l) (Int r)) = return . Int $ l `mod` r
evalBinExpr _ (BinExpr Mod (Int _) (Float _)) = throwError "Modulo cannot be applied to Int and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Float _) (Int _)) = throwError "Modulo cannot be applied to Float and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (String _) (Int _)) = throwError "Modulo cannot be applied to String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Int _) (String _)) = throwError "Modulo cannot be applied to Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Bool _) (Int _)) = throwError "Modulo cannot be applied to Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Int _) (Bool _)) = throwError "Modulo cannot be applied to Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Float _) (Float _)) = throwError "Modulo cannot be applied to Floats" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (String _) (Float _)) = throwError "Modulo cannot be applied to String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Float _) (String _)) = throwError "Modulo cannot be applied to Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Bool _) (Float _)) = throwError "Modulo cannot be applied to Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Float _) (Bool _)) = throwError "Modulo cannot be applied to Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (String _) (String _)) = throwError "Modulo cannot be applied to Strings"
evalBinExpr _ (BinExpr Mod (Bool _) (String _)) = throwError "Modulo cannot be applied to Bool and String"
evalBinExpr _ (BinExpr Mod (String _) (Bool _)) = throwError "Modulo cannot be applied to String and Bool"
evalBinExpr _ (BinExpr Mod (Bool _) (Bool _)) = throwError "Modulo cannot be applied to Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Lt (Int l) (Int r)) = return . Bool $ l < r
evalBinExpr _ (BinExpr Lt (Int l) (Float r)) = return . Bool $ fromInteger l < r
evalBinExpr _ (BinExpr Lt (Float l) (Int r)) = return . Bool $ l < fromInteger r
evalBinExpr _ (BinExpr Lt (String _) (Int _)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Int _) (String _)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Float l) (Float r)) = return . Bool $ l < r
evalBinExpr _ (BinExpr Lt (String _) (Float _)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Float _) (String _)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (String l) (String r)) = return . Bool $ l < r
evalBinExpr _ (BinExpr Lt (Bool _) (String _)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Lt (String _) (Bool _)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Lt (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Le (Int l) (Int r)) = return . Bool $ l <= r
evalBinExpr _ (BinExpr Le (Int l) (Float r)) = return . Bool $ fromInteger l <= r
evalBinExpr _ (BinExpr Le (Float l) (Int r)) = return . Bool $ l <= fromInteger r
evalBinExpr _ (BinExpr Le (String _) (Int _)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Int _) (String _)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Float l) (Float r)) = return . Bool $ l <= r
evalBinExpr _ (BinExpr Le (String _) (Float _)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Float _) (String _)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Le (String l) (String r)) = return . Bool $ l <= r
evalBinExpr _ (BinExpr Le (Bool _) (String _)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Le (String _) (Bool _)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Le (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Gt (Int l) (Int r)) = return . Bool $ l > r
evalBinExpr _ (BinExpr Gt (Int l) (Float r)) = return . Bool $ fromInteger l > r
evalBinExpr _ (BinExpr Gt (Float l) (Int r)) = return . Bool $ l > fromInteger r
evalBinExpr _ (BinExpr Gt (String _) (Int _)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Int _) (String _)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Float l) (Float r)) = return . Bool $ l > r
evalBinExpr _ (BinExpr Gt (String _) (Float _)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Float _) (String _)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (String l) (String r)) = return . Bool $ l > r
evalBinExpr _ (BinExpr Gt (Bool _) (String _)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Gt (String _) (Bool _)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Gt (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Ge (Int l) (Int r)) = return . Bool $ l >= r
evalBinExpr _ (BinExpr Ge (Int l) (Float r)) = return . Bool $ fromInteger l >= r
evalBinExpr _ (BinExpr Ge (Float l) (Int r)) = return . Bool $ l >= fromInteger r
evalBinExpr _ (BinExpr Ge (String _) (Int _)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Int _) (String _)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Float l) (Float r)) = return . Bool $ l >= r
evalBinExpr _ (BinExpr Ge (String _) (Float _)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Float _) (String _)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (String l) (String r)) = return . Bool $ l >= r
evalBinExpr _ (BinExpr Ge (Bool _) (String _)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Ge (String _) (Bool _)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Ge (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Neq (Int l) (Int r)) = return . Bool $ l /= r
evalBinExpr _ (BinExpr Neq (Int l) (Float r)) = return . Bool $ fromInteger l /= r
evalBinExpr _ (BinExpr Neq (Float l) (Int r)) = return . Bool $ l /= fromInteger r
evalBinExpr _ (BinExpr Neq (String _) (Int _)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Int _) (String _)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Float l) (Float r)) = return . Bool $ l /= r
evalBinExpr _ (BinExpr Neq (String _) (Float _)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Float _) (String _)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (String l) (String r)) = return . Bool $ l /= r
evalBinExpr _ (BinExpr Neq (Bool _) (String _)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Neq (String _) (Bool _)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Neq (Bool l) (Bool r)) = return . Bool $ l /= r

evalBinExpr _ (BinExpr Eq (Int l) (Int r)) = return . Bool $ l == r
evalBinExpr _ (BinExpr Eq (Int l) (Float r)) = return . Bool $ fromInteger l == r
evalBinExpr _ (BinExpr Eq (Float l) (Int r)) = return . Bool $ l == fromInteger r
evalBinExpr _ (BinExpr Eq (String _) (Int _)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Int _) (String _)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Float l) (Float r)) = return . Bool $ l == r
evalBinExpr _ (BinExpr Eq (String _) (Float _)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Float _) (String _)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (String l) (String r)) = return . Bool $ l == r
evalBinExpr _ (BinExpr Eq (Bool _) (String _)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Eq (String _) (Bool _)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Eq (Bool l) (Bool r)) = return . Bool $ l == r

evalBinExpr _ (BinExpr Pow (Int l) (Int r)) = return . Float $ fromInteger l ** fromInteger r
evalBinExpr _ (BinExpr Pow (Int l) (Float r)) = return . Float $ fromInteger l ** r
evalBinExpr _ (BinExpr Pow (Float l) (Int r)) = return . Float $ l ** fromInteger r
evalBinExpr _ (BinExpr Pow (String _) (Int _)) = throwError "Cannot raise String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Int _) (String _)) = throwError "Cannot raise Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Bool _) (Int _)) = throwError "Cannot raise Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Int _) (Bool _)) = throwError "Cannot raise Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Float l) (Float r)) = return . Float $ l ** r
evalBinExpr _ (BinExpr Pow (String _) (Float _)) = throwError "Cannot raise String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Float _) (String _)) = throwError "Cannot raise Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Bool _) (Float _)) = throwError "Cannot raise Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Float _) (Bool _)) = throwError "Cannot raise Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (String _) (String _)) = throwError "Cannot raise Strings"
evalBinExpr _ (BinExpr Pow (Bool _) (String _)) = throwError "Cannot raise Bool and String"
evalBinExpr _ (BinExpr Pow (String _) (Bool _)) = throwError "Cannot raise String and Bool"
evalBinExpr _ (BinExpr Pow (Bool _) (Bool _)) = throwError "Cannot raise Bools" -- FIXME: translate

evalBinExpr env (BinExpr op l r) = do
    (_, a) <- eval env $ E l
    (_, b) <- eval env $ E r
    evalBinExpr env $ BinExpr op a b


evalUnExpr :: Env-> Expr -> Error Expr
evalUnExpr _ (UnExpr UnMinus (Int a)) = return . Int $ -1*a
evalUnExpr _ (UnExpr Not (Int _)) = throwError "Integer cannot be negated"
evalUnExpr _ (UnExpr UnMinus (Float a)) = return . Float $ -1.0*a
evalUnExpr _ (UnExpr Not (Float _)) = throwError "Float cannot be negated"
evalUnExpr _ (UnExpr UnMinus (String a)) = return . String $ reverse a
evalUnExpr _ (UnExpr Not (String _)) = throwError "String cannot be negated"
evalUnExpr _ (UnExpr _ (Bool a)) = return . Bool $ not a
evalUnExpr _ (UnExpr UnMinus (List a)) = return . List $ reverse a
evalUnExpr _ (UnExpr Not (List _)) = throwError "List cannot be negated"
evalUnExpr env (UnExpr op a) =  do
    (_, val) <- eval env $ E a
    evalUnExpr env $ UnExpr op val
