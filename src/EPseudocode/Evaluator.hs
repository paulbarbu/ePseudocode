{-# LANGUAGE TupleSections #-}
module EPseudocode.Evaluator (interpret, interpretProgram, eval, interpretStdLib)
where

import Control.Monad.Except
import Data.Foldable (foldlM)
import Data.Maybe
import Data.List ((\\))
import Debug.Trace

import EPseudocode.Data
import EPseudocode.Lexer
import EPseudocode.Helpers
import EPseudocode.Parser


{-
 * TODO: clean the Env, it should not be cluttered when I reassign a variable in a loop
 * TODO: find a better way to implement :stopiteration: and :ret:
-}

interpretProgram :: Env -> [Stmt] -> [String] -> ErrorWithIO ()
interpretProgram env program argv = if mainHasArgs program
    then void $ interpret' env (program ++ [E (FuncCall (Var "main") [[List $ map String argv]])])
    else void $ interpret' env (program ++ [E (FuncCall (Var "main") [])])


-- TODO: clean this up
interpretStdLib :: Env -> String -> ErrorWithIO (Env, Expr)
interpretStdLib env input = case eParse toplevelParser input of
        Left err -> throwError err
        Right prog -> interpret' env prog


interpret :: Env -> String -> ErrorWithIO (Env, Expr)
interpret env input = case eParse mainParser input of
        Left err -> throwError err
        Right prog -> interpret' env prog


interpret' :: Env -> [Stmt] -> ErrorWithIO (Env, Expr)
interpret' env stmts = foldlM runUntilBreak (env, undefined) stmts
    where
    runUntilBreak :: (Env, Expr) -> Stmt -> ErrorWithIO (Env, Expr)
    runUntilBreak (e, _) stmt = case stmt of
        Break -> return ((":stopiteration:", Bool True):env, Void)
        _ -> if continueIteration e then eval e stmt else return (e, Void)


applyToNamedList :: Env -> Expr -> Maybe Expr -> ErrorWithIO (Env, Expr)
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


applyToAnonString :: Env -> IndexingListExpr -> String -> Maybe Expr -> ErrorWithIO Expr
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
applyToAnonString _ _ _ _ = throwError "Multiple indexing can be applied only to Lists"


applyToAnonList :: Env -> IndexingListExpr -> IndexedList -> Maybe Expr -> ErrorWithIO Expr
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


applyToStructIndex :: Env -> Env -> Maybe String -> String -> IndexingListExpr -> Maybe Expr -> Maybe (Maybe Expr -> ErrorWithIO (Env, Expr)) -> ErrorWithIO (Env, Expr)
applyToStructIndex env structEnv structName memberName indexingExpr assignedExpr retFunc =
    case lookup memberName structEnv of
        Just (List m) -> do
            -- trace "applyToStructindex: List" (return (env, Void))
            val <- applyToAnonList structEnv indexingExpr m assignedExpr
            if isJust assignedExpr
            then do
                let modifiedStruct' = (memberName, val):structEnv
                let modifiedStruct = Struct $ updateStructFuncEnv modifiedStruct' modifiedStruct'

                if isJust retFunc && isNothing structName
                    then fromJust retFunc $ Just modifiedStruct
                    else return ((fromJust structName, modifiedStruct):env, val)
            else return (env, val)
        Just (String m) -> do
            -- trace "applyToStructindex: String" (return (env, Void))
            val <- applyToAnonString structEnv indexingExpr m assignedExpr
            if isJust assignedExpr
            then do
                let modifiedStruct' = (memberName, val):structEnv
                let modifiedStruct = Struct $ updateStructFuncEnv modifiedStruct' modifiedStruct'

                if isJust retFunc && isNothing structName
                    then fromJust retFunc $ Just modifiedStruct
                    else return ((fromJust structName, modifiedStruct):env, val)
            else
                return (env, val)
        Just _ -> throwError "Only Lists and Strings can be indexed"
        Nothing -> inexistentMember memberName structName


inexistentMember memberName structName = throwError $ "No such member " ++ memberName ++ " in struct" ++ (maybe ("") (\name -> " " ++ name) structName)

applyToStructVar :: Env -> Env -> Maybe String -> String -> Maybe Expr -> Maybe (Maybe Expr -> ErrorWithIO (Env, Expr)) -> ErrorWithIO (Env, Expr)
applyToStructVar env structEnv structName memberName assignedExpr retFunc =
    case lookup memberName structEnv of
        Just m -> if isJust assignedExpr
            then do
                (_, val) <- eval env . E  $ fromJust assignedExpr
                let modifiedStruct' = (memberName, val):structEnv
                let modifiedStruct = Struct $ updateStructFuncEnv modifiedStruct' modifiedStruct'

                if isJust retFunc && isNothing structName
                    then fromJust retFunc $ Just modifiedStruct
                    else return ((fromJust structName, modifiedStruct):env, val)
            else do
                (_, val) <- eval structEnv $ E m
                return (env, val)
        Nothing -> inexistentMember memberName structName


invalidAccess = "Only structs can have their members accessed"


eval :: Env -> Stmt -> ErrorWithIO (Env, Expr)
eval env Break = return (env, Void)
eval env (E Void) = return (env, Void)
eval env (E a@(Int _)) = return (env, a)
eval env (E a@(Float _)) = return (env, a)
eval env (E a@(String _)) = return (env, a)
eval env (E a@(Bool _)) = return (env, a)
eval env (E a@(Struct _)) = return (env, a)
eval env (E (List a)) = do
    x <- mapM (eval env . E) a
    return (env, List $ map snd x)
eval env (E (Var name)) = maybe (throwError $ "Unbound variable name " ++ name)
    (\val -> return (env, val))
    $ lookup name env
eval env (E index@(Index _ _)) = applyToNamedList env index Nothing
eval env (E unExpr@(UnExpr _ _)) = evalUnExpr env unExpr >>= eval env . E
eval env (E binExpr@BinExpr{}) = evalBinExpr env binExpr >>= \(e,v)-> eval e $ E v
eval env (Ret expr) = eval env (E expr) >>= \(e, val) -> return ((":ret:", val):e, val) -- I can do this since Ret may appear only inside functions and the environment is not propagated outside the functions
eval env (Assign (Var name) s) = do
    (newEnv, val) <- eval env $ E s
    return ((name,val) : newEnv, val)
eval env (Assign index@(Index _ _) s) = do
    (_, val) <- eval env $ E s
    applyToNamedList env index $ Just val
eval env (Assign (BinExpr MemberAccess x y) assignedExpr) =
    case x of
        Var structName ->
            case lookup structName env of
                Just (Struct sEnv) ->
                    case y of
                        Index memberName indexingExpr ->
                            applyToStructIndex env sEnv (Just structName) memberName indexingExpr (Just assignedExpr) Nothing
                        Var memberName ->
                            applyToStructVar env sEnv (Just structName) memberName (Just assignedExpr) Nothing
                        _ -> throwError tempMemberErr
                Nothing -> throwError $ "Struct " ++ structName ++ "not found"
                _ -> throwError invalidAccess
        index@Index{} -> do
            (_, lst) <- applyToNamedList env index Nothing
            case lst of
                Struct sEnv ->
                    case y of
                        Index memberName indexingExpr ->
                             applyToStructIndex env sEnv Nothing memberName indexingExpr (Just assignedExpr) $ Just (applyToNamedList env index)
                        Var memberName ->
                            applyToStructVar env sEnv Nothing memberName (Just assignedExpr) $ Just (applyToNamedList env index)
                        _ -> throwError tempMemberErr
                _ -> throwError invalidAccess
        _ -> throwError "You cannot assign to temporary objects"
    where
        tempMemberErr = "Cannot assign to temporary member in struct"
eval env (SimpleIf cond stmts) = do
    (newEnv, res) <- eval env (E cond)
    case res of
        Bool val -> if val
            then evalStmtBody newEnv stmts
            else return (newEnv, Void)
        _ -> throwError "An If's condition should evaluate to Bool"
eval env (CompleteIf cond trueStmts falseStmts) = do
    (newEnv, res) <- eval env (E cond)
    case res of
        Bool val -> evalStmtBody newEnv (if val then trueStmts else falseStmts)
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
eval env (TypeDef name body) =
    case lookup name env of
        Just _ -> throwError $ "Cannot declare type " ++ name ++ " since it will shadow other names"
        Nothing -> do
            structEnv <- structToEnv env body
            return ((name, Func [] [Ret $ Struct structEnv] env):env, Void)
eval env (E (FuncDef "" args body)) = return (env, Func args body env)
eval env (E (FuncDef name args body)) = case lookup name env of
    Nothing -> return ((name, Func args body env):env, Func args body env)
    Just _ -> throwError $ "The function name \"" ++ name ++ "\" shadows another name in the current scope"
eval env (E (FuncCall nameExpr args)) = eval env (E nameExpr) >>= \(e, f) ->
    case f of
        Func{} -> applyFunc e f args -- >>= return . (e,)
        BuiltinIOFunc primitive -> mapM (getEvaledExprList env) args >>=
            primitive >>= \val ->
            return (e, val)
        BuiltinFunc primitive -> mapM (getEvaledExprList env) args >>= \a ->
            case primitive a of
                Left err -> throwError err
                Right val -> return val
                >>= \val -> return (e, val)
        _ -> throwError "Only functions are callable"


applyFunc :: Env -> Expr -> [[Expr]] -> ErrorWithIO (Env, Expr)
applyFunc env (Func _ body _) [] = evalFuncBody env body
applyFunc env (Func argNames body closure) [args] =
    getEvaledExprList env args >>=
    argsToEnv argNames >>= \e ->
    evalFuncBody (e++closure++env) body
applyFunc env (Func argNames body closure) (arg:args) =
    getEvaledExprList env arg >>=
    argsToEnv argNames >>= \e ->
    evalFuncBody (e ++ closure ++ env) body >>= \(innerEnv, Func innerArgNames innerBody innerClosure) ->
    eval (innerClosure ++ innerEnv ++ env) (E (FuncCall (FuncDef "" innerArgNames innerBody) args))


evalFuncBody :: Env -> [Stmt] -> ErrorWithIO (Env, Expr)
evalFuncBody env (Ret expr:_) = eval env (E expr)
evalFuncBody env [stmt] = eval env stmt >>= \(e, val) -> case lookup ":ret:" e of
    Nothing -> return (e, val)
    Just v -> return (e, v)
evalFuncBody env (stmt:stmts) = eval env stmt >>= \(e, _) -> case lookup ":ret:" e of
    Nothing -> evalFuncBody e stmts
    Just v -> return (e, v)


argsToEnv :: [String] -> [Expr] -> ErrorWithIO Env
argsToEnv argNames args = if length argNames == length args
    then return $ zip argNames args
    else throwError $ "Trying to pass " ++ show (length args) ++ " args to a function that takes " ++ show (length argNames)


repeatWhile :: Env -> Expr -> [Stmt] -> ErrorWithIO (Env, Expr)
repeatWhile env cond stmts = do
    (newEnv, res) <- eval env (E cond)
    case res of
        Bool val -> if val && continueIteration env
            then liftM fst (evalStmtBody newEnv stmts) >>= (\e -> repeatWhile e cond stmts)
            else return ((":stopiteration:",Bool False):newEnv, Void)
        _ -> throwError "A loop's condition should evaluate to Bool"


evalStmtBody :: Env -> [Stmt] -> ErrorWithIO (Env, Expr)
evalStmtBody env stmts = liftM fst (interpret' env stmts) >>= (\e -> return (e, Void))


getEvaledExprList :: Env -> [Expr] -> ErrorWithIO [Expr]
getEvaledExprList env l = do
    a' <- mapM (eval env . E) l
    return $ map snd a'


evalBinExpr :: Env -> Expr -> ErrorWithIO (Env, Expr)
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
    (liftM and . sequence $ zipWith (lt env) a b) >>= \v -> return (env, Bool v)
evalBinExpr _ (BinExpr Lt _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Lt (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Le (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    (liftM and . sequence $ zipWith (le env) a b) >>= \v -> return (env, Bool v)
evalBinExpr _ (BinExpr Le _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Le (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Ge (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    (liftM and . sequence $ zipWith (ge env) a b) >>= \v -> return (env, Bool v)
evalBinExpr _ (BinExpr Ge _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Ge (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Gt (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    (liftM and . sequence $ zipWith (gt env) a b) >>= \v -> return (env, Bool v)
evalBinExpr _ (BinExpr Gt _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Gt (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Neq (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    (liftM and . sequence $ zipWith (neq env) a b) >>= \v -> return (env, Bool v)
evalBinExpr env (BinExpr Neq (Bool _) (List _)) = return (env, Bool True)
evalBinExpr env (BinExpr Neq (List _) (Bool _)) = return (env, Bool True)
evalBinExpr _ (BinExpr Neq _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Neq (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Eq (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    (liftM and . sequence $ zipWith (eq env) a b) >>= \v -> return (env, Bool v)
evalBinExpr env (BinExpr Eq (Bool _) (List _)) = return (env, Bool False)
evalBinExpr env (BinExpr Eq (List _) (Bool _)) = return (env, Bool False)
evalBinExpr _ (BinExpr Eq _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr _ (BinExpr Eq (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr env (BinExpr Plus (List l) (List r)) = do
    a <- getEvaledExprList env l
    b <- getEvaledExprList env r
    return (env, List $ a ++ [List b])
evalBinExpr env (BinExpr Plus l (List r)) = do
     (_, val) <- eval env $ E l
     case val of
         List lst -> return (env, List $ lst ++ [List r])
         _ -> return (env, List $ val : r)
evalBinExpr env (BinExpr Plus (List l) r) = do
     (_, val) <- eval env $ E r
     case val of
        List lst -> return (env, List $ l ++ [List lst])
        _ -> return (env, List $ l ++ [val])
evalBinExpr _ (BinExpr Minus _ (List _)) = throwError "Cannot subtract a list from a value"
evalBinExpr env (BinExpr Minus (List l) r) = do
    (_, val) <- eval env (E r)
    lst <- getEvaledExprList env l
    filterM (neq env val) lst >>= \v -> return (env, List v)

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
evalBinExpr env (BinExpr And l r) = do
    (_, a) <- eval env $ E l
    case a of
        Bool v1 -> if v1
            then do
                (_, b) <- eval env $ E r
                case b of
                    Bool v2 -> return (env, Bool $ v1 && v2)
                    _ -> throwError "And is valid only on Bools"
            else return (env, Bool v1)
        _ -> throwError "And is valid only on Bools"

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
evalBinExpr env (BinExpr Or l r) = do
    (_, a) <- eval env $ E l
    case a of
        Bool v1 -> if v1
            then return (env, Bool v1)
            else do
                (_, b) <- eval env $ E r
                case b of
                    Bool v2 -> return (env, Bool $ v1 || v2)
                    _ -> throwError "Or is valid only on Bools"
        _ -> throwError "Or is valid only on Bools"

evalBinExpr env (BinExpr Plus (Int l) (Int r)) = return (env, Int $ l + r)
evalBinExpr env (BinExpr Plus (Int l) (Float r)) = return (env, Float $ fromIntegral l + r)
evalBinExpr env (BinExpr Plus (Float l) (Int r)) = return (env, Float $ l + fromIntegral r)
evalBinExpr env (BinExpr Plus (String l) (Int r)) = return (env, String $ l ++ show r)
evalBinExpr env (BinExpr Plus (Int l) (String r)) = return (env, String $ show l ++ r)
evalBinExpr _ (BinExpr Plus (Bool _) (Int _)) = throwError "Cannot add together Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Plus (Int _) (Bool _)) = throwError "Cannot add together Int and Bool" -- FIXME: translate
evalBinExpr env (BinExpr Plus (Float l) (Float r)) = return (env, Float $ l + r)
evalBinExpr env (BinExpr Plus (String l) (Float r)) = return (env, String $ l ++ show r)
evalBinExpr env (BinExpr Plus (Float l) (String r)) = return (env, String $ show l ++ r)
evalBinExpr _ (BinExpr Plus (Bool _) (Float _)) = throwError "Cannot add together Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Plus (Float _) (Bool _)) = throwError "Cannot add together Float and Bool" -- FIXME: translate
evalBinExpr env (BinExpr Plus (String l) (String r)) = return (env, String $ l ++ r)
evalBinExpr env (BinExpr Plus (Bool l) (String r)) = return (env, String $ (if l then tTrue else tFalse) ++ r)
evalBinExpr env (BinExpr Plus (String l) (Bool r)) = return (env, String $ l ++ if r then tTrue else tFalse)
evalBinExpr _ (BinExpr Plus (Bool _) (Bool _)) = throwError "Cannot add together Bools" -- FIXME: translate

evalBinExpr env (BinExpr Minus (Int l) (Int r)) = return (env, Int $ l - r)
evalBinExpr env (BinExpr Minus (Int l) (Float r)) = return (env, Float $ fromIntegral l - r)
evalBinExpr env (BinExpr Minus (Float l) (Int r)) = return (env, Float $ l - fromIntegral r)
evalBinExpr _ (BinExpr Minus (String _) (Int _)) = throwError "Cannot subtract Int from String" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Int _) (String _)) = throwError "Cannot subtract String from Int" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Bool _) (Int _)) = throwError "Cannot subtract Int from Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Int _) (Bool _)) = throwError "Cannot subtract Bool from Int" -- FIXME: translate
evalBinExpr env (BinExpr Minus (Float l) (Float r)) = return (env, Float $ l - r)
evalBinExpr _ (BinExpr Minus (String _) (Float _)) = throwError "Cannot subtract Float from String" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Float _) (String _)) = throwError "Cannot subtract String from Float" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Bool _) (Float _)) = throwError "Cannot subtract Float from Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Float _) (Bool _)) = throwError "Cannot subtract Bool from Float" -- FIXME: translate
evalBinExpr env (BinExpr Minus (String l) (String r)) = return (env, String $ l \\ r)
evalBinExpr _ (BinExpr Minus (Bool _) (String _)) = throwError "Cannot subtract String from Bool"
evalBinExpr _ (BinExpr Minus (String _) (Bool _)) = throwError "Cannot subtract Bool from String"
evalBinExpr _ (BinExpr Minus (Bool _) (Bool _)) = throwError "Cannot subtract Bools" -- FIXME: translate

evalBinExpr env (BinExpr Mul (Int l) (Int r)) = return (env, Int $ l * r)
evalBinExpr env (BinExpr Mul (Int l) (Float r)) = return (env, Float $ fromIntegral l * r)
evalBinExpr env (BinExpr Mul (Float l) (Int r)) = return (env, Float $ l * fromIntegral r)
evalBinExpr _ (BinExpr Mul (String _) (Int _)) = throwError "Cannot multiply String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Int _) (String _)) = throwError "Cannot multiply Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Bool _) (Int _)) = throwError "Cannot multiply Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Int _) (Bool _)) = throwError "Cannot multiply Int and Bool" -- FIXME: translate
evalBinExpr env (BinExpr Mul (Float l) (Float r)) = return (env, Float $ l * r)
evalBinExpr _ (BinExpr Mul (String _) (Float _)) = throwError "Cannot multiply String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Float _) (String _)) = throwError "Cannot multiply Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Bool _) (Float _)) = throwError "Cannot multiply Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Float _) (Bool _)) = throwError "Cannot multiply Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (String _) (String _)) = throwError "Cannot multiply Strings"
evalBinExpr _ (BinExpr Mul (Bool _) (String _)) = throwError "Cannot multiply Bool and String"
evalBinExpr _ (BinExpr Mul (String _) (Bool _)) = throwError "Cannot multiply String and Bool"
evalBinExpr _ (BinExpr Mul (Bool _) (Bool _)) = throwError "Cannot multiply Bools" -- FIXME: translate

evalBinExpr env (BinExpr Div (Int l) (Int r)) = return (env, Float $ fromIntegral l / fromIntegral r)
evalBinExpr env (BinExpr Div (Int l) (Float r)) = return (env, Float $ fromIntegral l / r)
evalBinExpr env (BinExpr Div (Float l) (Int r)) = return (env, Float $ l / fromIntegral r)
evalBinExpr _ (BinExpr Div (String _) (Int _)) = throwError "Cannot divide String by Int" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Int _) (String _)) = throwError "Cannot divide Int by String" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Bool _) (Int _)) = throwError "Cannot divide Bool by Int" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Int _) (Bool _)) = throwError "Cannot divide Int by Bool" -- FIXME: translate
evalBinExpr env (BinExpr Div (Float l) (Float r)) = return (env, Float $ l / r)
evalBinExpr _ (BinExpr Div (String _) (Float _)) = throwError "Cannot divide String by Float" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Float _) (String _)) = throwError "Cannot divide Float by String" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Bool _) (Float _)) = throwError "Cannot divide Bool by Float" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Float _) (Bool _)) = throwError "Cannot divide Float by Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Div (String _) (String _)) = throwError "Cannot divide Strings"
evalBinExpr _ (BinExpr Div (Bool _) (String _)) = throwError "Cannot divide Bool by String"
evalBinExpr _ (BinExpr Div (String _) (Bool _)) = throwError "Cannot divide String by Bool"
evalBinExpr _ (BinExpr Div (Bool _) (Bool _)) = throwError "Cannot divide Bools" -- FIXME: translate

evalBinExpr env (BinExpr Mod (Int l) (Int r)) = return (env, Int $ l `mod` r)
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

evalBinExpr env (BinExpr Pow (Int l) (Int r)) = return (env, Float $ fromInteger l ** fromInteger r)
evalBinExpr env (BinExpr Pow (Int l) (Float r)) = return (env, Float $ fromInteger l ** r)
evalBinExpr env (BinExpr Pow (Float l) (Int r)) = return (env, Float $ l ** fromInteger r)
evalBinExpr _ (BinExpr Pow (String _) (Int _)) = throwError "Cannot raise String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Int _) (String _)) = throwError "Cannot raise Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Bool _) (Int _)) = throwError "Cannot raise Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Int _) (Bool _)) = throwError "Cannot raise Int and Bool" -- FIXME: translate
evalBinExpr env (BinExpr Pow (Float l) (Float r)) = return (env, Float $ l ** r)
evalBinExpr _ (BinExpr Pow (String _) (Float _)) = throwError "Cannot raise String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Float _) (String _)) = throwError "Cannot raise Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Bool _) (Float _)) = throwError "Cannot raise Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Float _) (Bool _)) = throwError "Cannot raise Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (String _) (String _)) = throwError "Cannot raise Strings"
evalBinExpr _ (BinExpr Pow (Bool _) (String _)) = throwError "Cannot raise Bool and String"
evalBinExpr _ (BinExpr Pow (String _) (Bool _)) = throwError "Cannot raise String and Bool"
evalBinExpr _ (BinExpr Pow (Bool _) (Bool _)) = throwError "Cannot raise Bools" -- FIXME: translate

evalBinExpr env (BinExpr Lt a b) = do
    (_, l) <- eval env $ E a
    (_, r) <- eval env $ E b
    v <- lt env l r
    return (env, Bool v)
evalBinExpr env (BinExpr Le a b) = do
    (_, l) <- eval env $ E a
    (_, r) <- eval env $ E b
    v <- le env l r
    return (env, Bool v)
evalBinExpr env (BinExpr Ge a b) = do
    (_, l) <- eval env $ E a
    (_, r) <- eval env $ E b
    v <- ge env l r
    return (env, Bool v)
evalBinExpr env (BinExpr Gt a b) = do
    (_, l) <- eval env $ E a
    (_, r) <- eval env $ E b
    v <- gt env l r
    return (env, Bool v)
evalBinExpr env (BinExpr Neq a b) = do
    (_, l) <- eval env $ E a
    (_, r) <- eval env $ E b
    v <- neq env l r
    return (env, Bool v)
evalBinExpr env (BinExpr Eq a b) = do
    (_, l) <- eval env $ E a
    (_, r) <- eval env $ E b
    v <- eq env l r
    return (env, Bool v)

--TODO: a[1].foo()
--TODO: a.x.y = 1
--TODO: test with a.foo() = 3
--TODO: test with a.foo = 3 where foo is a function
evalBinExpr env (BinExpr MemberAccess x y) = do
    -- trace ("Member Access: " ++ show x ++ " : " ++ show y) (return Void)
    case x of
    -- TODO: here I can work with Indexes too, because I know the name
        Var structName -> do
            -- trace "struct VAR" (return Void)
            case lookup structName env of
                Just (Struct sEnv) -> --TODO: here I might not find the struct
                    case y of
                        Index memberName indexingExpr -> do
                            -- trace "V: member INDEX" (return Void)
                            applyToStructIndex env sEnv Nothing memberName indexingExpr Nothing Nothing
                        Var memberName -> do
                            -- trace "V: member VAR" (return Void)
                            applyToStructVar env sEnv (Just structName) memberName Nothing Nothing
                        FuncCall (Var memberName) args -> do
                            -- trace "V: member FUNC" (return Void)
                            case lookup memberName sEnv of
                                Just f -> do
                                    (modifiedStruct,v) <- applyFunc sEnv f args
                                    return ((structName, Struct $ updateStructFuncEnv modifiedStruct modifiedStruct):env, v)
                                Nothing -> inexistentMember memberName $ Just structName
                        _ -> throwError invalidMember
                _ -> throwError invalidAccess
        index@Index{} -> do
            -- trace "struct INDEX" (return Void)
            (_, lst) <- applyToNamedList env index Nothing
            case lst of
                Struct sEnv ->
                    case y of
                        Index memberName indexingExpr -> do
                            -- trace "I: member INDEX" (return (env,Void))
                            applyToStructIndex env sEnv Nothing memberName indexingExpr Nothing Nothing
                        Var memberName -> do
                            -- trace "I: member VAR" (return (env,Void))
                            applyToStructVar env sEnv Nothing memberName Nothing Nothing
                        FuncCall (Var memberName) args -> do
                            -- trace "I: member FUNC" (return (env,Void))
                            case lookup memberName sEnv of
                                Just f -> do
                                    -- (_, v) <- applyFunc sEnv f args
                                    -- return (env, v)
                                    -- TODO: here I can propagate the change in the list
                                    (modifiedStruct, _) <- applyFunc sEnv f args
                                    applyToNamedList env index . Just . Struct $ updateStructFuncEnv modifiedStruct modifiedStruct
                                Nothing -> inexistentMember memberName Nothing
                        _ -> throwError invalidMember
                _ -> throwError invalidAccess
        _ -> do
            -- trace "struct GENERAL" (return Void)
            (e1, val) <- eval env $ E x
            case val of
                Struct sEnv ->
                    case y of
                        Index memberName indexingExpr -> do
                            -- trace "G: member INDEX" (return (env,Void))
                            applyToStructIndex env sEnv Nothing memberName indexingExpr Nothing Nothing
                        Var memberName -> do
                            -- trace "G: member VAR" (return (env,Void))
                            applyToStructVar env sEnv Nothing memberName Nothing Nothing
                        FuncCall (Var memberName) args -> do
                            -- trace "G: member FUNC" (return (env,Void))
                            case lookup memberName sEnv of
                                Just f -> do
                                    (modifiedStruct, v) <- applyFunc sEnv f args
                                    --return (env++e1++foo, v)
                                    updateGeneralStruct env x $ updateStructFuncEnv modifiedStruct modifiedStruct
                                Nothing -> inexistentMember memberName Nothing
                        _ -> throwError invalidMember
                _ -> throwError invalidAccess
    where invalidMember = "Struct members can only be variables, lists and functions"

evalBinExpr env (BinExpr op l r) = do
    (_, a) <- eval env $ E l
    (_, b) <- eval env $ E r
    evalBinExpr env $ BinExpr op a b

--TODO: cleanup
updateGeneralStruct :: Env -> Expr -> Env -> ErrorWithIO (Env, Expr)
updateGeneralStruct initialEnv index@Index{} newVal =
    applyToNamedList initialEnv index Nothing >>= \(env, xVal) ->
        case xVal of
            Struct s -> applyToNamedList initialEnv index $ Just $ Struct newVal
                -- >>= return (, s) RETURN THE MODIFIED STRUCT elems[1]
            _ -> throwError "not a struct you want to modify2"
updateGeneralStruct initialEnv (BinExpr MemberAccess x y) newVal = do
    case x of
        index@Index{} -> do
            applyToNamedList initialEnv index Nothing >>= \(env, xVal) ->
                case xVal of
                    Struct s -> updateGeneralStruct s y newVal >>= \(e, nVal) ->
                        applyToNamedList initialEnv index $ Just $ Struct e

                    _ -> throwError "not a struct you want to modify"
        _ -> throwError "x not INDEX"

updateStructFuncEnv :: Env -> Env -> Env
updateStructFuncEnv [(name, Func args body _)] newEnv = [(name, Func args body newEnv)]
updateStructFuncEnv [e] _ = [e]
updateStructFuncEnv (e:es) newEnv = (updateStructFuncEnv [e] newEnv) ++ (updateStructFuncEnv es newEnv)

evalUnExpr :: Env-> Expr -> ErrorWithIO Expr
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


lt :: Env -> Expr -> Expr -> ErrorWithIO Bool
lt _ (Int l) (Int r) = return $ l < r
lt _ (Int l) (Float r) = return $ fromInteger l < r
lt _ (Float l) (Int r) = return $ l < fromInteger r
lt _ (String _) (Int _) = throwError "Cannot compare String and Int" -- FIXME: translate
lt _ (Int _) (String _) = throwError "Cannot compare Int and String" -- FIXME: translate
lt _ (Bool _) (Int _) = throwError "Cannot compare Bool and Int" -- FIXME: translate
lt _ (Int _) (Bool _) = throwError "Cannot compare Int and Bool" -- FIXME: translate
lt _ (Float l) (Float r) = return $ l < r
lt _ (String _) (Float _) = throwError "Cannot compare String and Float" -- FIXME: translate
lt _ (Float _) (String _) = throwError "Cannot compare Float and String" -- FIXME: translate
lt _ (Bool _) (Float _) = throwError "Cannot compare Bool and Float" -- FIXME: translate
lt _ (Float _) (Bool _) = throwError "Cannot compare Float and Bool" -- FIXME: translate
lt _ (String l) (String r) = return $ l < r
lt _ (Bool _) (String _) = throwError "Cannot compare Bool and String"
lt _ (String _) (Bool _) = throwError "Cannot compare String and Bool"
lt _ (Bool _) (Bool _) = throwError "Cannot compare Bools" -- FIXME: translate
lt env a b = do
     (_, l) <- eval env $ E a
     (_, r) <- eval env $ E b
     eval env (E (BinExpr Lt l r)) >>= \(_, res) -> case res of
        Bool val -> return val


le :: Env -> Expr -> Expr -> ErrorWithIO Bool
le _ (Int l) (Int r) = return $ l <= r
le _ (Int l) (Float r) = return $ fromInteger l <= r
le _ (Float l) (Int r) = return $ l <= fromInteger r
le _ (String _) (Int _) = throwError "Cannot compare String and Int" -- FIXME: translate
le _ (Int _) (String _) = throwError "Cannot compare Int and String" -- FIXME: translate
le _ (Bool _) (Int _) = throwError "Cannot compare Bool and Int" -- FIXME: translate
le _ (Int _) (Bool _) = throwError "Cannot compare Int and Bool" -- FIXME: translate
le _ (Float l) (Float r) = return $ l <= r
le _ (String _) (Float _) = throwError "Cannot compare String and Float" -- FIXME: translate
le _ (Float _) (String _) = throwError "Cannot compare Float and String" -- FIXME: translate
le _ (Bool _) (Float _) = throwError "Cannot compare Bool and Float" -- FIXME: translate
le _ (Float _) (Bool _) = throwError "Cannot compare Float and Bool" -- FIXME: translate
le _ (String l) (String r) = return $ l <= r
le _ (Bool _) (String _) = throwError "Cannot compare Bool and String"
le _ (String _) (Bool _) = throwError "Cannot compare String and Bool"
le _ (Bool _) (Bool _) = throwError "Cannot compare Bools" -- FIXME: translate
le env a b = do
     (_, l) <- eval env $ E a
     (_, r) <- eval env $ E b
     eval env (E (BinExpr Le l r)) >>= \(_, res) -> case res of
        Bool val -> return val


ge :: Env -> Expr -> Expr -> ErrorWithIO Bool
ge _ (Int l) (Int r) = return $ l >= r
ge _ (Int l) (Float r) = return $ fromInteger l >= r
ge _ (Float l) (Int r) = return $ l >= fromInteger r
ge _ (String _) (Int _) = throwError "Cannot compare String and Int" -- FIXME: translate
ge _ (Int _) (String _) = throwError "Cannot compare Int and String" -- FIXME: translate
ge _ (Bool _) (Int _) = throwError "Cannot compare Bool and Int" -- FIXME: translate
ge _ (Int _) (Bool _) = throwError "Cannot compare Int and Bool" -- FIXME: translate
ge _ (Float l) (Float r) = return $ l >= r
ge _ (String _) (Float _) = throwError "Cannot compare String and Float" -- FIXME: translate
ge _ (Float _) (String _) = throwError "Cannot compare Float and String" -- FIXME: translate
ge _ (Bool _) (Float _) = throwError "Cannot compare Bool and Float" -- FIXME: translate
ge _ (Float _) (Bool _) = throwError "Cannot compare Float and Bool" -- FIXME: translate
ge _ (String l) (String r) = return $ l >= r
ge _ (Bool _) (String _) = throwError "Cannot compare Bool and String"
ge _ (String _) (Bool _) = throwError "Cannot compare String and Bool"
ge _ (Bool _) (Bool _) = throwError "Cannot compare Bools" -- FIXME: translate
ge env a b = do
     (_, l) <- eval env $ E a
     (_, r) <- eval env $ E b
     eval env (E (BinExpr Ge l r)) >>= \(_, res) -> case res of
        Bool val -> return val


gt :: Env -> Expr -> Expr -> ErrorWithIO Bool
gt _ (Int l) (Int r) = return $ l > r
gt _ (Int l) (Float r) = return $ fromInteger l > r
gt _ (Float l) (Int r) = return $ l > fromInteger r
gt _ (String _) (Int _) = throwError "Cannot compare String and Int" -- FIXME: translate
gt _ (Int _) (String _) = throwError "Cannot compare Int and String" -- FIXME: translate
gt _ (Bool _) (Int _) = throwError "Cannot compare Bool and Int" -- FIXME: translate
gt _ (Int _) (Bool _) = throwError "Cannot compare Int and Bool" -- FIXME: translate
gt _ (Float l) (Float r) = return $ l > r
gt _ (String _) (Float _) = throwError "Cannot compare String and Float" -- FIXME: translate
gt _ (Float _) (String _) = throwError "Cannot compare Float and String" -- FIXME: translate
gt _ (Bool _) (Float _) = throwError "Cannot compare Bool and Float" -- FIXME: translate
gt _ (Float _) (Bool _) = throwError "Cannot compare Float and Bool" -- FIXME: translate
gt _ (String l) (String r) = return $ l > r
gt _ (Bool _) (String _) = throwError "Cannot compare Bool and String"
gt _ (String _) (Bool _) = throwError "Cannot compare String and Bool"
gt _ (Bool _) (Bool _) = throwError "Cannot compare Bools" -- FIXME: translate
gt env a b = do
     (_, l) <- eval env $ E a
     (_, r) <- eval env $ E b
     eval env (E (BinExpr Gt l r)) >>= \(_, res) -> case res of
        Bool val -> return val


neq :: Env -> Expr -> Expr -> ErrorWithIO Bool
neq _ (Int l) (Int r) = return $ l /= r
neq _ (Int l) (Float r) = return $ fromInteger l /= r
neq _ (Float l) (Int r) = return $ l /= fromInteger r
neq _ (String _) (Int _) = throwError "Cannot compare String and Int" -- FIXME: translate
neq _ (Int _) (String _) = throwError "Cannot compare Int and String" -- FIXME: translate
neq _ (Bool _) (Int _) = return True
neq _ (Int _) (Bool _) = return True
neq _ (Float l) (Float r) = return $ l /= r
neq _ (String _) (Float _) = throwError "Cannot compare String and Float" -- FIXME: translate
neq _ (Float _) (String _) = throwError "Cannot compare Float and String" -- FIXME: translate
neq _ (Bool _) (Float _) = return True
neq _ (Float _) (Bool _) = return True
neq _ (String l) (String r) = return $ l /= r
neq _ (Bool _) (String _) = return True
neq _ (String _) (Bool _) = return True
neq _ (Bool l) (Bool r) = return $ l /= r
neq env a b = do
     (_, l) <- eval env $ E a
     (_, r) <- eval env $ E b
     eval env (E (BinExpr Neq l r)) >>= \(_, res) -> case res of
        Bool val -> return val


eq :: Env -> Expr -> Expr -> ErrorWithIO Bool
eq _ (Int l) (Int r) = return $ l == r
eq _ (Int l) (Float r) = return $ fromInteger l == r
eq _ (Float l) (Int r) = return $ l == fromInteger r
eq _ (String _) (Int _) = throwError "Cannot compare String and Int" -- FIXME: translate
eq _ (Int _) (String _) = throwError "Cannot compare Int and String" -- FIXME: translate
eq _ (Bool _) (Int _) = return False
eq _ (Int _) (Bool _) = return False
eq _ (Float l) (Float r) = return $ l == r
eq _ (String _) (Float _) = throwError "Cannot compare String and Float" -- FIXME: translate
eq _ (Float _) (String _) = throwError "Cannot compare Float and String" -- FIXME: translate
eq _ (Bool _) (Float _) = return False
eq _ (Float _) (Bool _) = return False
eq _ (String l) (String r) = return $ l == r
eq _ (Bool _) (String _) = return False
eq _ (String _) (Bool _) = return False
eq _ (Bool l) (Bool r) = return $ l == r
eq env a b = do
     (_, l) <- eval env $ E a
     (_, r) <- eval env $ E b
     eval env (E (BinExpr Eq l r)) >>= \(_, res) -> case res of
        Bool val -> return val


structToEnv :: Env -> [Stmt] -> ErrorWithIO Env
structToEnv env [a@Assign{}] = do
    (e, _) <- eval env a
    return e
structToEnv env [f@(E FuncDef{})] = do
    (e, _) <- eval env f
    return e
structToEnv env (stmt:stmts) = do
    e <- structToEnv env [stmt]
    structToEnv (e++env) stmts

-- TODO: collision fo scrie (from stdlib) with foo.scrie()
-- TODO: check that newer members actually have access to older members inside the struct itself
-- TODO: constructor fucntions for user defined are overwriteable
