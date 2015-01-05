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
{-
TODO: plus pe liste
epc> a={1, 2,3}
ans:
{1, 2, 3}
epc> a+ 3
ans:
{1, 2, 3, 3}
epc> a+ {4,5}
ans:
{{1, 2, 3}, 4, 5}

-}


--TODO: the (Int 42) Expr could be replaced by an Apophasis type that is not displayed and when matched against, throws an error because a Stmt was used in an Expr
interpret :: Env -> String -> Error (Env, Expr)
interpret env input = eParse mainParser input >>= foldlM (\(e, exr) stmt -> eval e stmt) (env, Int 42)

interpret' :: Env -> [Stmt] -> Error (Env, Expr)
interpret' env stmts = foldlM (\(e, exr) stmt -> eval e stmt) (env, Int 42) stmts

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
                otherwise -> throwError "List can be indexed only with Integer evaluating expressions"
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
                            otherwise -> throwError "Only Lists and Strings can be indexed"

                    else
                        throwError $ invalidListIndex name i
                otherwise -> throwError "List can be indexed only with Integer evaluating expressions"


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
        otherwise -> throwError "List can be indexed only with Integer evaluating expressions"
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
                    otherwise -> throwError "Only Lists and Strings can be indexed"
            else
                throwError $ invalidNestedListIndex i
        otherwise -> throwError "List can be indexed only with Integer evaluating expressions"


eval :: Env -> Stmt -> Error (Env, Expr)
--TODO: everything in the AST, both Expr and Stmt
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
eval env (Assign (Var name) s) = do
    (newEnv, val) <- eval env $ E s
    return ((name,val) : newEnv, val)
eval env (Assign index@(Index name _) s) = do --TODO: apply to string indexing, too
    (newEnv, val) <- eval env $ E s
    applyToNamedList env index $ Just val
eval env (SimpleIf expr stmts) = case eval env (E expr) of
    Left err -> throwError err
    Right (newEnv, (Bool val)) -> if val then interpret' newEnv stmts else return (newEnv, Bool False) -- TODO: do I really want ifs to return stuff?
    otherwise -> throwError "An If's condition should evaluate to Bool"


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
evalBinExpr env (BinExpr Plus l (List r)) = do
    (_, val) <- eval env $ E l
    return . List $ val : r
evalBinExpr env (BinExpr Plus (List l) r) = do
    (_, val) <- eval env $ E r
    return . List $ l ++ [val]
evalBinExpr _ (BinExpr Minus l (List r)) = throwError "Cannot subtract a list from a value"
evalBinExpr env (BinExpr Minus (List l) r) = do
    (_, val) <- eval env (E r)
    return . List $ filter (/= val) l

evalBinExpr _ (BinExpr And (Int l) (Int r)) = throwError "And is invalid on Ints"
evalBinExpr _ (BinExpr And (Int l) (Float r)) = throwError "And is invalid on Int and Float"
evalBinExpr _ (BinExpr And (Float l) (Int r)) = throwError "And is invalid on Float and Int"
evalBinExpr _ (BinExpr And (String l) (Int r)) = throwError "And is invalid on String and Int"
evalBinExpr _ (BinExpr And (Int l) (String r)) = throwError "And is invalid on Int and String"
evalBinExpr _ (BinExpr And (Bool _) (Int _)) = throwError "And is invalid on Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr And (Int _) (Bool _)) = throwError "And is invalid on Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr And (Float l) (Float r)) = throwError "And is invalid on Floats"
evalBinExpr _ (BinExpr And (String l) (Float r)) = throwError "And is invalid on String and Float"
evalBinExpr _ (BinExpr And (Float l) (String r)) = throwError "And is invalid on Float and String"
evalBinExpr _ (BinExpr And (Bool _) (Float _)) = throwError "And is invalid on Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr And (Float _) (Bool _)) = throwError "And is invalid on Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr And (String l) (String r)) = throwError "And is invalid on Strings"
evalBinExpr _ (BinExpr And (Bool l) (String r)) = throwError "And is invalid on Bool and String"
evalBinExpr _ (BinExpr And (String l) (Bool r)) = throwError "And is invalid on String and Bool"
evalBinExpr _ (BinExpr And (Bool l) (Bool r)) = return . Bool $ l && r

evalBinExpr _ (BinExpr Or (Int l) (Int r)) = throwError "Or is invalid on Ints"
evalBinExpr _ (BinExpr Or (Int l) (Float r)) = throwError "Or is invalid on Int and Float"
evalBinExpr _ (BinExpr Or (Float l) (Int r)) = throwError "Or is invalid on Float and Int"
evalBinExpr _ (BinExpr Or (String l) (Int r)) = throwError "Or is invalid on String and Int"
evalBinExpr _ (BinExpr Or (Int l) (String r)) = throwError "Or is invalid on Int and String"
evalBinExpr _ (BinExpr Or (Bool _) (Int _)) = throwError "Or is invalid on Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Or (Int _) (Bool _)) = throwError "Or is invalid on Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Or (Float l) (Float r)) = throwError "Or is invalid on Floats"
evalBinExpr _ (BinExpr Or (String l) (Float r)) = throwError "Or is invalid on String and Float"
evalBinExpr _ (BinExpr Or (Float l) (String r)) = throwError "Or is invalid on Float and String"
evalBinExpr _ (BinExpr Or (Bool _) (Float _)) = throwError "Or is invalid on Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Or (Float _) (Bool _)) = throwError "Or is invalid on Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Or (String l) (String r)) = throwError "Or is invalid on Strings"
evalBinExpr _ (BinExpr Or (Bool l) (String r)) = throwError "Or is invalid on Bool and String"
evalBinExpr _ (BinExpr Or (String l) (Bool r)) = throwError "Or is invalid on String and Bool"
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
evalBinExpr _ (BinExpr Plus (Bool l) (String r)) = return . String $ if l then tTrue else tFalse ++ r
evalBinExpr _ (BinExpr Plus (String l) (Bool r)) = return . String $ l ++ if r then tTrue else tFalse
evalBinExpr _ (BinExpr Plus (Bool _) (Bool _)) = throwError "Cannot add together Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Minus (Int l) (Int r)) = return . Int $ l - r
evalBinExpr _ (BinExpr Minus (Int l) (Float r)) = return . Float $ fromIntegral l - r
evalBinExpr _ (BinExpr Minus (Float l) (Int r)) = return . Float $ l - fromIntegral r
evalBinExpr _ (BinExpr Minus (String l) (Int r)) = throwError "Cannot subtract Int from String" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Int l) (String r)) = throwError "Cannot subtract String from Int" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Bool _) (Int _)) = throwError "Cannot subtract Int from Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Int _) (Bool _)) = throwError "Cannot subtract Bool from Int" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Float l) (Float r)) = return . Float $ l - r
evalBinExpr _ (BinExpr Minus (String l) (Float r)) = throwError "Cannot subtract Float from String" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Float l) (String r)) = throwError "Cannot subtract String from Float" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Bool _) (Float _)) = throwError "Cannot subtract Float from Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (Float _) (Bool _)) = throwError "Cannot subtract Bool from Float" -- FIXME: translate
evalBinExpr _ (BinExpr Minus (String l) (String r)) = return . String $ l \\ r
evalBinExpr _ (BinExpr Minus (Bool l) (String r)) = throwError "Cannot subtract String from Bool"
evalBinExpr _ (BinExpr Minus (String l) (Bool r)) = throwError "Cannot subtract Bool from String"
evalBinExpr _ (BinExpr Minus (Bool _) (Bool _)) = throwError "Cannot subtract Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Mul (Int l) (Int r)) = return . Int $ l * r
evalBinExpr _ (BinExpr Mul (Int l) (Float r)) = return . Float $ fromIntegral l * r
evalBinExpr _ (BinExpr Mul (Float l) (Int r)) = return . Float $ l * fromIntegral r
evalBinExpr _ (BinExpr Mul (String l) (Int r)) = throwError "Cannot multiply String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Int l) (String r)) = throwError "Cannot multiply Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Bool _) (Int _)) = throwError "Cannot multiply Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Int _) (Bool _)) = throwError "Cannot multiply Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Float l) (Float r)) = return . Float $ l * r
evalBinExpr _ (BinExpr Mul (String l) (Float r)) = throwError "Cannot multiply String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Float l) (String r)) = throwError "Cannot multiply Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Bool _) (Float _)) = throwError "Cannot multiply Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (Float _) (Bool _)) = throwError "Cannot multiply Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mul (String l) (String r)) = return . String $ l \\ r
evalBinExpr _ (BinExpr Mul (Bool l) (String r)) = throwError "Cannot multiply Bool and String"
evalBinExpr _ (BinExpr Mul (String l) (Bool r)) = throwError "Cannot multiply String and Bool"
evalBinExpr _ (BinExpr Mul (Bool _) (Bool _)) = throwError "Cannot multiply Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Div (Int l) (Int r)) = return . Float $ fromIntegral l / fromIntegral r
evalBinExpr _ (BinExpr Div (Int l) (Float r)) = return . Float $ fromIntegral l / r
evalBinExpr _ (BinExpr Div (Float l) (Int r)) = return . Float $ l / fromIntegral r
evalBinExpr _ (BinExpr Div (String l) (Int r)) = throwError "Cannot divide String by Int" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Int l) (String r)) = throwError "Cannot divide Int by String" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Bool _) (Int _)) = throwError "Cannot divide Bool by Int" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Int _) (Bool _)) = throwError "Cannot divide Int by Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Float l) (Float r)) = return . Float $ l / r
evalBinExpr _ (BinExpr Div (String l) (Float r)) = throwError "Cannot divide String by Float" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Float l) (String r)) = throwError "Cannot divide Float by String" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Bool _) (Float _)) = throwError "Cannot divide Bool by Float" -- FIXME: translate
evalBinExpr _ (BinExpr Div (Float _) (Bool _)) = throwError "Cannot divide Float by Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Div (String l) (String r)) = throwError "Cannot divide Strings"
evalBinExpr _ (BinExpr Div (Bool l) (String r)) = throwError "Cannot divide Bool by String"
evalBinExpr _ (BinExpr Div (String l) (Bool r)) = throwError "Cannot divide String by Bool"
evalBinExpr _ (BinExpr Div (Bool _) (Bool _)) = throwError "Cannot divide Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Mod (Int l) (Int r)) = return . Int $ l `mod` r
evalBinExpr _ (BinExpr Mod (Int l) (Float r)) = throwError "Modulo cannot be applied to Int and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Float l) (Int r)) = throwError "Modulo cannot be applied to Float and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (String l) (Int r)) = throwError "Modulo cannot be applied to String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Int l) (String r)) = throwError "Modulo cannot be applied to Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Bool _) (Int _)) = throwError "Modulo cannot be applied to Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Int _) (Bool _)) = throwError "Modulo cannot be applied to Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Float l) (Float r)) = throwError "Modulo cannot be applied to Floats" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (String l) (Float r)) = throwError "Modulo cannot be applied to String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Float l) (String r)) = throwError "Modulo cannot be applied to Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Bool _) (Float _)) = throwError "Modulo cannot be applied to Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (Float _) (Bool _)) = throwError "Modulo cannot be applied to Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Mod (String l) (String r)) = throwError "Modulo cannot be applied to Strings"
evalBinExpr _ (BinExpr Mod (Bool l) (String r)) = throwError "Modulo cannot be applied to Bool and String"
evalBinExpr _ (BinExpr Mod (String l) (Bool r)) = throwError "Modulo cannot be applied to String and Bool"
evalBinExpr _ (BinExpr Mod (Bool _) (Bool _)) = throwError "Modulo cannot be applied to Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Lt (Int l) (Int r)) = return . Bool $ l < r
evalBinExpr _ (BinExpr Lt (Int l) (Float r)) = return . Bool $ fromInteger l < r
evalBinExpr _ (BinExpr Lt (Float l) (Int r)) = return . Bool $ l < fromInteger r
evalBinExpr _ (BinExpr Lt (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Float l) (Float r)) = return . Bool $ l < r
evalBinExpr _ (BinExpr Lt (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Lt (String l) (String r)) = return . Bool $ l < r
evalBinExpr _ (BinExpr Lt (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Lt (String l) (Bool r)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Lt (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Le (Int l) (Int r)) = return . Bool $ l <= r
evalBinExpr _ (BinExpr Le (Int l) (Float r)) = return . Bool $ fromInteger l <= r
evalBinExpr _ (BinExpr Le (Float l) (Int r)) = return . Bool $ l <= fromInteger r
evalBinExpr _ (BinExpr Le (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Float l) (Float r)) = return . Bool $ l <= r
evalBinExpr _ (BinExpr Le (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Le (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Le (String l) (String r)) = return . Bool $ l <= r
evalBinExpr _ (BinExpr Le (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Le (String l) (Bool r)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Le (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Gt (Int l) (Int r)) = return . Bool $ l > r
evalBinExpr _ (BinExpr Gt (Int l) (Float r)) = return . Bool $ fromInteger l > r
evalBinExpr _ (BinExpr Gt (Float l) (Int r)) = return . Bool $ l > fromInteger r
evalBinExpr _ (BinExpr Gt (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Float l) (Float r)) = return . Bool $ l > r
evalBinExpr _ (BinExpr Gt (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Gt (String l) (String r)) = return . Bool $ l > r
evalBinExpr _ (BinExpr Gt (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Gt (String l) (Bool r)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Gt (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Ge (Int l) (Int r)) = return . Bool $ l >= r
evalBinExpr _ (BinExpr Ge (Int l) (Float r)) = return . Bool $ fromInteger l >= r
evalBinExpr _ (BinExpr Ge (Float l) (Int r)) = return . Bool $ l >= fromInteger r
evalBinExpr _ (BinExpr Ge (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Float l) (Float r)) = return . Bool $ l >= r
evalBinExpr _ (BinExpr Ge (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Ge (String l) (String r)) = return . Bool $ l >= r
evalBinExpr _ (BinExpr Ge (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Ge (String l) (Bool r)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Ge (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

evalBinExpr _ (BinExpr Neq (Int l) (Int r)) = return . Bool $ l /= r
evalBinExpr _ (BinExpr Neq (Int l) (Float r)) = return . Bool $ fromInteger l /= r
evalBinExpr _ (BinExpr Neq (Float l) (Int r)) = return . Bool $ l /= fromInteger r
evalBinExpr _ (BinExpr Neq (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Float l) (Float r)) = return . Bool $ l /= r
evalBinExpr _ (BinExpr Neq (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Neq (String l) (String r)) = return . Bool $ l /= r
evalBinExpr _ (BinExpr Neq (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Neq (String l) (Bool r)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Neq (Bool l) (Bool r)) = return . Bool $ l /= r

evalBinExpr _ (BinExpr Eq (Int l) (Int r)) = return . Bool $ l == r
evalBinExpr _ (BinExpr Eq (Int l) (Float r)) = return . Bool $ fromInteger l == r
evalBinExpr _ (BinExpr Eq (Float l) (Int r)) = return . Bool $ l == fromInteger r
evalBinExpr _ (BinExpr Eq (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Float l) (Float r)) = return . Bool $ l == r
evalBinExpr _ (BinExpr Eq (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Eq (String l) (String r)) = return . Bool $ l == r
evalBinExpr _ (BinExpr Eq (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr _ (BinExpr Eq (String l) (Bool r)) = throwError "Cannot compare String and Bool"
evalBinExpr _ (BinExpr Eq (Bool l) (Bool r)) = return . Bool $ l == r

evalBinExpr _ (BinExpr Pow (Int l) (Int r)) = return . Float $ fromInteger l ** fromInteger r
evalBinExpr _ (BinExpr Pow (Int l) (Float r)) = return . Float $ fromInteger l ** r
evalBinExpr _ (BinExpr Pow (Float l) (Int r)) = return . Float $ l ** fromInteger r
evalBinExpr _ (BinExpr Pow (String l) (Int r)) = throwError "Cannot raise String and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Int l) (String r)) = throwError "Cannot raise Int and String" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Bool _) (Int _)) = throwError "Cannot raise Bool and Int" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Int _) (Bool _)) = throwError "Cannot raise Int and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Float l) (Float r)) = return . Float $ l ** r
evalBinExpr _ (BinExpr Pow (String l) (Float r)) = throwError "Cannot raise String and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Float l) (String r)) = throwError "Cannot raise Float and String" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Bool _) (Float _)) = throwError "Cannot raise Bool and Float" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (Float _) (Bool _)) = throwError "Cannot raise Float and Bool" -- FIXME: translate
evalBinExpr _ (BinExpr Pow (String l) (String r)) = throwError "Cannot raise Strings"
evalBinExpr _ (BinExpr Pow (Bool l) (String r)) = throwError "Cannot raise Bool and String"
evalBinExpr _ (BinExpr Pow (String l) (Bool r)) = throwError "Cannot raise String and Bool"
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
