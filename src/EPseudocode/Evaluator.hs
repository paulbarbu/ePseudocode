module EPseudocode.Evaluator (eval)
where

import Data.List ((\\))

import Control.Monad.Except

import EPseudocode.Data
import EPseudocode.Lexer

eval :: Stmt -> Error Expr
--TODO: everythign in the AST, both Expr and Stmt
eval (E a@(Int _)) = return a
eval (E a@(Float _)) = return a
eval (E a@(String _)) = return a
eval (E a@(Bool _)) = return a
eval (E unExpr@(UnExpr _ _)) = evalUnExpr unExpr >>= eval . E
eval (E binExpr@BinExpr{}) = evalBinExpr binExpr >>= eval . E


evalBinExpr :: Expr -> Error Expr
evalBinExpr (BinExpr And _ (List _)) = throwError "Cannot apply and to a list"
evalBinExpr (BinExpr And (List _) _) = throwError "Cannot apply and to a list"
evalBinExpr (BinExpr Or _ (List _)) = throwError "Cannot apply or to a list"
evalBinExpr (BinExpr Or (List _) _) = throwError "Cannot apply or to a list"
evalBinExpr (BinExpr Mul _ (List _)) = throwError "Cannot multiply a list"
evalBinExpr (BinExpr Mul (List _) _) = throwError "Cannot multiply to a list"
evalBinExpr (BinExpr Div _ (List _)) = throwError "Cannot divide to a list"
evalBinExpr (BinExpr Div (List _) _) = throwError "Cannot divide a list"
evalBinExpr (BinExpr Mod _ (List _)) = throwError "Cannot apply mod to a list"
evalBinExpr (BinExpr Mod (List _) _) = throwError "Cannot apply mod to a list"
evalBinExpr (BinExpr Pow _ (List _)) = throwError "Cannot raise to a list power"
evalBinExpr (BinExpr Pow (List _) _) = throwError "Cannot raise a list to power"
evalBinExpr (BinExpr Lt (List l) (List r)) = do
    a <- mapM eval l
    b <- mapM eval r
    return . Bool $ a < b
evalBinExpr (BinExpr Lt _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Lt (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Le (List l) (List r)) = do
    a <- mapM eval l
    b <- mapM eval r
    return . Bool $ a <= b
evalBinExpr (BinExpr Le _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Le (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Ge (List l) (List r)) = do
    a <- mapM eval l
    b <- mapM eval r
    return . Bool $ a >= b
evalBinExpr (BinExpr Ge _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Ge (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Gt (List l) (List r)) = do
    a <- mapM eval l
    b <- mapM eval r
    return . Bool $ a > b
evalBinExpr (BinExpr Gt _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Gt (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Neq (List l) (List r)) = do
    a <- mapM eval l
    b <- mapM eval r
    return . Bool $ a /= b
evalBinExpr (BinExpr Neq _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Neq (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Eq (List l) (List r)) = do
    a <- mapM eval l
    b <- mapM eval r
    return . Bool $ a == b
evalBinExpr (BinExpr Eq _ (List _)) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Eq (List _) _) = throwError "Lists can be compared only to lists"
evalBinExpr (BinExpr Plus l (List r)) = do
    val <- eval (E l)
    return . List $ E val : r
evalBinExpr (BinExpr Plus (List l) r) = do
    val <- eval (E r)
    return . List $ l ++ [E val]

evalBinExpr (BinExpr Minus l (List r)) = throwError "Cannot subtract a list from a value"
evalBinExpr (BinExpr Minus (List l) r) = do
    val <- eval (E r)
    return . List $ filter (\x -> x /= E val) l

    -- Int with all
evalBinExpr (BinExpr And (Int l) (Int r)) = throwError "And is invalid on Ints"
evalBinExpr (BinExpr And (Int l) (Float r)) = throwError "And is invalid on Int and Float"
evalBinExpr (BinExpr And (Float l) (Int r)) = throwError "And is invalid on Float and Int"
evalBinExpr (BinExpr And (String l) (Int r)) = throwError "And is invalid on String and Int"
evalBinExpr (BinExpr And (Int l) (String r)) = throwError "And is invalid on Int and String"
evalBinExpr (BinExpr And (Bool _) (Int _)) = throwError "And is invalid on Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr And (Int _) (Bool _)) = throwError "And is invalid on Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr And (Float l) (Float r)) = throwError "And is invalid on Floats"
evalBinExpr (BinExpr And (String l) (Float r)) = throwError "And is invalid on String and Float"
evalBinExpr (BinExpr And (Float l) (String r)) = throwError "And is invalid on Float and String"
evalBinExpr (BinExpr And (Bool _) (Float _)) = throwError "And is invalid on Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr And (Float _) (Bool _)) = throwError "And is invalid on Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr And (String l) (String r)) = throwError "And is invalid on Strings"
evalBinExpr (BinExpr And (Bool l) (String r)) = throwError "And is invalid on Bool and String"
evalBinExpr (BinExpr And (String l) (Bool r)) = throwError "And is invalid on String and Bool"
    -- Bool with all
evalBinExpr (BinExpr And (Bool l) (Bool r)) = return . Bool $ l && r

    -- Int with all
evalBinExpr (BinExpr Or (Int l) (Int r)) = throwError "Or is invalid on Ints"
evalBinExpr (BinExpr Or (Int l) (Float r)) = throwError "Or is invalid on Int and Float"
evalBinExpr (BinExpr Or (Float l) (Int r)) = throwError "Or is invalid on Float and Int"
evalBinExpr (BinExpr Or (String l) (Int r)) = throwError "Or is invalid on String and Int"
evalBinExpr (BinExpr Or (Int l) (String r)) = throwError "Or is invalid on Int and String"
evalBinExpr (BinExpr Or (Bool _) (Int _)) = throwError "Or is invalid on Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Or (Int _) (Bool _)) = throwError "Or is invalid on Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Or (Float l) (Float r)) = throwError "Or is invalid on Floats"
evalBinExpr (BinExpr Or (String l) (Float r)) = throwError "Or is invalid on String and Float"
evalBinExpr (BinExpr Or (Float l) (String r)) = throwError "Or is invalid on Float and String"
evalBinExpr (BinExpr Or (Bool _) (Float _)) = throwError "Or is invalid on Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Or (Float _) (Bool _)) = throwError "Or is invalid on Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Or (String l) (String r)) = throwError "Or is invalid on Strings"
evalBinExpr (BinExpr Or (Bool l) (String r)) = throwError "Or is invalid on Bool and String"
evalBinExpr (BinExpr Or (String l) (Bool r)) = throwError "Or is invalid on String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Or (Bool l) (Bool r)) = return . Bool $ l || r

    -- Int with all
evalBinExpr (BinExpr Plus (Int l) (Int r)) = return . Int $ l + r
evalBinExpr (BinExpr Plus (Int l) (Float r)) = return . Float $ fromIntegral l + r
evalBinExpr (BinExpr Plus (Float l) (Int r)) = return . Float $ l + fromIntegral r
evalBinExpr (BinExpr Plus (String l) (Int r)) = return . String $ l ++ show r
evalBinExpr (BinExpr Plus (Int l) (String r)) = return . String $ show l ++ r
evalBinExpr (BinExpr Plus (Bool _) (Int _)) = throwError "Cannot add together Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Plus (Int _) (Bool _)) = throwError "Cannot add together Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Plus (Float l) (Float r)) = return . Float $ l + r
evalBinExpr (BinExpr Plus (String l) (Float r)) = return . String $ l ++ show r
evalBinExpr (BinExpr Plus (Float l) (String r)) = return . String $ show l ++ r
evalBinExpr (BinExpr Plus (Bool _) (Float _)) = throwError "Cannot add together Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Plus (Float _) (Bool _)) = throwError "Cannot add together Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Plus (String l) (String r)) = return . String $ l ++ r
evalBinExpr (BinExpr Plus (Bool l) (String r)) = return . String $ if l then tTrue else tFalse ++ r
evalBinExpr (BinExpr Plus (String l) (Bool r)) = return . String $ l ++ if r then tTrue else tFalse
    -- Bool with all
evalBinExpr (BinExpr Plus (Bool _) (Bool _)) = throwError "Cannot add together Bools" -- FIXME: translate

    -- Int with all
evalBinExpr (BinExpr Minus (Int l) (Int r)) = return . Int $ l - r
evalBinExpr (BinExpr Minus (Int l) (Float r)) = return . Float $ fromIntegral l - r
evalBinExpr (BinExpr Minus (Float l) (Int r)) = return . Float $ l - fromIntegral r
evalBinExpr (BinExpr Minus (String l) (Int r)) = throwError "Cannot subtract Int from String" -- FIXME: translate
evalBinExpr (BinExpr Minus (Int l) (String r)) = throwError "Cannot subtract String from Int" -- FIXME: translate
evalBinExpr (BinExpr Minus (Bool _) (Int _)) = throwError "Cannot subtract Int from Bool" -- FIXME: translate
evalBinExpr (BinExpr Minus (Int _) (Bool _)) = throwError "Cannot subtract Bool from Int" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Minus (Float l) (Float r)) = return . Float $ l - r
evalBinExpr (BinExpr Minus (String l) (Float r)) = throwError "Cannot subtract Float from String" -- FIXME: translate
evalBinExpr (BinExpr Minus (Float l) (String r)) = throwError "Cannot subtract String from Float" -- FIXME: translate
evalBinExpr (BinExpr Minus (Bool _) (Float _)) = throwError "Cannot subtract Float from Bool" -- FIXME: translate
evalBinExpr (BinExpr Minus (Float _) (Bool _)) = throwError "Cannot subtract Bool from Float" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Minus (String l) (String r)) = return . String $ l \\ r
evalBinExpr (BinExpr Minus (Bool l) (String r)) = throwError "Cannot subtract String from Bool"
evalBinExpr (BinExpr Minus (String l) (Bool r)) = throwError "Cannot subtract Bool from String"
    -- Bool with all
evalBinExpr (BinExpr Minus (Bool _) (Bool _)) = throwError "Cannot subtract Bools" -- FIXME: translate

    -- Int with all
evalBinExpr (BinExpr Mul (Int l) (Int r)) = return . Int $ l * r
evalBinExpr (BinExpr Mul (Int l) (Float r)) = return . Float $ fromIntegral l * r
evalBinExpr (BinExpr Mul (Float l) (Int r)) = return . Float $ l * fromIntegral r
evalBinExpr (BinExpr Mul (String l) (Int r)) = throwError "Cannot multiply String and Int" -- FIXME: translate
evalBinExpr (BinExpr Mul (Int l) (String r)) = throwError "Cannot multiply Int and String" -- FIXME: translate
evalBinExpr (BinExpr Mul (Bool _) (Int _)) = throwError "Cannot multiply Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Mul (Int _) (Bool _)) = throwError "Cannot multiply Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Mul (Float l) (Float r)) = return . Float $ l * r
evalBinExpr (BinExpr Mul (String l) (Float r)) = throwError "Cannot multiply String and Float" -- FIXME: translate
evalBinExpr (BinExpr Mul (Float l) (String r)) = throwError "Cannot multiply Float and String" -- FIXME: translate
evalBinExpr (BinExpr Mul (Bool _) (Float _)) = throwError "Cannot multiply Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Mul (Float _) (Bool _)) = throwError "Cannot multiply Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Mul (String l) (String r)) = return . String $ l \\ r
evalBinExpr (BinExpr Mul (Bool l) (String r)) = throwError "Cannot multiply Bool and String"
evalBinExpr (BinExpr Mul (String l) (Bool r)) = throwError "Cannot multiply String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Mul (Bool _) (Bool _)) = throwError "Cannot multiply Bools" -- FIXME: translate

-- Int with all
evalBinExpr (BinExpr Div (Int l) (Int r)) = return . Float $ fromIntegral l / fromIntegral r
evalBinExpr (BinExpr Div (Int l) (Float r)) = return . Float $ fromIntegral l / r
evalBinExpr (BinExpr Div (Float l) (Int r)) = return . Float $ l / fromIntegral r
evalBinExpr (BinExpr Div (String l) (Int r)) = throwError "Cannot divide String by Int" -- FIXME: translate
evalBinExpr (BinExpr Div (Int l) (String r)) = throwError "Cannot divide Int by String" -- FIXME: translate
evalBinExpr (BinExpr Div (Bool _) (Int _)) = throwError "Cannot divide Bool by Int" -- FIXME: translate
evalBinExpr (BinExpr Div (Int _) (Bool _)) = throwError "Cannot divide Int by Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Div (Float l) (Float r)) = return . Float $ l / r
evalBinExpr (BinExpr Div (String l) (Float r)) = throwError "Cannot divide String by Float" -- FIXME: translate
evalBinExpr (BinExpr Div (Float l) (String r)) = throwError "Cannot divide Float by String" -- FIXME: translate
evalBinExpr (BinExpr Div (Bool _) (Float _)) = throwError "Cannot divide Bool by Float" -- FIXME: translate
evalBinExpr (BinExpr Div (Float _) (Bool _)) = throwError "Cannot divide Float by Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Div (String l) (String r)) = throwError "Cannot divide Strings"
evalBinExpr (BinExpr Div (Bool l) (String r)) = throwError "Cannot divide Bool by String"
evalBinExpr (BinExpr Div (String l) (Bool r)) = throwError "Cannot divide String by Bool"
    -- Bool with all
evalBinExpr (BinExpr Div (Bool _) (Bool _)) = throwError "Cannot divide Bools" -- FIXME: translate

-- Int with all
evalBinExpr (BinExpr Mod (Int l) (Int r)) = return . Int $ l `mod` r
evalBinExpr (BinExpr Mod (Int l) (Float r)) = throwError "Modulo cannot be applied to Int and Float" -- FIXME: translate
evalBinExpr (BinExpr Mod (Float l) (Int r)) = throwError "Modulo cannot be applied to Float and Int" -- FIXME: translate
evalBinExpr (BinExpr Mod (String l) (Int r)) = throwError "Modulo cannot be applied to String and Int" -- FIXME: translate
evalBinExpr (BinExpr Mod (Int l) (String r)) = throwError "Modulo cannot be applied to Int and String" -- FIXME: translate
evalBinExpr (BinExpr Mod (Bool _) (Int _)) = throwError "Modulo cannot be applied to Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Mod (Int _) (Bool _)) = throwError "Modulo cannot be applied to Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Mod (Float l) (Float r)) = throwError "Modulo cannot be applied to Floats" -- FIXME: translate
evalBinExpr (BinExpr Mod (String l) (Float r)) = throwError "Modulo cannot be applied to String and Float" -- FIXME: translate
evalBinExpr (BinExpr Mod (Float l) (String r)) = throwError "Modulo cannot be applied to Float and String" -- FIXME: translate
evalBinExpr (BinExpr Mod (Bool _) (Float _)) = throwError "Modulo cannot be applied to Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Mod (Float _) (Bool _)) = throwError "Modulo cannot be applied to Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Mod (String l) (String r)) = throwError "Modulo cannot be applied to Strings"
evalBinExpr (BinExpr Mod (Bool l) (String r)) = throwError "Modulo cannot be applied to Bool and String"
evalBinExpr (BinExpr Mod (String l) (Bool r)) = throwError "Modulo cannot be applied to String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Mod (Bool _) (Bool _)) = throwError "Modulo cannot be applied to Bools" -- FIXME: translate

 -- Int with all
evalBinExpr (BinExpr Lt (Int l) (Int r)) = return . Bool $ l < r
evalBinExpr (BinExpr Lt (Int l) (Float r)) = return . Bool $ fromInteger l < r
evalBinExpr (BinExpr Lt (Float l) (Int r)) = return . Bool $ l < fromInteger r
evalBinExpr (BinExpr Lt (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr (BinExpr Lt (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr (BinExpr Lt (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Lt (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Lt (Float l) (Float r)) = return . Bool $ l < r
evalBinExpr (BinExpr Lt (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr (BinExpr Lt (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr (BinExpr Lt (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Lt (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Lt (String l) (String r)) = return . Bool $ l < r
evalBinExpr (BinExpr Lt (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr (BinExpr Lt (String l) (Bool r)) = throwError "Cannot compare String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Lt (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

 -- Int with all
evalBinExpr (BinExpr Le (Int l) (Int r)) = return . Bool $ l <= r
evalBinExpr (BinExpr Le (Int l) (Float r)) = return . Bool $ fromInteger l <= r
evalBinExpr (BinExpr Le (Float l) (Int r)) = return . Bool $ l <= fromInteger r
evalBinExpr (BinExpr Le (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr (BinExpr Le (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr (BinExpr Le (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Le (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Le (Float l) (Float r)) = return . Bool $ l <= r
evalBinExpr (BinExpr Le (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr (BinExpr Le (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr (BinExpr Le (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Le (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Le (String l) (String r)) = return . Bool $ l <= r
evalBinExpr (BinExpr Le (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr (BinExpr Le (String l) (Bool r)) = throwError "Cannot compare String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Le (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

    -- Int with all
evalBinExpr (BinExpr Gt (Int l) (Int r)) = return . Bool $ l > r
evalBinExpr (BinExpr Gt (Int l) (Float r)) = return . Bool $ fromInteger l > r
evalBinExpr (BinExpr Gt (Float l) (Int r)) = return . Bool $ l > fromInteger r
evalBinExpr (BinExpr Gt (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr (BinExpr Gt (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr (BinExpr Gt (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Gt (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Gt (Float l) (Float r)) = return . Bool $ l > r
evalBinExpr (BinExpr Gt (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr (BinExpr Gt (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr (BinExpr Gt (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Gt (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Gt (String l) (String r)) = return . Bool $ l > r
evalBinExpr (BinExpr Gt (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr (BinExpr Gt (String l) (Bool r)) = throwError "Cannot compare String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Gt (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

    -- Int with all
evalBinExpr (BinExpr Ge (Int l) (Int r)) = return . Bool $ l >= r
evalBinExpr (BinExpr Ge (Int l) (Float r)) = return . Bool $ fromInteger l >= r
evalBinExpr (BinExpr Ge (Float l) (Int r)) = return . Bool $ l >= fromInteger r
evalBinExpr (BinExpr Ge (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr (BinExpr Ge (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr (BinExpr Ge (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Ge (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Ge (Float l) (Float r)) = return . Bool $ l >= r
evalBinExpr (BinExpr Ge (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr (BinExpr Ge (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr (BinExpr Ge (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Ge (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Ge (String l) (String r)) = return . Bool $ l >= r
evalBinExpr (BinExpr Ge (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr (BinExpr Ge (String l) (Bool r)) = throwError "Cannot compare String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Ge (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

    -- Int with all
evalBinExpr (BinExpr Neq (Int l) (Int r)) = return . Bool $ l /= r
evalBinExpr (BinExpr Neq (Int l) (Float r)) = return . Bool $ fromInteger l /= r
evalBinExpr (BinExpr Neq (Float l) (Int r)) = return . Bool $ l /= fromInteger r
evalBinExpr (BinExpr Neq (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr (BinExpr Neq (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr (BinExpr Neq (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Neq (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Neq (Float l) (Float r)) = return . Bool $ l /= r
evalBinExpr (BinExpr Neq (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr (BinExpr Neq (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr (BinExpr Neq (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Neq (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Neq (String l) (String r)) = return . Bool $ l /= r
evalBinExpr (BinExpr Neq (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr (BinExpr Neq (String l) (Bool r)) = throwError "Cannot compare String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Neq (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

    -- Int with all
evalBinExpr (BinExpr Eq (Int l) (Int r)) = return . Bool $ l == r
evalBinExpr (BinExpr Eq (Int l) (Float r)) = return . Bool $ fromInteger l == r
evalBinExpr (BinExpr Eq (Float l) (Int r)) = return . Bool $ l == fromInteger r
evalBinExpr (BinExpr Eq (String l) (Int r)) = throwError "Cannot compare String and Int" -- FIXME: translate
evalBinExpr (BinExpr Eq (Int l) (String r)) = throwError "Cannot compare Int and String" -- FIXME: translate
evalBinExpr (BinExpr Eq (Bool _) (Int _)) = throwError "Cannot compare Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Eq (Int _) (Bool _)) = throwError "Cannot compare Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Eq (Float l) (Float r)) = return . Bool $ l == r
evalBinExpr (BinExpr Eq (String l) (Float r)) = throwError "Cannot compare String and Float" -- FIXME: translate
evalBinExpr (BinExpr Eq (Float l) (String r)) = throwError "Cannot compare Float and String" -- FIXME: translate
evalBinExpr (BinExpr Eq (Bool _) (Float _)) = throwError "Cannot compare Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Eq (Float _) (Bool _)) = throwError "Cannot compare Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Eq (String l) (String r)) = return . Bool $ l == r
evalBinExpr (BinExpr Eq (Bool l) (String r)) = throwError "Cannot compare Bool and String"
evalBinExpr (BinExpr Eq (String l) (Bool r)) = throwError "Cannot compare String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Eq (Bool _) (Bool _)) = throwError "Cannot compare Bools" -- FIXME: translate

    -- Int with all
evalBinExpr (BinExpr Pow (Int l) (Int r)) = return . Float $ fromInteger l ** fromInteger r
evalBinExpr (BinExpr Pow (Int l) (Float r)) = return . Float $ fromInteger l ** r
evalBinExpr (BinExpr Pow (Float l) (Int r)) = return . Float $ l ** fromInteger r
evalBinExpr (BinExpr Pow (String l) (Int r)) = throwError "Cannot raise String and Int" -- FIXME: translate
evalBinExpr (BinExpr Pow (Int l) (String r)) = throwError "Cannot raise Int and String" -- FIXME: translate
evalBinExpr (BinExpr Pow (Bool _) (Int _)) = throwError "Cannot raise Bool and Int" -- FIXME: translate
evalBinExpr (BinExpr Pow (Int _) (Bool _)) = throwError "Cannot raise Int and Bool" -- FIXME: translate
    -- Float with all
evalBinExpr (BinExpr Pow (Float l) (Float r)) = return . Float $ l ** r
evalBinExpr (BinExpr Pow (String l) (Float r)) = throwError "Cannot raise String and Float" -- FIXME: translate
evalBinExpr (BinExpr Pow (Float l) (String r)) = throwError "Cannot raise Float and String" -- FIXME: translate
evalBinExpr (BinExpr Pow (Bool _) (Float _)) = throwError "Cannot raise Bool and Float" -- FIXME: translate
evalBinExpr (BinExpr Pow (Float _) (Bool _)) = throwError "Cannot raise Float and Bool" -- FIXME: translate
    -- String with all
evalBinExpr (BinExpr Pow (String l) (String r)) = throwError "Cannot raise Strings"
evalBinExpr (BinExpr Pow (Bool l) (String r)) = throwError "Cannot raise Bool and String"
evalBinExpr (BinExpr Pow (String l) (Bool r)) = throwError "Cannot raise String and Bool"
    -- Bool with all
evalBinExpr (BinExpr Pow (Bool _) (Bool _)) = throwError "Cannot raise Bools" -- FIXME: translate

evalBinExpr (BinExpr op l (Var r)) = throwError "TODO: Not implemented yet" -- dereference Var, use its value
evalBinExpr (BinExpr op (Var l) r) = throwError "TODO: Not implemented yet" -- dereference Var, use its value

evalBinExpr (BinExpr op l r) = do
    a <- eval $ E l
    b <- eval $ E r
    return $ BinExpr op a b


evalUnExpr :: Expr -> Error Expr
evalUnExpr (UnExpr UnMinus (Int a)) = return . Int $ -1*a
evalUnExpr (UnExpr Not (Int _)) = throwError "Integer cannot be negated"
evalUnExpr (UnExpr UnMinus (Float a)) = return . Float $ -1.0*a
evalUnExpr (UnExpr Not (Float _)) = throwError "Float cannot be negated"
evalUnExpr (UnExpr UnMinus (String a)) = return . String $ reverse a
evalUnExpr (UnExpr Not (String _)) = throwError "String cannot be negated"
evalUnExpr (UnExpr _ (Bool a)) = return . Bool $ not a
evalUnExpr (UnExpr UnMinus (List a)) = return . List $ reverse a
evalUnExpr (UnExpr Not (List _)) = throwError "List cannot be negated"
evalUnExpr (UnExpr op (Var a)) = throwError "TODO: Not implemented yet"
evalUnExpr (UnExpr op a) =  do
    val <- eval (E a)
    evalUnExpr $ UnExpr op val
