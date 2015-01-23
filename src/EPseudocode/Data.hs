module EPseudocode.Data
where

import Control.Monad.Except
import Data.List (intercalate)
import Text.Show.Functions ()
import System.IO

import EPseudocode.Lexer

data Expr =
    -- literal values
    Int Integer
    | Float Double
    | String String
    | Bool Bool
    | List [Expr] --list of expressions, function definitions or function calls
    -- variables
    | Var String
    -- list indexing
    | Index String [Expr] -- a[1+2], a[2][3]
    -- unary/binary expressions
    | UnExpr UnOp Expr
    | BinExpr BinOp Expr Expr
    -- function calls, the Stmt is limited to expr or function definitions
    | Func [String] [Stmt] Env -- func args body env - the actual type that gets generated upon defining a function
    | FuncCall Expr [[Expr]] -- foo() = [[]], func_in_func()(2) = [[],[2]], func_in_list[1]() = [[]], a(1)() = [[1],[]]
    | FuncDef String [String] [Stmt] -- func name args body - this comes out of the parser
    | Void -- should not be evaluated, just a placeholder for "none" or apophasis
    | BuiltinIOFunc ([[Expr]] -> ErrorWithIO Expr)
    | BuiltinFunc ([[Expr]] -> Error Expr)
    | File Handle
    deriving (Show)


data BinOp = And | Or | Plus | Minus | Mul | Div | Mod | Lt | Le | Gt | Ge | Neq | Eq | Pow
    deriving (Show)


data UnOp = Not | UnMinus
    deriving (Show)


data Stmt = Assign Expr Expr -- assignments (the left side is limited to variable or list index and the right side is limited here to expression or function (call/def) similarly to Ret)
    | CompleteIf Expr [Stmt] [Stmt] -- if condition then statements else statements
    | SimpleIf Expr [Stmt] -- if condition then statements
    | While Expr [Stmt] -- while condition then statements
    | For (Maybe Stmt) (Maybe Expr) (Maybe Stmt) [Stmt] -- for initial, condition, iteration then statements (the initial and the iteration are limited to assignments)
    | Ret Expr -- return statement (only expression or function call/def)
    | Break
    | E Expr
    deriving (Show)


type Env = [(String, Expr)]


type Error = Either String
type ErrorWithIO = ExceptT String IO


-- Helpers
type IndexingExpr = Expr
type IndexingListExpr = [Expr]
type IndexedList = [Expr]


showExpr :: Expr -> String
showExpr (Int i) = show i
showExpr (Float f) = show f
showExpr (String s) = "\"" ++ s ++ "\""
showExpr (Bool b) = if b then tTrue else tFalse
showExpr (List l) = "{" ++ intercalate ", " (map showExpr l) ++ "}"
showExpr Void = ""
showExpr BuiltinFunc{} = "<builtin func>"
showExpr BuiltinIOFunc{} = "<builtin func>"
showExpr (Func args _ _) = "<user defined func taking " ++ show (length args) ++ " args>"
showExpr a = show a
