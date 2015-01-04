module EPseudocode.Data
where

import Data.List (intercalate)


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
    | FuncCall Expr [[Expr]] -- foo() = [[]], func_in_func()(2) = [[],[2]], func_in_list[1]() = [[]], a(1)() = [[1],[]]
    | FuncDef String [String] [Stmt] -- func name args body
    deriving (Eq, Ord)

data BinOp = And | Or | Plus | Minus | Mul | Div | Mod | Lt | Le | Gt | Ge | Neq | Eq | Pow
    deriving (Show, Eq, Ord)


data UnOp = Not | UnMinus
    deriving (Show, Eq, Ord)


data Stmt = Assign Expr Expr -- assignments (the left side is limited to variable or list index and the right side is limited here to expression or function (call/def) similarly to Ret)
    | CompleteIf Expr [Stmt] [Stmt] -- if condition then statements else statements
    | SimpleIf Expr [Stmt] -- if condition then statements
    | While Expr [Stmt] -- while condition then statements
    | For (Maybe Stmt) (Maybe Expr) (Maybe Stmt) [Stmt] -- for initial, condition, iteration then statements (the initial and the iteration are limited to assignments)
    | Ret Expr -- return statement (only expression or function call/def)
    | E Expr
    deriving (Show, Eq, Ord)


type Env = [(String, Expr)]


type Error = Either String

-- Helpers
type IndexingExpr = Expr
type IndexingListExpr = [Expr]
type IndexedList = [Expr]


-- Instances
instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Int i) = show i
showExpr (List l) = "{" ++ intercalate ", " (map showExpr l) ++ "}"
showExpr a = show a
