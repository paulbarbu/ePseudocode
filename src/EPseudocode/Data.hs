module EPseudocode.Data
where

-- TODO: this could be simplified I merge Expr with Stmt and make the difference in the parser

data Expr =
    -- literal values
    Int Integer
    | Float Double
    | String String
    | Bool Bool
    | List [Stmt] --list of experssions, fucntion definitions or function calls
    -- variables
    | Var String
    -- list indexing
    | Index String [Expr] -- a[1+2], a[2][3]
    -- unary/binary expressions
    | UnExpr UnOp Expr
    | BinExpr BinOp Expr Expr
    -- function calls, the Stmt is limited to expr or function definitions
    | FuncCall Expr [[Stmt]] -- foo() = [[]], func_in_func()(2) = [[],[2]], func_in_list[1]() = [[]], a(1)() = [[1],[]]
    deriving (Show, Eq)

data BinOp = And | Or | Plus | Minus | Mul | Div | Mod | Lt | Le | Gt | Ge | Neq | Eq
    deriving (Show, Eq)


data UnOp = Not | UnMinus
    deriving (Show, Eq)


data Stmt = Assign String Stmt -- assignments (stmt is limited here to expression or function similarly to Ret)
    | CompleteIf Expr [Stmt] [Stmt] -- if condition then statements else statements
    | SimpleIf Expr [Stmt] -- if condition then statements
    | While Expr [Stmt] -- while condition then statements
    | For Stmt Expr Stmt [Stmt] -- for initial, condition, iteration then statements
    | Ret Stmt -- return statement (only expression or function)
    | FuncDef String [String] [Stmt] -- func name args body
    | E Expr
    deriving (Show, Eq)
