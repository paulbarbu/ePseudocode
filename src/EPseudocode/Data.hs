module EPseudocode.Data
where

data Val = Int Integer
    | Float Float
    | String String
    | Bool Bool
    | List [Val]
    deriving Show

data Expr = Val -- literal values
    | Var String -- variables
    | UnaryOp String Expr -- unary operations -- TODO: here instead of String I could use a type if needed
    | BinaryOp String Expr Expr -- binary operations
    deriving Show

data Stmt = Assign String Expr -- assignments
    | CompleteIf Expr Stmt Stmt -- if condition then statements else statements
    | SimpleIf Expr Stmt -- if condition then statements
    | While Expr Stmt -- while condition then statements
    | For Stmt Expr Expr Stmt -- for initial, condition, iteration then statements
    deriving Show

answer = 42
