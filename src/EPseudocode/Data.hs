module EPseudocode.Data
where


data Expr =
    -- literal values
    Int Integer
    | Float Double
    | String String
    | Bool Bool
    | List [Expr]
    -- variables
    | Var String
    -- unary/binary expressions
    | UnExpr UnOp Expr
    | BinExpr BinOp Expr Expr
    deriving Show

data BinOp = And | Or | Plus | Minus | Mul | Div | Mod | Lt | Le | Gt | Ge | Neq | Eq
    deriving Show


data UnOp = Not | UnMinus
    deriving Show


data Stmt = Assign String Expr -- assignments
    | CompleteIf Expr Stmt Stmt -- if condition then statements else statements
    | SimpleIf Expr Stmt -- if condition then statements
    | While Expr Stmt -- while condition then statements
    | For Stmt Expr Expr Stmt -- for initial, condition, iteration then statements
    -- TODO: sequence of statements, terminated by \n, return is used only inside functions
    -- TODO: functions
    deriving Show
