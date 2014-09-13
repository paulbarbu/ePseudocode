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
    | FuncCall String [Expr]
    deriving Show

data BinOp = And | Or | Plus | Minus | Mul | Div | Mod | Lt | Le | Gt | Ge | Neq | Eq
    deriving Show


data UnOp = Not | UnMinus
    deriving Show


data Stmt = Assign String Stmt -- assignments (stmt is limited here to expression or function similarly to Ret)
    | CompleteIf Expr Stmt Stmt -- if condition then statements else statements
    | SimpleIf Expr Stmt -- if condition then statements
    | While Expr Stmt -- while condition then statements
    | For Stmt Expr Stmt Stmt -- for initial, condition, iteration then statements
    | Ret Stmt -- return statement (only expression or function)
    | FuncDef String [String] Stmt -- func name args body
    | E Expr
    | Seq [Stmt]
    --TODO: multiple statements one after the other: runLex mainParser "func x() ret 5 sf func daca 3 atunci 5 sf daca"
    deriving Show
