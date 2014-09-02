module EPseudocode.Data
where


data Val = Int Integer
    | Float Double
    | String String
    | Bool Bool
    | List [Val]
    deriving (Show, Eq, Ord)


data Expr = Ct Val -- literal values
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


instance Num Val where
    (Int x) + (Int y) = Int $ x + y
    (Float x) + (Float y) = Float $ x + y
    (Int x) + (Float y) = Float $ fromInteger x + y
    (Float x) + (Int y) = Float $ x + fromInteger y

    (Int x) * (Int y) = Int $ x * y
    (Float x) * (Float y) = Float $ x * y
    (Int x) * (Float y) = Float $ fromInteger x * y
    (Float x) * (Int y) = Float $ x * fromInteger y

    abs (Int x) = Int $ abs x
    abs (Float x) = Float $ abs x

    signum (Int x) = Int $ signum x
    signum (Float x) = Float $ signum x

    fromInteger x = Int x

    negate (Int x) = Int $ negate x
    negate (Float x) = Float $ negate x


instance Enum Val where
    fromEnum (Int _) = 0
    fromEnum (Float _) = 1
    fromEnum (String _) = 2
    fromEnum (Bool _) = 3
    fromEnum (List _) = 4

    toEnum _ = Int 0


instance Real Val where
    toRational (Int x) = toRational x
    toRational (Float x) = toRational x


instance Integral Val where
    toInteger (Int x) = x
    (Int x) `quotRem` (Int y) = (Int $ x `quot` y, Int $ x `rem` y)
    -- TODO: erros in rest

instance Fractional Val where
    fromRational x = Float $ fromRational x

    recip (Int x) = Float $ 1/fromInteger x
    recip (Float x) = Float $ 1/x


    {- --TODO: think where these belong
    String + String
    List + Int
    List + Float
    List + String
    String + List
    Float + List
    String + List
    -}
