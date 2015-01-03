module EPseudocode.Scope (isValidScope)
where

--import Data.List (elemIndices, nub, (\\))
--import Control.Monad.Except

--import EPseudocode.Data

----data Name = VarName String | FuncName String deriving (Eq, Show)
--type Scope = [String] -- this is a stack of available names

--type ScopeError = Either String

--defaultScope :: Scope
--defaultScope = ["scrie", "citeste", "lung"] -- TODO: more stdlib


--gatherFunctions :: [Stmt] -> Scope
--gatherFunctions program = foldl addFunc [] program
--    where addFunc acc (FuncDef name _ _) = name : acc
--          addFunc acc _ = acc


--isSingleMain :: Scope -> Bool
--isSingleMain globalScope = 1 == length ("main" `elemIndices` globalScope)


--isValidGlobalScope :: Scope -> ScopeError ()
--isValidGlobalScope globalScope
--    | not $ isSingleMain globalScope = throwError "There should be exactly one \"main\" function declared" -- FIXME: translate
--    | globalScope /= nub globalScope =
--        throwError $ "Duplicate names in the same scope: " ++ show (globalScope \\ nub globalScope) -- FIXME: translate
--    | otherwise = return ()


--isValidScope :: [Stmt] -> ScopeError ()
--isValidScope program = isValidScope' defaultScope program


--isValidScope' :: Scope -> [Stmt] -> ScopeError ()
--isValidScope' defScope program = do
--    let globalScope = defScope ++ gatherFunctions program
--    isValidGlobalScope globalScope
--    checkInnerScope globalScope program


--checkInnerScope :: Scope -> [Stmt] -> ScopeError ()
--checkInnerScope scope (FuncDef name args body:xs) =
--  if args == nub args
--    then do
--      let localFuncs = gatherFunctions body
--      -- add the arguments as both variable names and function names so I can validate lambdas
--      let scope' = scope ++ args ++ [name] ++ localFuncs
--      if name `elem` localFuncs
--        then throwError $ "Duplicate function names defined in the same scope: " ++ name -- FIXME: translate
--        else checkInnerScope scope' body
--      checkInnerScope scope xs
--  else
--    throwError $ "Duplicate parameter names in \"" ++ name ++ "\" function definition"

--checkInnerScope scope (CompleteIf cond thenBody elseBody:xs) = do
--    checkInnerScope scope [E cond]
--    checkInnerScope scope thenBody
--    checkInnerScope scope elseBody
--    checkInnerScope scope xs

--checkInnerScope scope (SimpleIf cond body:xs) = do
--    checkInnerScope scope [E cond]
--    checkInnerScope scope body
--    checkInnerScope scope xs

--checkInnerScope scope (While cond body:xs) = do
--    checkInnerScope scope [E cond]
--    checkInnerScope scope body
--    checkInnerScope scope xs

--checkInnerScope scope ((For (Just initial) (Just cond) (Just iter) body):xs) = do
--  checkInnerScope scope [initial]
--  checkInnerScope (scope ++ [getAssignedVarName initial]) [E cond]
--  checkInnerScope (scope ++ [getAssignedVarName initial]) [iter]
--  checkInnerScope (scope ++ [getAssignedVarName initial]) body
--  checkInnerScope scope xs

--checkInnerScope scope ((For (Nothing) (Just cond) (Just iter) body):xs) = do
--  checkInnerScope scope [E cond]
--  checkInnerScope scope [iter]
--  checkInnerScope scope body
--  checkInnerScope scope xs

--checkInnerScope scope ((For (Just initial) (Nothing) (Just iter) body):xs) = do
--  checkInnerScope scope [initial]
--  checkInnerScope (scope ++ [getAssignedVarName initial]) [iter]
--  checkInnerScope (scope ++ [getAssignedVarName initial]) body
--  checkInnerScope scope xs

--checkInnerScope scope ((For (Just initial) (Just cond) (Nothing) body):xs) = do
--  checkInnerScope scope [initial]
--  checkInnerScope (scope ++ [getAssignedVarName initial]) [E cond]
--  checkInnerScope (scope ++ [getAssignedVarName initial]) body
--  checkInnerScope scope xs

--checkInnerScope scope ((For (Nothing) (Nothing) (Just iter) body):xs) = do
--  checkInnerScope scope [iter]
--  checkInnerScope scope body
--  checkInnerScope scope xs

--checkInnerScope scope ((For (Nothing) (Just cond) (Nothing) body):xs) = do
--  checkInnerScope scope [E cond]
--  checkInnerScope scope body
--  checkInnerScope scope xs

--checkInnerScope scope ((For (Just initial) (Nothing) (Nothing) body):xs) = do
--  checkInnerScope scope [initial]
--  checkInnerScope (scope ++ [getAssignedVarName initial]) body
--  checkInnerScope scope xs

--checkInnerScope scope ((For (Nothing) (Nothing) (Nothing) body):xs) = do
--  checkInnerScope scope body
--  checkInnerScope scope xs

--checkInnerScope scope (Assign (Var name) s:xs) = do
--    checkInnerScope scope [s]
--    checkInnerScope (scope ++ [name]) xs

--checkInnerScope scope (Assign (Index name exprs) s:xs) = do
--    mapM (\e -> checkInnerScope scope [E e]) exprs
--    checkInnerScope scope [s]
--    checkInnerScope (scope ++ [name]) xs

--checkInnerScope _ (Assign _ _:_) = error "Invalid assignment, please file a bug report" -- FIXME: translate

--checkInnerScope scope (Ret ret:xs) = do
--  checkInnerScope scope [ret]
--  checkInnerScope scope xs

--checkInnerScope scope (E (BinExpr _ leftExpr rightExpr):xs) = do
--    checkInnerScope scope [E leftExpr]
--    checkInnerScope scope [E rightExpr]
--    checkInnerScope scope xs

--checkInnerScope scope (E (UnExpr _ expr):xs) = do
--    checkInnerScope scope [E expr]
--    checkInnerScope scope xs

--checkInnerScope scope (E (FuncCall (Var name) args):xs) = if name `elem` scope
--      then do
--        mapM (\a -> checkInnerScope scope a) args
--        checkInnerScope scope xs
--      else throwError $ "Call to undefined name: " ++ name -- FIXME: translate

--checkInnerScope scope (E (FuncCall (Index name exprs) args):xs) = if name `elem` scope
--    then do
--      mapM (\e -> checkInnerScope scope [E e]) exprs
--      mapM (\a -> checkInnerScope scope a) args
--      checkInnerScope scope xs
--    else throwError $ "Call to undefined name: " ++ name -- FIXME: translate

--checkInnerScope _ (E (FuncCall _ _):_) = error "Invalid function call, plase file a bug report" -- FIXME: translate

--checkInnerScope scope (E (Var name):xs) = if name `elem` scope
--    then checkInnerScope scope xs
--    else throwError $ "Reference to undefined name: " ++ name -- FIXME: translate

--checkInnerScope scope (E (List stmts):xs) = do
--    checkInnerScope scope stmts
--    checkInnerScope scope xs

--checkInnerScope scope (E (Index name exprs):xs) = if name `elem` scope
--    then do
--      mapM (\e -> checkInnerScope scope [E e]) exprs
--      checkInnerScope scope xs
--    else throwError $ "Reference to undefined name: " ++ name -- FIXME: translate

--checkInnerScope scope (_:xs) = checkInnerScope scope xs
--checkInnerScope _ [] = return ()


--getAssignedVarName :: Stmt -> String
--getAssignedVarName (Assign (Var name) _) = name
--getAssignedVarName (Assign (Index name _) _) = name
--getAssignedVarName _ = error "Invalid for initial expression, please file a bug report"
