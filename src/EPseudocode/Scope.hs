module EPseudocode.Scope (isValidScope)
where

import Data.List (elemIndices, nub, (\\))
import Control.Monad.Except

import EPseudocode.Data

data Name = VarName String | FuncName String deriving (Eq, Show)
type Scope = [Name] -- this is a stack of available names

type ScopeError = Either String

defaultScope :: [Name]
defaultScope = [FuncName "scrie"] -- TODO: more stdlib


gatherFunctions :: [Stmt] -> Scope
gatherFunctions program = foldl addFunc [] program
    where addFunc acc (FuncDef name _ _) = FuncName name : acc
          addFunc acc _ = acc


isSingleMain :: Scope -> Bool
isSingleMain globalScope = 1 == length (FuncName "main" `elemIndices` globalScope)


noDuplicateFunctions :: Scope -> Bool
noDuplicateFunctions scope = let funcs = [x | x@(FuncName _) <- scope] in funcs == nub funcs


isValidGlobalScope :: Scope -> ScopeError ()
isValidGlobalScope globalScope
    | not $ isSingleMain globalScope = throwError "There should be exactly one \"main\" function declared" -- FIXME: translate
    | not $ noDuplicateFunctions globalScope =
        throwError $ "Duplicate functions in the same scope: " ++ show (globalScope \\ nub globalScope) -- FIXME: translate
    | otherwise = return ()


isValidScope :: [Stmt] -> ScopeError ()
isValidScope program = isValidScope' defaultScope program


isValidScope' :: Scope -> [Stmt] -> ScopeError ()
isValidScope' defScope program = do
    let globalScope = defScope ++ gatherFunctions program
    isValidGlobalScope globalScope
    checkInnerScope globalScope program


--TODO: ret 42

checkInnerScope :: Scope -> [Stmt] -> ScopeError ()
checkInnerScope scope (FuncDef name args body:xs) =
    do let localFuncs = gatherFunctions body
       let scope' = scope ++ map VarName args ++ [FuncName name] ++ localFuncs
       if FuncName name `elem` localFuncs
          then throwError $ "Duplicate function names defined in the same scope: " ++ name -- FIXME: translate
          else checkInnerScope scope' body
       checkInnerScope scope xs -- TODO: check this, I think I have to add the current function to be able to reference it later

checkInnerScope scope (CompleteIf cond thenBody elseBody:xs) = do
    checkInnerScope scope [E cond]
    checkInnerScope scope thenBody
    checkInnerScope scope elseBody
    checkInnerScope scope xs

checkInnerScope scope (SimpleIf cond body:xs) = do
    checkInnerScope scope [E cond]
    checkInnerScope scope body
    checkInnerScope scope xs

checkInnerScope scope (While cond body:xs) = do
    checkInnerScope scope [E cond]
    checkInnerScope scope body
    checkInnerScope scope xs

checkInnerScope scope ((For (Just initial) (Just cond) (Just iter) body):xs) = do
  checkInnerScope scope [initial]
  checkInnerScope (scope ++ [getAssignedVarName initial]) [E cond]
  checkInnerScope (scope ++ [getAssignedVarName initial]) [iter]
  checkInnerScope (scope ++ [getAssignedVarName initial]) body
  checkInnerScope scope xs

checkInnerScope scope ((For (Nothing) (Just cond) (Just iter) body):xs) = do
  checkInnerScope scope [E cond]
  checkInnerScope scope [iter]
  checkInnerScope scope body
  checkInnerScope scope xs

checkInnerScope scope ((For (Just initial) (Nothing) (Just iter) body):xs) = do
  checkInnerScope scope [initial]
  checkInnerScope (scope ++ [getAssignedVarName initial]) [iter]
  checkInnerScope (scope ++ [getAssignedVarName initial]) body
  checkInnerScope scope xs

checkInnerScope scope ((For (Just initial) (Just cond) (Nothing) body):xs) = do
  checkInnerScope scope [initial]
  checkInnerScope (scope ++ [getAssignedVarName initial]) [E cond]
  checkInnerScope (scope ++ [getAssignedVarName initial]) body
  checkInnerScope scope xs

checkInnerScope scope ((For (Nothing) (Nothing) (Just iter) body):xs) = do
  checkInnerScope scope [iter]
  checkInnerScope scope body
  checkInnerScope scope xs

checkInnerScope scope ((For (Nothing) (Just cond) (Nothing) body):xs) = do
  checkInnerScope scope [E cond]
  checkInnerScope scope body
  checkInnerScope scope xs

checkInnerScope scope ((For (Just initial) (Nothing) (Nothing) body):xs) = do
  checkInnerScope scope [initial]
  checkInnerScope (scope ++ [getAssignedVarName initial]) body
  checkInnerScope scope xs

checkInnerScope scope ((For (Nothing) (Nothing) (Nothing) body):xs) = do
  checkInnerScope scope body
  checkInnerScope scope xs

checkInnerScope scope (Assign (Var name) s:xs) = do
    checkInnerScope scope [s]
    checkInnerScope (scope ++ [VarName name]) xs

checkInnerScope scope (Assign (Index name exprs) s:xs) = do
    mapM (\e -> checkInnerScope scope [E e]) exprs
    checkInnerScope scope [s]
    checkInnerScope (scope ++ [VarName name]) xs

checkInnerScope _ (Assign _ _:_) = error "Invalid assignment, please file a bug report" -- FIXME: translate

checkInnerScope scope (E (BinExpr _ leftExpr rightExpr):xs) = do
    checkInnerScope scope [E leftExpr]
    checkInnerScope scope [E rightExpr]
    checkInnerScope scope xs

checkInnerScope scope (E (UnExpr _ expr):xs) = do
    checkInnerScope scope [E expr]
    checkInnerScope scope xs

checkInnerScope scope (E (FuncCall (Var name) _):xs) = if FuncName name `elem` scope
      then checkInnerScope scope xs
      else throwError $ "Call to undefined function name: " ++ name -- FIXME: translate

checkInnerScope scope (E (Var name):xs) = if VarName name `elem` scope
    then checkInnerScope scope xs
    else throwError $ "Reference to undefined variable name: " ++ name -- FIXME: translate

checkInnerScope scope (E (Index name exprs):xs) = if VarName name `elem` scope
    then do
      mapM (\e -> checkInnerScope scope [E e]) exprs
      checkInnerScope scope xs
    else throwError $ "Reference to undefined variable name: " ++ name -- FIXME: translate

checkInnerScope _ _ = return ()


getAssignedVarName :: Stmt -> Name
getAssignedVarName (Assign (Var name) _) = VarName name
getAssignedVarName (Assign (Index name _) _) = VarName name
getAssignedVarName _ = error "Invalid for initial expression, please file a bug report"

-- TODO: global variables

--checkInnerScope _ [] = return () -- TODO: cover all cases
