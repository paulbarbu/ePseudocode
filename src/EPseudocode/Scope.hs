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
    where addFunc acc (FuncDef name _ _) = (FuncName name) : acc
          addFunc acc _ = acc


isSingleMain :: Scope -> Bool
isSingleMain globalScope = 1 == length ((FuncName "main") `elemIndices` globalScope)


noDuplicateFunctions :: Scope -> Bool
noDuplicateFunctions scope = let funcs = [x | x@(FuncName _) <- scope] in funcs == (nub funcs)


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


checkInnerScope :: Scope -> [Stmt] -> ScopeError ()
checkInnerScope scope ((FuncDef name args body):xs) =
    do let localFuncs = gatherFunctions body
       let scope' = scope ++ map VarName args ++ [FuncName name] ++ localFuncs
       if (FuncName name) `elem` localFuncs
          then throwError $ "Duplicate function names defined in the same scope: " ++ name -- FIXME: translate
          else checkInnerScope scope' body
       checkInnerScope scope xs -- TODO: check this, I think I have to add the current function to be able to reference it later

checkInnerScope scope ((Assign (Var name) s):xs) = do
    checkInnerScope scope [s]
    checkInnerScope (scope ++ [VarName name]) xs

checkInnerScope scope ((E (FuncCall (Var name) _)):xs) = do
   if (FuncName name) `elem` scope
      then checkInnerScope scope xs
      else throwError $ "Call to undefined function name: " ++ name -- FIXME: translate

checkInnerScope scope ((E (Var name)):xs) = if (VarName name) `elem` scope
    then checkInnerScope scope xs
    else throwError $ "Reference to undefined variable name: " ++ name -- FIXME: translate

checkInnerScope _ _ = return ()

-- TODO: global variables

-- checkInnerScope _ [] = return () TODO
