module EPseudocode.Helpers
where

import Control.Monad.Except

import EPseudocode.Data

replace :: Int -> a -> [a] -> [a]
replace i val list = a ++ [val] ++ b where (a, _:b) = splitAt i list


invalidListIndex :: String -> Integer -> String
invalidListIndex listName index = "Invalid list index: " ++ listName ++ "[" ++ show index ++ "]"


invalidNestedListIndex :: Integer -> String
invalidNestedListIndex index = "Invalid nested list index: " ++ show index


indexList :: (Integer -> String) -> IndexingExpr -> IndexedList -> ListAction -> Error (Env, Expr)
indexList invalidIndexErr index list action =
    case index of
        (Int i) ->
            if fromIntegral i < length list && i >= 0 then
                action i list
            else
                throwError $ invalidIndexErr i
        _ -> throwError "List can be indexed only with Integer evaluating expressions"
