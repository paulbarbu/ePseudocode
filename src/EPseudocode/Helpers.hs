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
