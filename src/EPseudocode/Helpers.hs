module EPseudocode.Helpers (replace)
where

replace :: Int -> a -> [a] -> [a]
replace i val list = a ++ [val] ++ b where (a, _:b) = splitAt i list
