
test = "Mississippi river"




frequency :: String -> [(Char, Int)]
--[[(x, 1) | x <- xs] generates a list of pairs with key x  and value 1 for all x in xs
-- fromListWith applies a funtion on the value of pairs with the same key. Using the function '+' therefore calculates the count.
frequency xs =  [(x, 1) | x <- xs]
                   


-- Gets unique elements in a list. Note that it reverses the order of the list.
unique :: String -> String
unique lst = unique' lst []
            where
                unique' [] ls = ls
                unique' (x:xs) ls
                    | x `elem` ls   = unique' xs ls
                    | otherwise     = unique' xs (x:ls)