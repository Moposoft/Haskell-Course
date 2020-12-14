f :: (Char, Char) -> Int -> String -> Int
f (a,b) g [] = 0
f (a,b) g (x:xs) = if x == a && xs !! g == b
  then 1 + f (a,b) g xs
  else f (a,b) g xs