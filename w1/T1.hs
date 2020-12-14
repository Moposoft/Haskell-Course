myFunction :: Int -> [(Int,Int)]  
myFunction x = [(a,b) | a <- [-x..x], b <- [-x..x], abs a+ abs b <= x]