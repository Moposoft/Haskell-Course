import Data.Char
onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits [x] = isDigit x
onlyDigits (x:xs)
  | isDigit x == False = False 
  | otherwise = onlyDigits xs