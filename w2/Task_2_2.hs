import Data.Char

--Task 2.2 uses Task 2.1
isFinnishIBAN :: String -> Bool
isFinnishIBAN xs = if length xs == 18 && head xs == 'F' && head(tail xs) == 'I' && onlyDigits(tail(tail xs))
  then True
  else False
  
--Task 2.1
onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits [x] = isDigit x
onlyDigits (x:xs)
  | isDigit x == False = False 
  | otherwise = onlyDigits xs