evaluateCards :: (Char,Int) -> (Char,Int) -> Int
evaluateCards (suit1,value1) (suit2, value2) 
  | (suit1=='s' && value1==14) || (suit2=='s' && value2==14) = 14
  | (suit1==suit2 && value1==value2+1) || (suit1==suit2 && value1==value2-1) = 8
  | (value1==value2) = 6
  | (value1==value2+1) || (value1==value2-1) = 4
  | (suit1==suit2) = 2
  | otherwise = 0