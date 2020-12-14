-- function 3.2a
distance1 :: String -> String -> Float
distance1 "" "" = 0
distance1 x y =
  let lenX = fromIntegral (length x)
      lenY = fromIntegral (length y)
      distXY = fromIntegral (length [c | c<-x, not (c `elem` y)])
      distYX = fromIntegral (length [c | c<-y, not (c `elem` x)])
  in (distXY + distYX) / (lenX + lenY)
  
-- function 3.2b
distance2 :: String -> String -> Float
distance2 "" "" = 0
distance2 x y =
  let lenX = fromIntegral (length x)
      lenY = fromIntegral (length y)
      distX = fromIntegral (length [c | c<-x, not (c `elem` ['0'..'9'])])
      distY = fromIntegral (length [c | c<-y, not (c `elem` ['0'..'9'])])
  in (distX + distY) / (lenX + lenY)

-- function 3.3
fun3_3 :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fun3_3 f d z ss = [x | x<-ss, (f x z) <= d]

-- function 3.4
fun3_4 :: (String -> String -> Float) -> Float -> [String] -> [[String]]
fun3_4 f d ss = map fun ss
  where fun s = fun3_3 f d s ss   