-- a)
distance1 :: String -> String -> Float
distance1 "" "" = 0
distance1 x y =
  let lenX = fromIntegral (length x)
      lenY = fromIntegral (length y)
      distXY = fromIntegral (length [c | c<-x, not (c `elem` y)])
      distYX = fromIntegral (length [c | c<-y, not (c `elem` x)])
  in (distXY + distYX) / (lenX + lenY)

-- b)
distance2 :: String -> String -> Float
distance2 "" "" = 0
distance2 x y =
  let lenX = fromIntegral (length x)
      lenY = fromIntegral (length y)
      distX = fromIntegral (length [c | c<-x, not (c `elem` ['0'..'9'])])
      distY = fromIntegral (length [c | c<-y, not (c `elem` ['0'..'9'])])
  in (distX + distY) / (lenX + lenY)