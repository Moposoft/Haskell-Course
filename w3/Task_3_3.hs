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

-- 3.3
funA :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
funA f _ _ [] = []
funA f d z (s:ss)
  | (f s z) <= d = s : funA f d z ss
  | otherwise = funA f d z ss
  
funB :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
funB f d z ss = [s | s <- ss, (f s z) <= d]

funC :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
funC f d z ss = reverse $ foldl (\ss s -> if (f s z) <= d then s:ss else ss) [] ss

funD :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
funD f d z ss = filter (\s -> (f s z) <= d) ss