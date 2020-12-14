f :: String -> String -> String
f _ [] = ""
f [] _ = ""
f (s:s1) s2 = if elem s s2
  then s : f s1 (tail (dropWhile (/=s) s2))
  else f s1 s2