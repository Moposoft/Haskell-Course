f :: (Ord a) => [a] -> [a]
f xs = [x | (x,y)<-zip xs ys, x<y]
  where ys = tail xs  