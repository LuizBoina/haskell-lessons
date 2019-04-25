ultimo ns = drop (length ns -1) ns  -- last
rUltimo ns = take (length ns -1) ns  -- init
add :: (Int,Int)->Int
add(x,y) = x+y
zeroto :: Int->[Int]
zeroto n = [0..n]
