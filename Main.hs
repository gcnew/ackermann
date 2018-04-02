{-# LANGUAGE Rank2Types #-}

rr :: a -> (Int -> (a -> a)) -> (Int -> a)
rr i f 0 = i
rr i f n = f (n-1) (rr i f (n-1))

next :: Int -> Int
next = (+ 1)

ack :: Int -> Int -> Int
ack m = rr next (\_ f n -> rr (f 1) (const f) n) m

main = print $ ack 3 4
