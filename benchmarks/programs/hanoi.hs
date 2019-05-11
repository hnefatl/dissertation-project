hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

main = show (hanoi (16 :: Int) 1 2 3 :: [(Int, Int)])