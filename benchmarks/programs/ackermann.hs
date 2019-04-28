ack 0 n = n + 1
ack m 0 = ack (m - 1) (1 :: Int)
ack m n = ack (m - 1) (ack m (n - 1))

main = show (ack (3 :: Int) 8)