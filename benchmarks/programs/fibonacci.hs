fib 0 = 1
fib n@1 = n
fib n = fib (n - 1) + fib (n - 2)

main = show (fib 20 :: Int)