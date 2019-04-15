fact 0 = 1
fact n = n * fact (n - 1)

main = show (fact 100 :: Integer)