upFrom n = n:upFrom (n + 1)
ints = upFrom 0

main = show (sum (map (2*) (take 100000 (filter odd ints))) :: Integer)