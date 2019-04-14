mergesort [] = []
mergesort [x] = [x]
mergesort [x, y] = if x < y then [x, y] else [y, x]
mergesort zs = let (xs, ys) = split zs in merge (mergesort xs) (mergesort ys)

split [] = ([], [])
split (x:[]) = ([x], [])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys)

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x:merge xs (y:ys) else y:merge (x:xs) ys

list = [19, 18, 9, 14, 4, 5, 8, 1, 15, 11, 17, 10, 2, 13, 20, 6, 7, 12, 16, 3] :: [Int]

main = show (mergesort list)
--main = print $ mergesort list