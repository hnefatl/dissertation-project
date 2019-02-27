data Int
data [] a = [] | a : [a]
data (,) a b = (,) a b
data (,,) a b c = (,,) a b c
data Bool = False | True

class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
instance Num Int where
    (+) = primNumIntAdd
    (-) = primNumIntSub
    (*) = primNumIntMult
    (/) = primNumIntDiv

primNumIntAdd :: Int -> Int -> Int
primNumIntSub :: Int -> Int -> Int
primNumIntMult :: Int -> Int -> Int
primNumIntDiv :: Int -> Int -> Int

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
instance Eq Int where
    (==) = primEqIntEq
    x /= y = not (x == y)

primEqIntEq :: Int -> Int -> Bool

id x = x

True && True = True
_ && _ = False

not True = False
not False = True

all f [] = True
all f (x:xs) = f x && all f xs

foldl _ a [] = a
foldl f a (x:xs) = foldl f (f a x) xs

sum = foldl (+) 0