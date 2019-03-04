data Int
data Integer
data Char
data [] a = [] | a : [a]
data (,) a b = (,) a b
data (,,) a b c = (,,) a b c
data Bool = False | True

class Ord a where
    (<) :: a -> a -> Bool
instance Ord Int where
    (<) = primOrdIntLess

primOrdIntLess :: Int -> Int -> Bool

class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    negate :: a -> a
    fromInteger :: Integer -> a
instance Num Int where
    (+) = primNumIntAdd
    (-) = primNumIntSub
    (*) = primNumIntMult
    negate = primNumIntNegate
    fromInteger = primNumIntFromInteger
primNumIntAdd :: Int -> Int -> Int
primNumIntSub :: Int -> Int -> Int
primNumIntMult :: Int -> Int -> Int
primNumIntNegate :: Int -> Int
primNumIntFromInteger :: Integer -> Int
instance Num Integer where
    (+) = primNumIntegerAdd
    (-) = primNumIntegerSub
    (*) = primNumIntegerMult
    negate = primNumIntegerNegate
    fromInteger = id
primNumIntegerAdd :: Integer -> Integer -> Integer
primNumIntegerSub :: Integer -> Integer -> Integer
primNumIntegerMult :: Integer -> Integer -> Integer
primNumIntegerNegate :: Integer -> Integer


class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
instance Eq Int where
    (==) = primEqIntEq
    x /= y = not (x == y)
instance Eq Integer where
    (==) = primEqIntegerEq
    x /= y = not (x == y)
instance Eq Char where
    (==) = primEqCharEq
    x /= y = not (x == y)
instance Eq Bool where
    True == True = True
    False == False = True
    _ == _ = False
    x /= y = not (x == y)

primEqIntEq :: Int -> Int -> Bool
primEqIntegerEq :: Integer -> Integer -> Bool
primEqCharEq :: Char -> Char -> Bool

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