data Int
data Integer
data Char
data [] a = [] | a : [a]
data (,) a b = (,) a b
data (,,) a b c = (,,) a b c
data Bool = False | True
data Maybe a = Nothing | Just a

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

class Show a where
    show :: a -> [Char]
instance Show Char where
    show c = [c]
instance Show Int where
    show = primShowIntShow
instance Show Integer where
    show = primShowIntegerShow
instance Show Bool where
    show True = "True"
    show False = "False"

primShowIntShow :: Int -> [Char]
primShowIntegerShow :: Integer -> [Char]

instance Show [Int] where
    show xs = "[" ++ ((intercalate ", " (map show xs)) ++ "]")
instance Show [Integer] where
    show xs = "[" ++ ((intercalate ", " (map show xs)) ++ "]")
instance Show [Char] where
    show = concat . map show
instance Show [Bool] where
    show xs = "[" ++ ((intercalate ", " (map show xs)) ++ "]")


class Functor f where
    fmap :: (a -> b) -> f a -> f b
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
instance Functor [] where
    fmap = map

class Monad m where
    (>>=) :: (a -> m b) -> m a -> m b
    return :: a -> m a
instance Monad Maybe where
    _ >>= Nothing = Nothing
    f >>= (Just x) = f x
    return = Just
instance Monad [] where
    _ >>= [] = []
    f >>= (x:xs) = (f x) ++ (f >>= xs)
    return x = [x]

[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

id x = x

True && True = True
_ && _ = False

not True = False
not False = True

all f [] = True
all f (x:xs) = f x && all f xs

foldl _ a [] = a
foldl f a (x:xs) = foldl f (f a x) xs

map f [] = []
map f (x:xs) = (f x):(map f xs)

concat = foldl (++) []

intercalate sep [] = []
intercalate sep [x] = x
intercalate sep (x:xs) = x ++ (sep ++ intercalate sep xs)

sum = foldl (+) 0

const x _ = x

f . g = \x -> f (g x)

undefined :: a
compilerError :: a
