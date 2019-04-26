data Int
data Integer
data Char
data [] a = [] | a : [a]
data (,) a b = (,) a b
data (,,) a b c = (,,) a b c
data (,,,) a b c d = (,,,) a b c d
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

-- These are hacky ways to work around not having support for instance superclasses (Show a => Show [a])
instance Show [Int] where
    show xs = "[" ++ ((intercalate "," (map show xs)) ++ "]")
instance Show [Integer] where
    show xs = "[" ++ ((intercalate "," (map show xs)) ++ "]")
instance Show [Char] where
    show = concat . map show
instance Show [Bool] where
    show xs = "[" ++ ((intercalate "," (map show xs)) ++ "]")
instance Show (Maybe Int) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

class Integral a where
    div :: a -> a -> a
    mod :: a -> a -> a
instance Integral Int where
    div = primIntegralIntDiv
    mod = primIntegralIntMod
instance Integral Integer where
    div = primIntegralIntegerDiv
    mod = primIntegralIntegerMod

primIntegralIntDiv :: Int -> Int -> Int
primIntegralIntMod :: Int -> Int -> Int
primIntegralIntegerDiv :: Integer -> Integer -> Integer
primIntegralIntegerMod :: Integer -> Integer -> Integer

even, odd :: (Eq a, Integral a, Num a) => a -> Bool
even x = x `mod` 2 == 0
odd = not . even

class Functor f where
    fmap :: (a -> b) -> f a -> f b
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
instance Functor [] where
    fmap = map

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
instance Monad Maybe where
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x
    return = Just
instance Monad [] where
    xs >>= f = concat (map f xs)
    return x = [x]


both :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
both f g (x, y) = (f x, g y)

--class Foldable t where
--    foldr :: (a -> b -> b) -> b -> t a -> b
--    foldl :: (b -> a -> b) -> b -> t a -> b
--    null :: t a -> Bool
--    toList :: t a -> [a]
--instance Foldable [] where
--    foldr _ e [] = e
--    foldr f e (x:xs) = f x (foldr f e xs)
--    foldl _ e [] = e
--    foldl f e (x:xs) = foldl f (f e x) xs
--    null [] = True
--    null _ = False
--    toList = id
-- TODO(kc506)
-- Currently investigating why above causes error
foldl _ e [] = e
foldl f e (x:xs) = foldl f (f e x) xs

-- (++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

-- id :: a -> a
id x = x

-- (&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

-- (||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

-- not :: Bool -> Bool
not True = False
not False = True

-- all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = f x && all f xs

-- take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = error "take called on empty list"
take n (x:xs) = x:take (n-1 :: Int) xs

-- map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x):(map f xs)

-- concat :: [a] -> [a] -> [a]
concat = foldl (++) []

-- intercalate :: [a] -> [[a]] -> [a]
intercalate sep [] = []
intercalate sep [x] = x
intercalate sep (x:xs) = x ++ (sep ++ intercalate sep xs)

-- sum :: (Foldable t, Num a) => t a -> a
sum = foldl (+) 0

-- const :: a -> b -> a
const x _ = x

-- (.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

undefined :: a
compilerError :: a
error :: [Char] -> a