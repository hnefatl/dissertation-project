module ExtraDefs where

import BasicPrelude hiding (intercalate)
import Language.Haskell.Pretty (Pretty, prettyPrint)
import Data.Text (intercalate, lines, unpack, pack)
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple (pString)
import TextShow (TextShow, showt)
import Data.Foldable (length, foldl', foldlM)
import qualified Data.Set as S
import qualified Data.Map as M

-- TODO(kc506): PR to `extra` to generalise these to foldables
allM, anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM f = foldlM (\x y -> (x &&) <$> f y) True
anyM f = foldlM (\x y -> (x ||) <$> f y) False

ifJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
ifJustM p action = p >>= maybe (return ()) action

pretty :: TextShow a => a -> Text
pretty = toStrict . pString . unpack . showt
synPrint :: Pretty a => a -> Text
synPrint = pack . prettyPrint

middleText :: (TextShow a, TextShow b) => Text -> a -> b -> Text
middleText m l r = showt l <> m <> showt r

-- |Check if the given function holds for each element in the same index in the given lists. Returns `False` if the
-- lists are different lengths.
pairwiseAndM :: Applicative f => (a -> b -> f Bool) -> [a] -> [b] -> f Bool
pairwiseAndM _ [] [] = pure True
pairwiseAndM f (x:xs) (y:ys) = (&&) <$> f x y <*> pairwiseAndM f xs ys
pairwiseAndM _ _ _ = pure False

containsDuplicates :: (Foldable f, Ord a) => f a -> Bool
containsDuplicates l = length l /= S.size (foldl' (flip S.insert) S.empty l)

deline :: Text -> Text
deline = intercalate " \\n " . lines

reverseLookup :: Ord v => v -> M.Map k v -> Maybe k
reverseLookup x = fmap fst . find ((x ==) . snd) . M.toList