{-# LANGUAGE DeriveFunctor #-}
module Table where

import qualified Data.Vector as V
import Data.Vector ((!))

newtype Table a = Table { lookup :: Int -> a }
  deriving Functor

fromList :: a -> [a] -> Table a
fromList fallback items = Table f where
  vect = V.fromList items
  count = V.length vect
  f i | i < 0 = fallback
      | i >= count = fallback
      | otherwise = vect ! i

