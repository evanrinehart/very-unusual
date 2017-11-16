{-# LANGUAGE DeriveFunctor #-}
module Matrix where

import qualified Data.Vector as V
import Data.Vector ((!))

import Int2
import Table

newtype Matrix a = Matrix { lookup :: Int2 -> a }
  deriving Functor

fromList :: a -> [[a]] -> Matrix a
fromList fallback rows = Matrix f where
  rowsVect = V.fromList (map V.fromList rows)
  f (I2 i j)
    | i < 0 || j < 0 = fallback
    | j >= V.length rowsVect = fallback
    | otherwise = let row = rowsVect ! j in
        if i >= V.length row then fallback else row ! i
