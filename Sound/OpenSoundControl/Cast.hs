-- | Bit-level type casts and byte layout string typecasts.
module Sound.OpenSoundControl.Cast (f32_i32, i32_f32,
                                    f64_i64, i64_f64) where

import Control.Monad.ST
import Data.Array.ST
import Data.Int

-- | The IEEE byte representation of a float packed into an integer.
f32_i32 :: Float -> Int32
f32_i32 d = runST ((from_array =<< castSTUArray =<< unit_array d) :: ST s Int32)

-- | Inverse of 'f32_i32'.
i32_f32 :: Int32 -> Float
i32_f32 d = runST ((from_array =<< castSTUArray =<< unit_array d) :: ST s Float)

-- | The IEEE byte representation of a double packed into an integer.
f64_i64 :: Double -> Int64
f64_i64 d = runST ((from_array =<< castSTUArray =<< unit_array d) :: ST s Int64)

{-
http://www.haskell.org/pipermail/haskell-cafe/2008-February/040000.html

{-# LANGUAGE MagicHash #-}
import Data.Int
import GHC.Exts

f64_i64 :: Double -> Int64 --ghc only
f64_i64 (D# x) = I64# (unsafeCoerce# x)
-}

-- | Inverse of 'f64_i64'.
i64_f64 :: Int64 -> Double
i64_f64 d = runST ((from_array =<< castSTUArray =<< unit_array d) :: ST s Double)

-- One element marray.
unit_array :: (MArray a e m) => e -> m (a Int e)
unit_array = newArray (0, 0::Int)

-- Extract first element from array.
from_array :: (MArray a e m) => a Int e -> m e
from_array = flip readArray 0
