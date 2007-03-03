module Sound.OpenSoundControl.Cast (f32_i32, i32_f32, f64_i64, i64_f64) where

import Data.Int (Int32, Int64)
import Control.Monad.ST (runST, ST)
import Data.Array.ST (MArray, newArray, readArray, castSTUArray)

f32_i32 :: Float -> Int32
f32_i32 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Int32)

f64_i64 :: Double -> Int64
f64_i64 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Int64)

i32_f32 :: Int32 -> Float
i32_f32 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Float)

i64_f64 :: Int64 -> Double
i64_f64 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Double)

singletonArray :: (MArray a e m) => e -> m (a Int e)
singletonArray = newArray (0, 0::Int)

fromArray :: (MArray a e m) => a Int e -> m e
fromArray = flip readArray 0
