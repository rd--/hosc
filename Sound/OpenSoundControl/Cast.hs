module Sound.OpenSoundControl.Cast (f32_i32, i32_f32,
                                    f64_i64, i64_f64,
                                    str_cstr, str_pstr) where

import Data.Word(Word8)
import Data.Int (Int32, Int64)
import Data.Char (ord)
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

-- | C strings are null suffixed byte strings.
str_cstr :: String -> [Word8]
str_cstr s = map (fromIntegral . ord) s ++ [0]

-- | Pascal strings are length prefixed byte strings.
str_pstr :: String -> [Word8]
str_pstr s = (fromIntegral (length s)) : map (fromIntegral . ord) s

singletonArray :: (MArray a e m) => e -> m (a Int e)
singletonArray = newArray (0, 0::Int)

fromArray :: (MArray a e m) => a Int e -> m e
fromArray = flip readArray 0
