module Sound.OpenSoundControl.Cast (f32_i32, i32_f32,
                                    f64_i64, i64_f64,
                                    str_cstr, cstr_str,
                                    str_pstr, pstr_str) where

import Control.Monad.ST
import Data.Array.ST
import Data.Char
import Data.Int
import Data.Word

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

-- | Transform a haskell string into a C string (a null suffixed byte string).
str_cstr :: String -> [Word8]
str_cstr s = map (fromIntegral . ord) s ++ [0]

-- | Inverse of 'str_cstr'.
cstr_str :: [Word8] -> String
cstr_str s = map (chr . fromIntegral) (takeWhile (/= 0) s)

-- | Transform a haskell string to a pascal string (a length prefixed byte string).
str_pstr :: String -> [Word8]
str_pstr s = (fromIntegral (length s)) : map (fromIntegral . ord) s

-- | Inverse of 'str_pstr'.
pstr_str :: [Word8] -> String
pstr_str s = map (chr . fromIntegral) (drop 1 s)

-- One element marray.
unit_array :: (MArray a e m) => e -> m (a Int e)
unit_array = newArray (0, 0::Int)

-- Extract first element from array.
from_array :: (MArray a e m) => a Int e -> m e
from_array = flip readArray 0
