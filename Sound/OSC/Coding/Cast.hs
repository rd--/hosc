-- | Bit-level type casts and byte layout string typecasts.
module Sound.OSC.Coding.Cast where

import Data.Char {- base -}
import Data.Word {- base -}

import qualified Data.Binary.IEEE754 as I {- data-binary-ieee754 -}

import Sound.OSC.Coding.Convert {- hosc -}

-- | The IEEE byte representation of a float.
f32_w32 :: Float -> Word32
f32_w32 = I.floatToWord

-- | Inverse of 'f32_w32'.
w32_f32 :: Word32 -> Float
w32_f32 = I.wordToFloat

-- | The IEEE byte representation of a double.
f64_w64 :: Double -> Word64
f64_w64 = I.doubleToWord

-- | Inverse of 'f64_i64'.
w64_f64 :: Word64 -> Double
w64_f64 = I.wordToDouble

-- | Transform a haskell string into a C string (a null suffixed byte string).
str_cstr :: String -> [Word8]
str_cstr s = map (int_to_word8 . ord) s ++ [0]

-- | Inverse of 'str_cstr'.
cstr_str :: [Word8] -> String
cstr_str = map (chr . word8_to_int) . takeWhile (/= 0)

-- | Transform a haskell string to a pascal string (a length prefixed byte string).
str_pstr :: String -> [Word8]
str_pstr s = int_to_word8 (length s) : map (int_to_word8 . ord) s

-- | Inverse of 'str_pstr'.
pstr_str :: [Word8] -> String
pstr_str = map (chr . word8_to_int) . drop 1
