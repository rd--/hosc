-- | Bit-level type casts and byte layout string typecasts.
module Sound.Osc.Coding.Cast where

import Data.Char {- base -}
import Data.Word {- base -}

import Sound.Osc.Coding.Byte {- hosc3 -}
import Sound.Osc.Coding.Convert {- hosc -}

{- | The IEEE byte representation of a float.

>>> f32_w32 pi
1078530011

>>> f32_w32 (-7913907.5)
3404825447

>>> 23 ^ 7
3404825447
-}
f32_w32 :: Float -> Word32
f32_w32 = decode_word32 . encode_f32

{- | Inverse of 'f32_w32'.

>>> w32_f32 1078530011
3.1415927

>>> w32_f32 (23 ^ 7)
-7913907.5
-}
w32_f32 :: Word32 -> Float
w32_f32 = decode_f32 . encode_word32

{- | The IEEE byte representation of a double.


>>> f64_w64 pi
4614256656552045848

>>> f64_w64 1.6822072834e-314
3404825447
-}
f64_w64 :: Double -> Word64
f64_w64 = decode_word64 . encode_f64

{- | Inverse of 'f64_w64'.

>>> w64_f64 4614256656552045848
3.141592653589793

>>> w64_f64 (23 ^ 7)
1.6822072834e-314
-}
w64_f64 :: Word64 -> Double
w64_f64 = decode_f64 . encode_word64

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
