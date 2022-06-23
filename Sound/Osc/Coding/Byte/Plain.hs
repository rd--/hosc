-- | Byte-level coding utility functions.   This form does not require the binary library.
module Sound.Osc.Coding.Byte.Plain where

import Control.Exception (bracket) {- base -}
import Control.Monad.ST (runST, ST) {- base -}
import Data.Bits (Bits, shiftL, shiftR, (.&.)) {- base -}
import Data.Char (chr, ord) {- base -}
import Data.Int (Int32, Int64) {- base -}
import Data.Word (Word8, Word32, Word64) {- base -}
import System.IO (openFile, IOMode(..), hPutStr, hClose) {- base -}

import Data.Array.ST (MArray, newArray, readArray) {- array -}
import Data.Array.Unsafe (castSTUArray) {- array -}

-- | Fetch byte n of the value a (zero indexed).
byte :: (Integral a, Bits a) => Int -> a -> Word8
byte n i = fromIntegral ((shiftR i (n * 8)) .&. 0xFF)

i8_bytes :: Int -> [Word8]
i8_bytes n = [fromIntegral n]

i16_bytes :: Int -> [Word8]
i16_bytes n = [byte 1 n, byte 0 n]

i32_bytes :: Int -> [Word8]
i32_bytes n = [byte 3 n, byte 2 n, byte 1 n, byte 0 n]

u32_bytes :: Int -> [Word8]
u32_bytes = i32_bytes

i64_bytes :: Integer -> [Word8]
i64_bytes n = [byte 7 n, byte 6 n, byte 5 n, byte 4 n, byte 3 n, byte 2 n, byte 1 n, byte 0 n]

u64_bytes :: Integer -> [Word8]
u64_bytes = i64_bytes

cast_f32_i32 :: Float -> Int32
cast_f32_i32 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Int32)

-- > cast_f32_u32 3.141 == 1078527525
cast_f32_u32 :: Float -> Word32
cast_f32_u32 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Word32)

f32_bytes :: Float -> [Word8]
f32_bytes f = i32_bytes (fromIntegral (cast_f32_i32 f))

f64_f32 :: Double -> Float
f64_f32 n = realToFrac n

f32_f64 :: Float -> Double
f32_f64 n = realToFrac n

cast_f64_i64 :: Double -> Int64
cast_f64_i64 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Int64)

-- > cast_f64_u64 3.141 == 4614255322014802772
cast_f64_u64 :: Double -> Word64
cast_f64_u64 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Word64)

f64_bytes :: Double -> [Word8]
f64_bytes f = i64_bytes (fromIntegral (cast_f64_i64 f))

str_bytes :: String -> [Word8]
str_bytes s = map (fromIntegral . ord) s

-- | pstr = pascal string
pstr_bytes :: String -> [Word8]
pstr_bytes s = i8_bytes n ++ str_bytes s
    where n = length s

bytes_str :: [Word8] -> String
bytes_str s = map (chr . fromIntegral) s

shift_byte_left :: (Bits b, Num b) => Word8 -> Int -> b
shift_byte_left n i = shiftL (fromIntegral n) i

bytes_i8 :: [Word8] -> Int
bytes_i8 [a] = shift_byte_left a 0
bytes_i8 _ = error "illegal input"

bytes_i16 :: [Word8] -> Int
bytes_i16 [b,a] = shift_byte_left b 8 + shift_byte_left a 0
bytes_i16 _ = error "illegal input"

bytes_i32 :: [Word8] -> Int
bytes_i32 [d,c,b,a] = shift_byte_left d 24 + shift_byte_left c 16 + shift_byte_left b 8 + shift_byte_left a 0
bytes_i32 _ = error "illegal input"

bytes_i64 :: [Word8] -> Integer
bytes_i64 [h,g,f,e,d,c,b,a] =
  shift_byte_left h 56 + shift_byte_left g 48 +
  shift_byte_left f 40 + shift_byte_left e 32 +
  shift_byte_left d 24 + shift_byte_left c 16 +
  shift_byte_left b 8 + shift_byte_left a 0
bytes_i64 _ = error "illegal input"

cast_i32_f32 :: Int32 -> Float
cast_i32_f32 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Float)

-- > cast_u32_f32 1078527525 == 3.141
cast_u32_f32 :: Word32 -> Float
cast_u32_f32 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Float)

bytes_f32 :: [Word8] -> Float
bytes_f32 b = cast_i32_f32 (fromIntegral (bytes_i32 b))

cast_i64_f64 :: Int64 -> Double
cast_i64_f64 d = runST ((fromArray =<< castSTUArray =<< singletonArray d) :: ST s Double)

bytes_f64 :: [Word8] -> Double
bytes_f64 b = cast_i64_f64 (fromIntegral $ bytes_i64 b)

-- IO

bytesWrite :: FilePath -> [Word8] -> IO ()
bytesWrite fn u =
  bracket
  (openFile fn WriteMode)
  hClose
  (flip hPutStr (bytes_str u))

-- * Util

singletonArray :: (MArray a e m) => e -> m (a Int e)
singletonArray = newArray (0, 0::Int)

fromArray :: (MArray a e m) => a Int e -> m e
fromArray = flip readArray 0

{-
This is a non-Haskell98 signature

castInt :: (MArray (STUArray s) b (ST s),
            MArray (STUArray s) e (ST s)) =>
           e -> ST s b
castInt d =
   fromArray =<< castSTUArray =<< singletonArray d
-}
