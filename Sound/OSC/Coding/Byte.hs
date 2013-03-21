-- | Byte-level coding utility functions.
module Sound.OSC.Coding.Byte where

import Data.Binary {- base -}
import Data.Bits {- base -}
import qualified Data.ByteString.Char8 as S {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as C {- bytestring -}
import Data.Int {- base -}

import Sound.OSC.Coding.Cast
import Sound.OSC.Type

-- | Encode a signed 8-bit integer.
encode_i8 :: Int -> B.ByteString
encode_i8 n = encode (fromIntegral n :: Int8)

-- | Encode a signed 16-bit integer.
encode_i16 :: Int -> B.ByteString
encode_i16 n = encode (fromIntegral n :: Int16)

-- | Encode a signed 32-bit integer.
encode_i32 :: Int -> B.ByteString
encode_i32 n = encode (fromIntegral n :: Int32)

-- | Encode an unsigned 16-bit integer.
encode_u32 :: Int -> B.ByteString
encode_u32 n = encode (fromIntegral n :: Word32)

-- | Encode a signed 64-bit integer.
encode_i64 :: Int64 -> B.ByteString
encode_i64 = encode

-- | Encode an unsigned 64-bit integer.
encode_u64 :: Word64 -> B.ByteString
encode_u64 = encode

-- | Encode a 32-bit IEEE floating point number.
encode_f32 :: Float -> B.ByteString
encode_f32 = encode . f32_w32

-- | Encode a 64-bit IEEE floating point number.
encode_f64 :: Double -> B.ByteString
encode_f64 = encode . f64_w64

-- | Encode an ASCII string.
encode_str :: ASCII -> B.ByteString
{-# INLINE encode_str #-}
encode_str (ASCII s) = B.pack s

-- | Decode a signed 8-bit integer.
decode_i8 :: B.ByteString -> Int
decode_i8 b = fromIntegral (decode b :: Int8)

-- | Decode a signed 16-bit integer.
decode_i16 :: B.ByteString -> Int
decode_i16 b = fromIntegral (decode b :: Int16)

-- | Decode a signed 32-bit integer.
decode_i32 :: B.ByteString -> Int
decode_i32 b = fromIntegral (decode b :: Int32)

-- | Decode an unsigned 32-bit integer.
decode_u32 :: B.ByteString -> Int
decode_u32 b = fromIntegral (decode b :: Word32)

-- | Decode a signed 64-bit integer.
decode_i64 :: B.ByteString -> Int64
decode_i64 = decode

-- | Decode an unsigned 64-bit integer.
decode_u64 :: B.ByteString -> Word64
decode_u64 = decode

-- | Decode a 32-bit IEEE floating point number.
decode_f32 :: B.ByteString -> Float
decode_f32 b = w32_f32 (decode b :: Word32)

-- | Decode a 64-bit IEEE floating point number.
decode_f64 :: B.ByteString -> Double
decode_f64 b = w64_f64 (decode b :: Word64)

-- | Decode an ASCII string.
decode_str :: B.ByteString -> ASCII
{-# INLINE decode_str #-}
decode_str = ASCII . B.unpack

-- | Bundle header as a strict ByteString.
bundleHeader_strict :: S.ByteString
bundleHeader_strict = S.pack "#bundle\0"

-- | Bundle header as a lazy ByteString.
bundleHeader :: B.ByteString
{-# INLINE bundleHeader #-}
bundleHeader = C.fromChunks [bundleHeader_strict]

-- | The number of bytes required to align an OSC value to the next
--   4-byte boundary.
--
-- > map align [0::Int .. 7] == [0,3,2,1,0,3,2,1]
align :: (Num i,Bits i) => i -> i
{-# INLINE align #-}
align n = ((n + 3) .&. complement 3) - n
