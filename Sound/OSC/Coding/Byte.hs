-- | Byte-level coding utility functions.
module Sound.OSC.Coding.Byte where

import qualified Data.Binary as B {- binary -}
import Data.Bits {- base -}
import qualified Data.ByteString as S {- bytestring -}
import qualified Data.ByteString.Char8 as S.C {- bytestring -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as L.C {- bytestring -}
import Data.Int {- base -}
import Data.Word {- base -}

import Sound.OSC.Coding.Cast
import Sound.OSC.Type

-- | Encode a signed 8-bit integer.
encode_i8 :: Int -> L.ByteString
encode_i8 n = B.encode (fromIntegral n :: Int8)

-- | Encode an un-signed 8-bit integer.
encode_u8 :: Int -> L.ByteString
encode_u8 n = B.encode (fromIntegral n :: Word8)

-- | Encode a signed 16-bit integer.
encode_i16 :: Int -> L.ByteString
encode_i16 n = B.encode (fromIntegral n :: Int16)

-- | Encode a signed 32-bit integer.
encode_i32 :: Int -> L.ByteString
encode_i32 n = B.encode (fromIntegral n :: Int32)

-- | Encode an unsigned 16-bit integer.
encode_u32 :: Int -> L.ByteString
encode_u32 n = B.encode (fromIntegral n :: Word32)

-- | Encode a signed 64-bit integer.
encode_i64 :: Int64 -> L.ByteString
encode_i64 = B.encode

-- | Encode an unsigned 64-bit integer.
encode_u64 :: Word64 -> L.ByteString
encode_u64 = B.encode

-- | Encode a 32-bit IEEE floating point number.
encode_f32 :: Float -> L.ByteString
encode_f32 = B.encode . f32_w32

-- | Encode a 64-bit IEEE floating point number.
encode_f64 :: Double -> L.ByteString
encode_f64 = B.encode . f64_w64

-- | Encode an ASCII string.
encode_str :: ASCII -> L.ByteString
{-# INLINE encode_str #-}
encode_str = L.pack . S.unpack

-- | Decode an un-signed 8-bit integer.
decode_u8 :: L.ByteString -> Int
decode_u8 = fromIntegral . L.head

-- | Decode a signed 8-bit integer.
decode_i8 :: L.ByteString -> Int
decode_i8 b = fromIntegral (B.decode b :: Int8)

-- | Decode a signed 16-bit integer.
decode_i16 :: L.ByteString -> Int
decode_i16 b = fromIntegral (B.decode b :: Int16)

-- | Decode a signed 32-bit integer.
decode_i32 :: L.ByteString -> Int
decode_i32 b = fromIntegral (B.decode b :: Int32)

-- | Decode an unsigned 32-bit integer.
decode_u32 :: L.ByteString -> Int
decode_u32 b = fromIntegral (B.decode b :: Word32)

-- | Decode a signed 64-bit integer.
decode_i64 :: L.ByteString -> Int64
decode_i64 = B.decode

-- | Decode an unsigned 64-bit integer.
decode_u64 :: L.ByteString -> Word64
decode_u64 = B.decode

-- | Decode a 32-bit IEEE floating point number.
decode_f32 :: L.ByteString -> Float
decode_f32 b = w32_f32 (B.decode b :: Word32)

-- | Decode a 64-bit IEEE floating point number.
decode_f64 :: L.ByteString -> Double
decode_f64 b = w64_f64 (B.decode b :: Word64)

-- | Decode an ASCII string.
decode_str :: L.ByteString -> ASCII
{-# INLINE decode_str #-}
decode_str = S.C.pack . L.C.unpack

-- | Bundle header as a (strict) 'S.C.ByteString'.
bundleHeader_strict :: S.C.ByteString
bundleHeader_strict = S.C.pack "#bundle\0"

-- | Bundle header as a lazy ByteString.
bundleHeader :: L.ByteString
{-# INLINE bundleHeader #-}
bundleHeader = L.C.fromChunks [bundleHeader_strict]

-- | The number of bytes required to align an OSC value to the next
--   4-byte boundary.
--
-- > map align [0::Int .. 7] == [0,3,2,1,0,3,2,1]
align :: (Num i,Bits i) => i -> i
{-# INLINE align #-}
align n = ((n + 3) .&. complement 3) - n
