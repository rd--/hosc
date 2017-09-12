-- | Byte-level coding utility functions.
-- Plain forms are big-endian, little-endian forms have @_le@ suffix.
module Sound.OSC.Coding.Byte where

import qualified Data.Binary as B {- binary -}
import qualified Data.Binary.Get as B {- binary -}
import qualified Data.Binary.Put as B {- binary -}
import Data.Bits {- base -}
import qualified Data.ByteString as S {- bytestring -}
import qualified Data.ByteString.Char8 as S.C {- bytestring -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as L.C {- bytestring -}
import Data.Int {- base -}
import Data.Word {- base -}
import System.IO {- base -}

import Sound.OSC.Coding.Cast
import Sound.OSC.Datum

-- * Encode

-- | Encode a signed 8-bit integer.
encode_i8 :: Int -> L.ByteString
encode_i8 n = B.encode (fromIntegral n :: Int8)

-- | Encode an un-signed 8-bit integer.
encode_u8 :: Int -> L.ByteString
encode_u8 n = B.encode (fromIntegral n :: Word8)

encode_w16 :: Word16 -> L.ByteString
encode_w16 = B.encode

-- | Encode an un-signed 16-bit integer.
--
-- > encode_u16 0x0102 == L.pack [1,2]
encode_u16 :: Int -> L.ByteString
encode_u16 = encode_w16 . fromIntegral

encode_w16_le :: Word16 -> L.ByteString
encode_w16_le = B.runPut . B.putWord16le

-- | Little-endian.
--
-- > encode_u16_le 0x0102 == L.pack [2,1]
encode_u16_le :: Int -> L.ByteString
encode_u16_le = encode_w16_le . fromIntegral

-- | Encode a signed 16-bit integer.
encode_i16 :: Int -> L.ByteString
encode_i16 n = B.encode (fromIntegral n :: Int16)

-- | Encode a signed 32-bit integer.
encode_i32 :: Int -> L.ByteString
encode_i32 n = B.encode (fromIntegral n :: Int32)

encode_w32 :: Word32 -> L.ByteString
encode_w32 = B.encode

-- | Encode an unsigned 32-bit integer.
--
-- > encode_u32 0x01020304 == L.pack [1,2,3,4]
encode_u32 :: Int -> L.ByteString
encode_u32 = encode_w32 . fromIntegral

encode_w32_le :: Word32 -> L.ByteString
encode_w32_le = B.runPut . B.putWord32le

-- | Little-endian.
--
-- > encode_u32_le 0x01020304 == L.pack [4,3,2,1]
encode_u32_le :: Int -> L.ByteString
encode_u32_le = encode_w32_le . fromIntegral

-- | Encode a signed 64-bit integer.
encode_i64 :: Int64 -> L.ByteString
encode_i64 = B.encode

-- | Encode an unsigned 64-bit integer.
encode_u64 :: Word64 -> L.ByteString
encode_u64 = B.encode

-- | Encode a 32-bit IEEE floating point number.
encode_f32 :: Float -> L.ByteString
encode_f32 = B.encode . f32_w32

encode_f32_le :: Float -> L.ByteString
encode_f32_le = B.runPut . B.putWord32le . f32_w32

-- | Encode a 64-bit IEEE floating point number.
encode_f64 :: Double -> L.ByteString
encode_f64 = B.encode . f64_w64

-- | Encode an ASCII string.
encode_str :: ASCII -> L.ByteString
{-# INLINE encode_str #-}
encode_str = L.pack . S.unpack

-- * Decode

-- | Decode an un-signed 8-bit integer.
decode_u8 :: L.ByteString -> Int
decode_u8 = fromIntegral . L.head

-- | Decode a signed 8-bit integer.
decode_i8 :: L.ByteString -> Int
decode_i8 b = fromIntegral (B.decode b :: Int8)

decode_word16 :: L.ByteString -> Word16
decode_word16 = B.decode

-- | Decode an unsigned 8-bit integer.
decode_u16 :: L.ByteString -> Int
decode_u16 = fromIntegral . decode_word16

decode_word16_le :: L.ByteString -> Word16
decode_word16_le = B.runGet B.getWord16le

decode_u16_le :: L.ByteString -> Int
decode_u16_le = fromIntegral . decode_word16_le

decode_int16 :: L.ByteString -> Int16
decode_int16 = B.decode

-- | Decode a signed 16-bit integer.
decode_i16 :: L.ByteString -> Int
decode_i16 = fromIntegral . decode_int16

decode_i16_le :: L.ByteString -> Int
decode_i16_le = decode_i16 . L.reverse

-- | Decode a signed 32-bit integer.
decode_i32 :: L.ByteString -> Int
decode_i32 b = fromIntegral (B.decode b :: Int32)

decode_word32 :: L.ByteString -> Word32
decode_word32 = B.decode

-- | Decode an unsigned 32-bit integer.
--
-- > decode_u32 (L.pack [1,2,3,4]) == 0x01020304
decode_u32 :: L.ByteString -> Int
decode_u32 = fromIntegral . decode_word32

decode_word32_le :: L.ByteString -> Word32
decode_word32_le = B.runGet B.getWord32le

-- | Little-endian variant.
--
-- > decode_u32_le (L.pack [1,2,3,4]) == 0x04030201
decode_u32_le :: L.ByteString -> Int
decode_u32_le = fromIntegral . decode_word32_le

-- | Decode a signed 64-bit integer.
decode_i64 :: L.ByteString -> Int64
decode_i64 = B.decode

-- | Decode an unsigned 64-bit integer.
decode_u64 :: L.ByteString -> Word64
decode_u64 = B.decode

-- | Decode a 32-bit IEEE floating point number.
decode_f32 :: L.ByteString -> Float
decode_f32 = w32_f32 . decode_word32

decode_f32_le :: L.ByteString -> Float
decode_f32_le = w32_f32 . decode_word32_le

-- | Decode a 64-bit IEEE floating point number.
decode_f64 :: L.ByteString -> Double
decode_f64 b = w64_f64 (B.decode b :: Word64)

-- | Decode an ASCII string.
decode_str :: L.ByteString -> ASCII
{-# INLINE decode_str #-}
decode_str = S.C.pack . L.C.unpack

-- * IO

read_u32 :: Handle -> IO Int
read_u32 = fmap decode_u32 . flip L.hGet 4

read_u32_le :: Handle -> IO Int
read_u32_le = fmap decode_u32_le . flip L.hGet 4

write_u32 :: Handle -> Int -> IO ()
write_u32 h = L.hPut h . encode_u32

write_u32_le :: Handle -> Int -> IO ()
write_u32_le h = L.hPut h . encode_u32_le

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
