-- | Byte-level coding utility functions.
--   Plain forms are big-endian, little-endian forms have @_le@ suffix.
module Sound.OSC.Coding.Byte where

import Data.Bits {- base -}
import Data.Int {- base -}
import Data.Word {- base -}
import System.IO {- base -}

import qualified Data.Binary as Binary {- binary -}
import qualified Data.Binary.Get as Get {- binary -}
import qualified Data.Binary.Put as Put {- binary -}
import qualified Data.ByteString as S {- bytestring -}
import qualified Data.ByteString.Char8 as S.C {- bytestring -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as L.C {- bytestring -}

import qualified Sound.OSC.Coding.Cast as Cast {- hosc -}
import Sound.OSC.Coding.Convert {- hosc -}

-- * Encode

-- | Encode a signed 8-bit integer.
encode_i8 :: Int -> L.ByteString
encode_i8 = Binary.encode . int_to_int8

-- | Encode an un-signed 8-bit integer.
encode_u8 :: Int -> L.ByteString
encode_u8 = Binary.encode . int_to_word8

-- | Type specialised 'Binary.encode'.
--
-- > encode_w16 0x0102 == L.pack [1,2]
encode_w16 :: Word16 -> L.ByteString
encode_w16 = Binary.encode

-- | Little-endian.
--
-- > encode_w16_le 0x0102 == L.pack [2,1]
encode_w16_le :: Word16 -> L.ByteString
encode_w16_le = Put.runPut . Put.putWord16le

-- | Encode an un-signed 16-bit integer.
--
-- > encode_u16 0x0102 == L.pack [1,2]
encode_u16 :: Int -> L.ByteString
encode_u16 = encode_w16 . int_to_word16

-- | Little-endian.
--
-- > encode_u16_le 0x0102 == L.pack [2,1]
encode_u16_le :: Int -> L.ByteString
encode_u16_le = encode_w16_le . int_to_word16

-- | Encode a signed 16-bit integer.
encode_i16 :: Int -> L.ByteString
encode_i16 = Binary.encode . int_to_int16

-- | Encode a signed 32-bit integer.
encode_i32 :: Int -> L.ByteString
encode_i32 = Binary.encode . int_to_int32

-- | Type specialised 'Binary.encode'.
encode_w32 :: Word32 -> L.ByteString
encode_w32 = Binary.encode

-- | Encode an unsigned 32-bit integer.
--
-- > encode_u32 0x01020304 == L.pack [1,2,3,4]
encode_u32 :: Int -> L.ByteString
encode_u32 = encode_w32 . int_to_word32

-- | Little-endian variant of 'encode_w32'.
encode_w32_le :: Word32 -> L.ByteString
encode_w32_le = Put.runPut . Put.putWord32le

-- | Little-endian.
--
-- > encode_u32_le 0x01020304 == L.pack [4,3,2,1]
encode_u32_le :: Int -> L.ByteString
encode_u32_le = encode_w32_le . int_to_word32

-- | Encode a signed 64-bit integer.
encode_i64 :: Int64 -> L.ByteString
encode_i64 = Binary.encode

-- | Encode an unsigned 64-bit integer.
encode_u64 :: Word64 -> L.ByteString
encode_u64 = Binary.encode

-- | Encode a 32-bit IEEE floating point number.
encode_f32 :: Float -> L.ByteString
encode_f32 = Binary.encode . Cast.f32_w32

-- | Little-endian variant of 'encode_f32'.
encode_f32_le :: Float -> L.ByteString
encode_f32_le = Put.runPut . Put.putWord32le . Cast.f32_w32

-- | Encode a 64-bit IEEE floating point number.
encode_f64 :: Double -> L.ByteString
encode_f64 = Binary.encode . Cast.f64_w64

-- | Encode an ASCII string (ASCII at Datum is an alias for a Char8 Bytetring).
encode_str :: S.C.ByteString -> L.ByteString
{-# INLINE encode_str #-}
encode_str = L.pack . S.unpack

-- * Decode

-- | Decode an un-signed 8-bit integer.
decode_u8 :: L.ByteString -> Int
decode_u8 = word8_to_int . L.head

-- | Decode a signed 8-bit integer.
decode_i8 :: L.ByteString -> Int
decode_i8 = int8_to_int . Binary.decode

-- | Type specialised 'Binary.decode'.
decode_word16 :: L.ByteString -> Word16
decode_word16 = Binary.decode

-- | Decode an unsigned 8-bit integer.
decode_u16 :: L.ByteString -> Int
decode_u16 = word16_to_int . decode_word16

-- | Little-endian variant of 'decode_word16'.
decode_word16_le :: L.ByteString -> Word16
decode_word16_le = Get.runGet Get.getWord16le

-- | Little-endian variant of 'decode_u16'.
decode_u16_le :: L.ByteString -> Int
decode_u16_le = word16_to_int . decode_word16_le

-- | Type specialised 'Binary.decode'.
decode_int16 :: L.ByteString -> Int16
decode_int16 = Binary.decode

-- | Decode a signed 16-bit integer.
decode_i16 :: L.ByteString -> Int
decode_i16 = int16_to_int . decode_int16

-- | Little-endian variant of 'decode_i16'.
decode_i16_le :: L.ByteString -> Int
decode_i16_le = decode_i16 . L.reverse

-- | Decode a signed 32-bit integer.
decode_i32 :: L.ByteString -> Int
decode_i32 = int32_to_int . Binary.decode

-- | Type specialised 'Binary.decode'.
decode_word32 :: L.ByteString -> Word32
decode_word32 = Binary.decode

-- | Decode an unsigned 32-bit integer.
--
-- > decode_u32 (L.pack [1,2,3,4]) == 0x01020304
decode_u32 :: L.ByteString -> Int
decode_u32 = word32_to_int . decode_word32

-- | Little-endian variant of 'decode_word32'.
decode_word32_le :: L.ByteString -> Word32
decode_word32_le = Get.runGet Get.getWord32le

-- | Little-endian variant of decode_u32.
--
-- > decode_u32_le (L.pack [1,2,3,4]) == 0x04030201
decode_u32_le :: L.ByteString -> Int
decode_u32_le = word32_to_int . decode_word32_le

-- | Type specialised 'Binary.decode'.
decode_i64 :: L.ByteString -> Int64
decode_i64 = Binary.decode

-- | Type specialised 'Binary.decode'.
decode_u64 :: L.ByteString -> Word64
decode_u64 = Binary.decode

-- | Decode a 32-bit IEEE floating point number.
decode_f32 :: L.ByteString -> Float
decode_f32 = Cast.w32_f32 . decode_word32

-- | Little-endian variant of 'decode_f32'.
decode_f32_le :: L.ByteString -> Float
decode_f32_le = Cast.w32_f32 . decode_word32_le

-- | Decode a 64-bit IEEE floating point number.
decode_f64 :: L.ByteString -> Double
decode_f64 b = Cast.w64_f64 (Binary.decode b :: Word64)

-- | Decode an ASCII string, inverse of 'encode_str'.
decode_str :: L.ByteString -> S.C.ByteString
{-# INLINE decode_str #-}
decode_str = S.C.pack . L.C.unpack

-- * IO

-- | 'decode_i8' of 'L.hGet'.
read_i8 :: Handle -> IO Int
read_i8 = fmap decode_i8 . flip L.hGet 1

-- | 'decode_i16' of 'L.hGet'.
read_i16 :: Handle -> IO Int
read_i16 = fmap decode_i16 . flip L.hGet 2

-- | 'decode_i32' of 'L.hGet'.
read_i32 :: Handle -> IO Int
read_i32 = fmap decode_i32 . flip L.hGet 4

-- | 'decode_u32' of 'L.hGet'.
read_u32 :: Handle -> IO Int
read_u32 = fmap decode_u32 . flip L.hGet 4

-- | 'decode_u32_le' of 'L.hGet'.
read_u32_le :: Handle -> IO Int
read_u32_le = fmap decode_u32_le . flip L.hGet 4

-- | Read u8 length prefixed ASCII string (pascal string).
read_pstr :: Handle -> IO S.C.ByteString
read_pstr h = do
  n <- fmap decode_u8 (L.hGet h 1)
  fmap decode_str (L.hGet h n)

-- | 'L.hPut' of 'encode_u32'.
write_u32 :: Handle -> Int -> IO ()
write_u32 h = L.hPut h . encode_u32

-- | 'L.hPut' of 'encode_u32_le'.
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
