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

-- | Type specialised 'Binary.encode'.
encode_int8 :: Int8 -> L.ByteString
encode_int8 = Binary.encode

-- | Encode a signed 64-bit integer.
encode_int64 :: Int64 -> L.ByteString
encode_int64 = Binary.encode

-- | Type specialised 'Binary.encode'.
encode_word8 :: Word8 -> L.ByteString
encode_word8 = Binary.encode

-- | Type specialised 'Binary.encode'.
--
-- > encode_word16 0x0102 == L.pack [1,2]
encode_word16 :: Word16 -> L.ByteString
encode_word16 = Binary.encode

-- | Little-endian.
--
-- > encode_word16_le 0x0102 == L.pack [2,1]
encode_word16_le :: Word16 -> L.ByteString
encode_word16_le = Put.runPut . Put.putWord16le

-- | Type specialised 'Binary.encode'.
encode_word32 :: Word32 -> L.ByteString
encode_word32 = Binary.encode

-- | Little-endian variant of 'encode_word32'.
encode_word32_le :: Word32 -> L.ByteString
encode_word32_le = Put.runPut . Put.putWord32le

-- | Encode an unsigned 64-bit integer.
encode_word64 :: Word64 -> L.ByteString
encode_word64 = Binary.encode

-- * Encode/Int

-- | Encode a signed 8-bit integer.
encode_i8 :: Int -> L.ByteString
encode_i8 = encode_int8 . int_to_int8

-- | Encode an un-signed 8-bit integer.
encode_u8 :: Int -> L.ByteString
encode_u8 = encode_word8 . int_to_word8

-- | Encode an un-signed 16-bit integer.
--
-- > encode_u16 0x0102 == L.pack [1,2]
encode_u16 :: Int -> L.ByteString
encode_u16 = encode_word16 . int_to_word16

-- | Little-endian.
--
-- > encode_u16_le 0x0102 == L.pack [2,1]
encode_u16_le :: Int -> L.ByteString
encode_u16_le = encode_word16_le . int_to_word16

-- | Encode a signed 16-bit integer.
encode_i16 :: Int -> L.ByteString
encode_i16 = Binary.encode . int_to_int16

-- | Encode a signed 32-bit integer.
encode_i32 :: Int -> L.ByteString
encode_i32 = Binary.encode . int_to_int32

-- | Encode an unsigned 32-bit integer.
--
-- > encode_u32 0x01020304 == L.pack [1,2,3,4]
encode_u32 :: Int -> L.ByteString
encode_u32 = encode_word32 . int_to_word32

-- | Little-endian.
--
-- > encode_u32_le 0x01020304 == L.pack [4,3,2,1]
encode_u32_le :: Int -> L.ByteString
encode_u32_le = encode_word32_le . int_to_word32

-- * Encode/Float

-- | Encode a 32-bit IEEE floating point number.
encode_f32 :: Float -> L.ByteString
encode_f32 = Binary.encode . Cast.f32_w32

-- | Little-endian variant of 'encode_f32'.
encode_f32_le :: Float -> L.ByteString
encode_f32_le = Put.runPut . Put.putWord32le . Cast.f32_w32

-- | Encode a 64-bit IEEE floating point number.
encode_f64 :: Double -> L.ByteString
encode_f64 = Binary.encode . Cast.f64_w64

-- * Encode/ASCII

-- | Encode an ASCII string (ASCII at Datum is an alias for a Char8 Bytetring).
encode_ascii :: S.C.ByteString -> L.ByteString
encode_ascii = L.pack . S.unpack

-- * Decode

-- | Type specialised 'Binary.decode'.
decode_word16 :: L.ByteString -> Word16
decode_word16 = Binary.decode

-- | Little-endian variant of 'decode_word16'.
decode_word16_le :: L.ByteString -> Word16
decode_word16_le = Get.runGet Get.getWord16le

-- | Type specialised 'Binary.decode'.
decode_int16 :: L.ByteString -> Int16
decode_int16 = Binary.decode

-- | Type specialised 'Binary.decode'.
decode_word32 :: L.ByteString -> Word32
decode_word32 = Binary.decode

-- | Little-endian variant of 'decode_word32'.
decode_word32_le :: L.ByteString -> Word32
decode_word32_le = Get.runGet Get.getWord32le

-- | Type specialised 'Binary.decode'.
decode_int64 :: L.ByteString -> Int64
decode_int64 = Binary.decode

-- | Type specialised 'Binary.decode'.
decode_word64 :: L.ByteString -> Word64
decode_word64 = Binary.decode

-- * Decode/Int

-- | Decode an un-signed 8-bit integer.
decode_u8 :: L.ByteString -> Int
decode_u8 = word8_to_int . L.head

-- | Decode a signed 8-bit integer.
decode_i8 :: L.ByteString -> Int
decode_i8 = int8_to_int . Binary.decode

-- | Decode an unsigned 8-bit integer.
decode_u16 :: L.ByteString -> Int
decode_u16 = word16_to_int . decode_word16

-- | Little-endian variant of 'decode_u16'.
decode_u16_le :: L.ByteString -> Int
decode_u16_le = word16_to_int . decode_word16_le

-- | Decode a signed 16-bit integer.
decode_i16 :: L.ByteString -> Int
decode_i16 = int16_to_int . decode_int16

-- | Little-endian variant of 'decode_i16'.
decode_i16_le :: L.ByteString -> Int
decode_i16_le = decode_i16 . L.reverse

-- | Decode a signed 32-bit integer.
--
-- > decode_i32 (L.pack [0x00,0x00,0x03,0xe7]) == 0x03e7
decode_i32 :: L.ByteString -> Int
decode_i32 = int32_to_int . Binary.decode

-- | Little-endian variant of 'decode_i32'.
--
-- > decode_i32_le (L.pack [0xe7,0x03,0x00,0x00]) == 0x03e7
decode_i32_le :: L.ByteString -> Int
decode_i32_le = decode_i32 . L.reverse

-- | Decode an unsigned 32-bit integer.
--
-- > decode_u32 (L.pack [1,2,3,4]) == 0x01020304
decode_u32 :: L.ByteString -> Int
decode_u32 = word32_to_int . decode_word32

-- | Little-endian variant of decode_u32.
--
-- > decode_u32_le (L.pack [1,2,3,4]) == 0x04030201
decode_u32_le :: L.ByteString -> Int
decode_u32_le = word32_to_int . decode_word32_le

-- * Decode/Float

-- | Decode a 32-bit IEEE floating point number.
decode_f32 :: L.ByteString -> Float
decode_f32 = Cast.w32_f32 . decode_word32

-- | Little-endian variant of 'decode_f32'.
decode_f32_le :: L.ByteString -> Float
decode_f32_le = Cast.w32_f32 . decode_word32_le

-- | Decode a 64-bit IEEE floating point number.
decode_f64 :: L.ByteString -> Double
decode_f64 b = Cast.w64_f64 (Binary.decode b :: Word64)

-- * Decode/ASCII

-- | Decode an ASCII string, inverse of 'encode_ascii'.
decode_ascii :: L.ByteString -> S.C.ByteString
{-# INLINE decode_ascii #-}
decode_ascii = S.C.pack . L.C.unpack

-- * IO

-- | Read /n/ bytes from /h/ and run /f/.
read_decode :: (L.ByteString -> t) -> Int -> Handle -> IO t
read_decode f n = fmap f . flip L.hGet n

-- | Type-specialised reader for 'Binary.decode'.
read_word32 :: Handle -> IO Word32
read_word32 = read_decode Binary.decode 4

-- | 'read_decode' of 'decode_word32_le'.
read_word32_le :: Handle -> IO Word32
read_word32_le = read_decode decode_word32_le 4

-- | 'L.hPut' of 'encode_word32'.
write_word32 :: Handle -> Word32 -> IO ()
write_word32 h = L.hPut h . encode_word32

-- | 'L.hPut' of 'encode_word32_le'.
write_word32_le :: Handle -> Word32 -> IO ()
write_word32_le h = L.hPut h . encode_word32_le

-- * IO/Int

-- | 'decode_i8' of 'L.hGet'.
read_i8 :: Handle -> IO Int
read_i8 = read_decode decode_i8 1

-- | 'decode_i16' of 'L.hGet'.
read_i16 :: Handle -> IO Int
read_i16 = read_decode decode_i16 2

-- | 'decode_i32' of 'L.hGet'.
read_i32 :: Handle -> IO Int
read_i32 = read_decode decode_i32 4

-- | 'decode_i32_le' of 'L.hGet'.
read_i32_le :: Handle -> IO Int
read_i32_le = read_decode decode_i32_le 4

-- | 'decode_u32' of 'L.hGet'.
read_u32 :: Handle -> IO Int
read_u32 = read_decode decode_u32 4

-- | 'decode_u32_le' of 'L.hGet'.
read_u32_le :: Handle -> IO Int
read_u32_le = read_decode decode_u32_le 4

-- | 'L.hPut' of 'encode_u32'.
write_u32 :: Handle -> Int -> IO ()
write_u32 h = L.hPut h . encode_u32

-- | 'L.hPut' of 'encode_u32_le'.
write_u32_le :: Handle -> Int -> IO ()
write_u32_le h = L.hPut h . encode_u32_le

-- * IO/Float

-- | 'decode_f32' of 'L.hGet'.
read_f32 :: Handle -> IO Float
read_f32 = read_decode decode_f32 4

-- | 'decode_f32_le' of 'L.hGet'.
read_f32_le :: Handle -> IO Float
read_f32_le = read_decode decode_f32_le 4

-- * IO/ASCII

-- | Read u8 length prefixed ASCII string (pascal string).
read_pstr :: Handle -> IO S.C.ByteString
read_pstr h = do
  n <- fmap decode_u8 (L.hGet h 1)
  fmap decode_ascii (L.hGet h n)

-- * Util

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
