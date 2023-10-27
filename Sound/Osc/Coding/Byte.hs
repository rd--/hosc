{- | Byte-level coding utility functions.
Plain forms are big-endian, little-endian forms have @_le@ suffix.
-}
module Sound.Osc.Coding.Byte where

import Data.Bits {- base -}
import Data.Int {- base -}
import Data.Word {- base -}
import System.IO {- base -}

import qualified GHC.ByteOrder {- base -}

import qualified Data.Binary as Binary {- binary -}
import qualified Data.Binary.Get as Binary.Get {- binary -}
import qualified Data.Binary.Put as Binary.Put {- binary -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.ByteString.Char8 as ByteString.Char8 {- bytestring -}
import qualified Data.ByteString.Internal as ByteString.Internal {- bytestring -}
import qualified Data.ByteString.Lazy as ByteString.Lazy {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8 {- bytestring -}
import qualified Data.ByteString.Unsafe as ByteString.Unsafe {- bytestring -}

import qualified Sound.Osc.Coding.Cast as Cast {- hosc -}
import Sound.Osc.Coding.Convert {- hosc -}

-- * Encode

{- | Type specialised 'Binary.encode' (big-endian). -}
encode_int8 :: Int8 -> ByteString.Lazy.ByteString
encode_int8 = Binary.encode

{- | Type specialised 'Binary.encode' (big-endian).

>>> encode_int16 0x0102 == ByteString.Lazy.pack [0x01,0x02]
True
-}
encode_int16 :: Int16 -> ByteString.Lazy.ByteString
encode_int16 = Binary.encode

{- | Little-endian.

>>> encode_int16_le 0x0102 == ByteString.Lazy.pack [0x02,0x01]
True
-}
encode_int16_le :: Int16 -> ByteString.Lazy.ByteString
encode_int16_le = Binary.Put.runPut . Binary.Put.putInt16le

{- | Encode a signed 64-bit integer (big-endian). -}
encode_int64 :: Int64 -> ByteString.Lazy.ByteString
encode_int64 = Binary.encode

{- | Type specialised 'Binary.encode' (big-endian). -}
encode_word8 :: Word8 -> ByteString.Lazy.ByteString
encode_word8 = Binary.encode

{- | Type specialised 'Binary.encode' (big-endian).

>>> encode_word16 0x0102 == ByteString.Lazy.pack [0x01,0x02]
True
-}
encode_word16 :: Word16 -> ByteString.Lazy.ByteString
encode_word16 = Binary.encode

{- | Little-endian.

>>> encode_word16_le 0x0102 == ByteString.Lazy.pack [0x02,0x01]
True
-}
encode_word16_le :: Word16 -> ByteString.Lazy.ByteString
encode_word16_le = Binary.Put.runPut . Binary.Put.putWord16le

{- | Type specialised 'Binary.encode'. -}
encode_word32 :: Word32 -> ByteString.Lazy.ByteString
encode_word32 = Binary.encode

{- | Little-endian variant of 'encode_word32'. -}
encode_word32_le :: Word32 -> ByteString.Lazy.ByteString
encode_word32_le = Binary.Put.runPut . Binary.Put.putWord32le

{- | Encode an unsigned 64-bit integer. -}
encode_word64 :: Word64 -> ByteString.Lazy.ByteString
encode_word64 = Binary.encode

-- * Encode/Int

{- | Encode a signed 8-bit integer. -}
encode_i8 :: Int -> ByteString.Lazy.ByteString
encode_i8 = encode_int8 . int_to_int8

{- | Encode an un-signed 8-bit integer. -}
encode_u8 :: Int -> ByteString.Lazy.ByteString
encode_u8 = encode_word8 . int_to_word8

{- | Encode an un-signed 16-bit integer.

>>> encode_u16 0x0102 == ByteString.Lazy.pack [1,2]
True
-}
encode_u16 :: Int -> ByteString.Lazy.ByteString
encode_u16 = encode_word16 . int_to_word16

{- | Little-endian.

>>> encode_u16_le 0x0102 == ByteString.Lazy.pack [2,1]
True
-}
encode_u16_le :: Int -> ByteString.Lazy.ByteString
encode_u16_le = encode_word16_le . int_to_word16

{- | Encode a signed 16-bit integer. -}
encode_i16 :: Int -> ByteString.Lazy.ByteString
encode_i16 = Binary.encode . int_to_int16

{- | Encode a signed 32-bit integer. -}
encode_i32 :: Int -> ByteString.Lazy.ByteString
encode_i32 = Binary.encode . int_to_int32

{- | Encode an unsigned 32-bit integer.

>>> encode_u32 0x01020304 == ByteString.Lazy.pack [1,2,3,4]
True
-}
encode_u32 :: Int -> ByteString.Lazy.ByteString
encode_u32 = encode_word32 . int_to_word32

{- | Little-endian.

>>> encode_u32_le 0x01020304 == ByteString.Lazy.pack [4,3,2,1]
True
-}
encode_u32_le :: Int -> ByteString.Lazy.ByteString
encode_u32_le = encode_word32_le . int_to_word32

-- * Encode/Float

{- | Encode a 32-bit IEEE floating point number.

>>> encode_f32 1.0 == ByteString.Lazy.pack [63, 128, 0, 0]
True
-}
encode_f32 :: Float -> ByteString.Lazy.ByteString
encode_f32 = Binary.encode . Cast.f32_w32

{- | Little-endian variant of 'encode_f32'. -}
encode_f32_le :: Float -> ByteString.Lazy.ByteString
encode_f32_le = Binary.Put.runPut . Binary.Put.putWord32le . Cast.f32_w32

{- | Encode a 64-bit IEEE floating point number. -}
encode_f64 :: Double -> ByteString.Lazy.ByteString
encode_f64 = Binary.encode . Cast.f64_w64

{- | Little-endian variant of 'encode_f64'. -}
encode_f64_le :: Double -> ByteString.Lazy.ByteString
encode_f64_le = Binary.Put.runPut . Binary.Put.putWord64le . Cast.f64_w64

-- * Encode/Ascii

{- | Encode an Ascii string (Ascii at Datum is an alias for a Char8 Bytetring). -}
encode_ascii :: ByteString.Char8.ByteString -> ByteString.Lazy.ByteString
encode_ascii = ByteString.Lazy.pack . ByteString.unpack

-- * Decode

{- | Type specialised 'Binary.decode'. -}
decode_word16 :: ByteString.Lazy.ByteString -> Word16
decode_word16 = Binary.decode

{- | Little-endian variant of 'decode_word16'. -}
decode_word16_le :: ByteString.Lazy.ByteString -> Word16
decode_word16_le = Binary.Get.runGet Binary.Get.getWord16le

{- | Type specialised 'Binary.decode'. -}
decode_int16 :: ByteString.Lazy.ByteString -> Int16
decode_int16 = Binary.decode

{- | Type specialised 'Binary.decode'. -}
decode_word32 :: ByteString.Lazy.ByteString -> Word32
decode_word32 = Binary.decode

{- | Little-endian variant of 'decode_word32'. -}
decode_word32_le :: ByteString.Lazy.ByteString -> Word32
decode_word32_le = Binary.Get.runGet Binary.Get.getWord32le

{- | Type specialised 'Binary.decode'. -}
decode_int64 :: ByteString.Lazy.ByteString -> Int64
decode_int64 = Binary.decode

{- | Type specialised 'Binary.decode'. -}
decode_word64 :: ByteString.Lazy.ByteString -> Word64
decode_word64 = Binary.decode

-- * Decode/Int

{- | Decode an un-signed 8-bit integer. -}
decode_u8 :: ByteString.Lazy.ByteString -> Int
decode_u8 = word8_to_int . ByteString.Lazy.head

{- | Decode a signed 8-bit integer. -}
decode_i8 :: ByteString.Lazy.ByteString -> Int
decode_i8 = int8_to_int . Binary.decode

{- | Decode an unsigned 8-bit integer. -}
decode_u16 :: ByteString.Lazy.ByteString -> Int
decode_u16 = word16_to_int . decode_word16

{- | Little-endian variant of 'decode_u16'. -}
decode_u16_le :: ByteString.Lazy.ByteString -> Int
decode_u16_le = word16_to_int . decode_word16_le

{- | Decode a signed 16-bit integer. -}
decode_i16 :: ByteString.Lazy.ByteString -> Int
decode_i16 = int16_to_int . decode_int16

{- | Little-endian variant of 'decode_i16'. -}
decode_i16_le :: ByteString.Lazy.ByteString -> Int
decode_i16_le = decode_i16 . ByteString.Lazy.reverse

{- | Decode a signed 32-bit integer.

>>> decode_i32 (ByteString.Lazy.pack [0x00,0x00,0x03,0xe7]) == 0x03e7
True
-}
decode_i32 :: ByteString.Lazy.ByteString -> Int
decode_i32 = int32_to_int . Binary.decode

{- | Little-endian variant of 'decode_i32'.

>>> decode_i32_le (ByteString.Lazy.pack [0xe7,0x03,0x00,0x00]) == 0x03e7
True
-}
decode_i32_le :: ByteString.Lazy.ByteString -> Int
decode_i32_le = decode_i32 . ByteString.Lazy.reverse

{- | Decode an unsigned 32-bit integer.

>>> decode_u32 (ByteString.Lazy.pack [1,2,3,4]) == 0x01020304
True
-}
decode_u32 :: ByteString.Lazy.ByteString -> Int
decode_u32 = word32_to_int . decode_word32

{- | Little-endian variant of decode_u32.

>>> decode_u32_le (ByteString.Lazy.pack [1,2,3,4]) == 0x04030201
True
-}
decode_u32_le :: ByteString.Lazy.ByteString -> Int
decode_u32_le = word32_to_int . decode_word32_le

-- * Decode/Float

{- | Decode a 32-bit IEEE floating point number. -}
decode_f32 :: ByteString.Lazy.ByteString -> Float
decode_f32 = Cast.w32_f32 . decode_word32

{- | Little-endian variant of 'decode_f32'. -}
decode_f32_le :: ByteString.Lazy.ByteString -> Float
decode_f32_le = Cast.w32_f32 . decode_word32_le

{- | Decode a 64-bit IEEE floating point number. -}
decode_f64 :: ByteString.Lazy.ByteString -> Double
decode_f64 b = Cast.w64_f64 (Binary.decode b :: Word64)

-- * Decode/Ascii

{- | Decode an Ascii string, inverse of 'encode_ascii'. -}
decode_ascii :: ByteString.Lazy.ByteString -> ByteString.Char8.ByteString
{-# INLINE decode_ascii #-}
decode_ascii = ByteString.Char8.pack . ByteString.Lazy.Char8.unpack

-- * IO

{- | Read /n/ bytes from /h/ and run /f/. -}
read_decode :: (ByteString.Lazy.ByteString -> t) -> Int -> Handle -> IO t
read_decode f n = fmap f . flip ByteString.Lazy.hGet n

{- | Type-specialised reader for 'Binary.decode'. -}
read_word32 :: Handle -> IO Word32
read_word32 = read_decode Binary.decode 4

{- | 'read_decode' of 'decode_word32_le'. -}
read_word32_le :: Handle -> IO Word32
read_word32_le = read_decode decode_word32_le 4

{- | 'ByteString.Lazy.hPut' of 'encode_word32'. -}
write_word32 :: Handle -> Word32 -> IO ()
write_word32 h = ByteString.Lazy.hPut h . encode_word32

{- | 'ByteString.Lazy.hPut' of 'encode_word32_le'. -}
write_word32_le :: Handle -> Word32 -> IO ()
write_word32_le h = ByteString.Lazy.hPut h . encode_word32_le

-- * Io/Int

{- | 'decode_i8' of 'ByteString.Lazy.hGet'. -}
read_i8 :: Handle -> IO Int
read_i8 = read_decode decode_i8 1

{- | 'decode_i16' of 'ByteString.Lazy.hGet'. -}
read_i16 :: Handle -> IO Int
read_i16 = read_decode decode_i16 2

{- | 'decode_i32' of 'ByteString.Lazy.hGet'. -}
read_i32 :: Handle -> IO Int
read_i32 = read_decode decode_i32 4

{- | 'decode_i32_le' of 'ByteString.Lazy.hGet'. -}
read_i32_le :: Handle -> IO Int
read_i32_le = read_decode decode_i32_le 4

{- | 'decode_u32' of 'ByteString.Lazy.hGet'. -}
read_u32 :: Handle -> IO Int
read_u32 = read_decode decode_u32 4

{- | 'decode_u32_le' of 'ByteString.Lazy.hGet'. -}
read_u32_le :: Handle -> IO Int
read_u32_le = read_decode decode_u32_le 4

{- | 'ByteString.Lazy.hPut' of 'encode_u32'. -}
write_u32 :: Handle -> Int -> IO ()
write_u32 h = ByteString.Lazy.hPut h . encode_u32

{- | 'ByteString.Lazy.hPut' of 'encode_u32_le'. -}
write_u32_le :: Handle -> Int -> IO ()
write_u32_le h = ByteString.Lazy.hPut h . encode_u32_le

-- * Io/Float

{- | 'decode_f32' of 'ByteString.Lazy.hGet'. -}
read_f32 :: Handle -> IO Float
read_f32 = read_decode decode_f32 4

{- | 'decode_f32_le' of 'ByteString.Lazy.hGet'. -}
read_f32_le :: Handle -> IO Float
read_f32_le = read_decode decode_f32_le 4

-- * Io/Ascii

{- | Read u8 length prefixed Ascii string (pascal string). -}
read_pstr :: Handle -> IO ByteString.Char8.ByteString
read_pstr h = do
  n <- fmap decode_u8 (ByteString.Lazy.hGet h 1)
  fmap decode_ascii (ByteString.Lazy.hGet h n)

-- * Util

{- | Bundle header as a (strict) 'ByteString.Char8.ByteString'.

>>> ByteString.Char8.length bundleHeader_strict
8
-}
bundleHeader_strict :: ByteString.Char8.ByteString
bundleHeader_strict = ByteString.Char8.pack "#bundle\0"

{- | Bundle header as a lazy ByteString.

>>> ByteString.Lazy.length bundleHeader
8
-}
bundleHeader :: ByteString.Lazy.ByteString
{-# INLINE bundleHeader #-}
bundleHeader = ByteString.Lazy.Char8.fromChunks [bundleHeader_strict]

{- | The number of bytes required to align an Osc value to the next 4-byte boundary.

>>> map align [0::Int .. 7]
[0,3,2,1,0,3,2,1]

>>> map align [512::Int .. 519]
[0,3,2,1,0,3,2,1]
-}
align :: (Num i,Bits i) => i -> i
{-# INLINE align #-}
align n = ((n + 3) .&. complement 3) - n

-- * ByteString

{- | Is machine little endian? -}
isLittleEndian :: Bool
isLittleEndian = GHC.ByteOrder.targetByteOrder == GHC.ByteOrder.LittleEndian

{- | Byte-swap byte string in four-byte segments. -}
byteStringSwap32BitWords :: ByteString.ByteString -> ByteString.ByteString
byteStringSwap32BitWords xs =
  ByteString.Internal.unsafePackLenBytes
  (ByteString.length xs `quot` 4 * 4)
  [ ByteString.Unsafe.unsafeIndex xs $ i * 4 + j
  | i <- [0 .. ByteString.length xs `quot` 4 - 1]
  , j <- [3, 2, 1, 0]
  ]

{- | If target is little-endian, swap bytes to be in network order, else identity. -}
byteString32BitNetworkOrder :: ByteString.ByteString -> ByteString.ByteString
byteString32BitNetworkOrder x = if isLittleEndian then byteStringSwap32BitWords x else x
