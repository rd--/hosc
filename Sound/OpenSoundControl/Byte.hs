module Sound.OpenSoundControl.Byte where

import Data.Int
import Data.Char
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Sound.OpenSoundControl.Cast

encode_i8 :: Int -> B.ByteString
encode_i8 n = encode (fromIntegral n :: Int8)

encode_i16 :: Int -> B.ByteString
encode_i16 n = encode (fromIntegral n :: Int16)

encode_i32 :: Int -> B.ByteString
encode_i32 n = encode (fromIntegral n :: Int32)

encode_u32 :: Int -> B.ByteString
encode_u32 n = encode (fromIntegral n :: Word32)

encode_i64 :: Integer -> B.ByteString
encode_i64 n = encode (fromIntegral n :: Int64)

encode_u64 :: Integer -> B.ByteString
encode_u64 n = encode (fromIntegral n :: Word64)

encode_f32 :: Double -> B.ByteString
encode_f32 n = encode (f32_i32 (realToFrac n))

encode_f64 :: Double -> B.ByteString
encode_f64 n = encode (f64_i64 n)

encode_str :: String -> B.ByteString
encode_str s = B.pack (map (fromIntegral . ord) s)

decode_i8 :: B.ByteString -> Int
decode_i8 b = fromIntegral (decode b :: Int8)

decode_i16 :: B.ByteString -> Int
decode_i16 b = fromIntegral (decode b :: Int16)

decode_i32 :: B.ByteString -> Int
decode_i32 b = fromIntegral (decode b :: Int32)

decode_u32 :: B.ByteString -> Int
decode_u32 b = fromIntegral (decode b :: Word32)

decode_i64 :: B.ByteString -> Integer
decode_i64 b = fromIntegral (decode b :: Int64)

decode_u64 :: B.ByteString -> Integer
decode_u64 b = fromIntegral (decode b :: Word64)

decode_f32 :: B.ByteString -> Double
decode_f32 b = realToFrac (i32_f32 (decode b :: Int32))

decode_f64 :: B.ByteString -> Double
decode_f64 b = i64_f64 (decode b :: Int64)

decode_str :: B.ByteString -> String
decode_str b = map (chr . fromIntegral) (B.unpack b)
