module Sound.OpenSoundControl.U8v where

import Data.Word (Word8)
import Data.Bits (Bits, shiftL, shiftR, (.&.))
import Data.Char (chr, ord)
import System.IO (openFile, IOMode(..), hPutStr, hClose, hGetContents)
import Control.Exception (bracket)
import Sound.OpenSoundControl.Cast

type Float32 = Float
type Float64 = Double

type U8   = Word8

asU8 :: (Integral a) => a -> U8
asU8 n = fromIntegral n

byte :: (Integral a, Bits a) => Int -> a -> U8
byte n i = asU8 $ (shiftR i (n * 8)) .&. 0xFF

i8_u8v :: Int -> [U8]
i8_u8v n = [asU8 n]

i16_u8v :: Int -> [U8]
i16_u8v n = [byte 1 n, byte 0 n]

i32_u8v :: Int -> [U8]
i32_u8v n = [byte 3 n, byte 2 n, byte 1 n, byte 0 n]

i64_u8v :: Integer -> [U8]
i64_u8v n = [byte 7 n, byte 6 n, byte 5 n, byte 4 n,
             byte 3 n, byte 2 n, byte 1 n, byte 0 n]

u64_u8v :: Integer -> [U8]
u64_u8v = i64_u8v

f32_u8v :: Float32 -> [U8]
f32_u8v f = i32_u8v (fromIntegral $ f32_i32 f)

f64_f32 :: Float64 -> Float32
f64_f32 n = realToFrac n

f32_f64 :: Float32 -> Float64
f32_f64 n = realToFrac n

f64_u8v :: Float64 -> [U8]
f64_u8v f = i64_u8v (fromIntegral $ f64_i64 f)

str_u8v :: String -> [U8]
str_u8v s = map (asU8 . ord) s

u8v_str :: [U8] -> String
u8v_str s = map (chr . fromIntegral) s

shiftL' :: (Bits b) => U8 -> Int -> b
shiftL' n i = shiftL (fromIntegral n) i

u8v_i8 :: [U8] -> Int
u8v_i8 [a] = shiftL' a 0
u8v_i8 _   = error "illegal input"

u8v_i16 :: [U8] -> Int
u8v_i16 [b,a] = shiftL' b 8 + shiftL' a 0
u8v_i16 _     = error "illegal input"

u8v_i32 :: [U8] -> Int
u8v_i32 [d,c,b,a] = shiftL' d 24 + shiftL' c 16 + shiftL' b 8 + shiftL' a 0
u8v_i32 _         = error "illegal input"

u8v_i64 :: [U8] -> Integer
u8v_i64 [h,g,f,e,d,c,b,a] = shiftL' h 56 + shiftL' g 48 +
                            shiftL' f 40 + shiftL' e 32 +
                            shiftL' d 24 + shiftL' c 16 +
                            shiftL' b 8 + shiftL' a 0
u8v_i64 _                 = error "illegal input"

u8v_f32 :: [U8] -> Float32
u8v_f32 b = i32_f32 (fromIntegral $ u8v_i32 b)

u8v_f64 :: [U8] -> Float64
u8v_f64 b = i64_f64 (fromIntegral $ u8v_i64 b)

-- IO

u8vWrite :: FilePath -> [U8] -> IO ()
u8vWrite fn u = bracket (openFile fn WriteMode) hClose
                        (flip hPutStr (u8v_str u))

u8vRead :: FilePath -> IO [U8]
u8vRead fn = do h <- openFile fn ReadMode
                s <- hGetContents h
                return (str_u8v s)
