-- | Type conversion.
module Sound.OSC.Coding.Convert where

import Data.Int {- base -}
import Data.Word {- base -}

-- * Int -> N

-- | Type specialised 'fromIntegral'
int_to_word8 :: Int -> Word8
int_to_word8 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_word32 :: Int -> Word32
int_to_word32 = fromIntegral

-- | Type specialised 'fromIntegral'.
int_to_word16 :: Int -> Word16
int_to_word16 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_int8 :: Int -> Int8
int_to_int8 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_int16 :: Int -> Int16
int_to_int16 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_int32 :: Int -> Int32
int_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_int64 :: Int -> Int64
int_to_int64 = fromIntegral

-- * N -> Int

-- | Type specialised 'fromIntegral'
int8_to_int :: Int8 -> Int
int8_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_int :: Int16 -> Int
int16_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_int :: Int32 -> Int
int32_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
int64_to_int :: Int64 -> Int
int64_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_int :: Word8 -> Int
word8_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_int :: Word16 -> Int
word16_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_int :: Word32 -> Int
word32_to_int = fromIntegral

-- * N -> N

-- | Type specialised 'fromIntegral'
word16_to_word32 :: Word16 -> Word32
word16_to_word32 = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_word16 :: Word32 -> Word16
word32_to_word16 = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_int32 :: Word32 -> Int32
word32_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_int64 :: Word32 -> Int64
word32_to_int64 = fromIntegral

-- | Type specialised 'fromIntegral'
word64_to_int64 :: Word64 -> Int64
word64_to_int64 = fromIntegral

-- | Type specialised 'fromIntegral'
int64_to_int32 :: Int64 -> Int32
int64_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
int64_to_word32 :: Int64 -> Word32
int64_to_word32 = fromIntegral

-- * N -> Real

-- | Type specialised 'fromIntegral'
word64_to_double :: Word64 -> Double
word64_to_double = fromIntegral

-- * Enum

-- | Type-specialised 'toEnum' of 'fromIntegral'
word8_to_enum :: Enum e => Word8 -> e
word8_to_enum = toEnum . fromIntegral

-- | Type-specialised 'toEnum' of 'fromIntegral'
word16_to_enum :: Enum e => Word16 -> e
word16_to_enum = toEnum . fromIntegral

-- | Type-specialised 'fromIntegral' of 'fromEnum'.
enum_to_word8 :: Enum e => e -> Word8
enum_to_word8 = fromIntegral . fromEnum

-- | Type-specialised 'fromIntegral' of 'fromEnum'.
enum_to_word16 :: Enum e => e -> Word16
enum_to_word16 = fromIntegral . fromEnum

-- * Enum/Char

-- | Type-specialised 'word8_to_enum'.
word8_to_char :: Word8 -> Char
word8_to_char = word8_to_enum

-- | Type-specialised 'enum_to_word8'.
char_to_word8 :: Char -> Word8
char_to_word8 = enum_to_word8
