-- | Type conversion.
module Sound.OSC.Coding.Convert where

import Data.Int {- base -}
import Data.Word {- base -}

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
int8_to_int :: Int8 -> Int
int8_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_int :: Int16 -> Int
int16_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_int :: Int32 -> Int
int32_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_int :: Word8 -> Int
word8_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_int :: Word16 -> Int
word16_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_int :: Word32 -> Int
word32_to_int = fromIntegral

