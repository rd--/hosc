module Sound.OpenSoundControl.OSC
   (OSC(..),
    Datum(..),
    Encodable, encode,
    Decodable, decode,
    ) where

import Sound.OpenSoundControl.Time (utc_ntp)
import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.Cast

import Data.Word (Word8)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B

data Datum = Int Int
           | Float Double
           | Double Double
           | String String
           | Blob [Word8]
             deriving (Eq, Show)

data OSC = Message String [Datum]
         | Bundle Double [OSC]
           deriving (Eq, Show)

class Encodable a where
    encode :: a -> B.ByteString

class Decodable a where
    decode :: B.ByteString -> a

-- | OSC types have single character identifiers.
tag :: Datum -> Char
tag (Int _)    = 'i'
tag (Float _)  = 'f'
tag (Double _) = 'd'
tag (String _) = 's'
tag (Blob _)   = 'b'

-- | Command argument types are given by a descriptor.
descriptor :: [Datum] -> Datum
descriptor l = String (',' : map tag l)

-- | The number of bytes required to align an OSC value.
align :: Int -> Int
align n = mod (-n) 4

-- | Align a byte string if required.
extend :: a -> [a] -> [a]
extend p s = s ++ replicate (align (length s)) p

instance Encodable Datum where
    encode (Int i)    = encode_i32 i
    encode (Float f)  = encode_f32 f
    encode (Double d) = encode_f64 d
    encode (String s) = B.pack (extend 0 (str_cstr s))
    encode (Blob b)   = B.concat [encode_i32 (length b), B.pack (extend 0 b)]

instance Encodable OSC where
    encode (Message c l) = B.concat [encode (String c),
                                     encode (descriptor l),
                                     B.concat (map encode l)]
    encode (Bundle t l) = B.concat [encode (String "#bundle"),
                                    encode_u64 (utc_ntp t),
                                    B.concat (map (encode . Blob . B.unpack . encode) l)]

-- | The plain byte count of an OSC value.
size :: Char -> B.ByteString -> Int
size 'i' _ = 4
size 'f' _ = 4
size 'd' _ = 8
size 's' b = fromIntegral (fromMaybe 
                           (error ("no terminating zero found in " ++ show b))
                           (B.elemIndex 0 b))
size 'b' b = decode_i32 (B.take 4 b)
size _   _ = error "illegal osc type"

-- | The storage byte count of an OSC value.
storage :: Char -> B.ByteString -> Int
storage 's'  b = n + align n where n = size 's' b + 1
storage 'b'  b = n + align n + 4 where n = size 'b' b
storage c    _ = size c B.empty

decodeDatum :: Char -> B.ByteString -> Datum
decodeDatum 'i' b = Int (decode_i32 b)
decodeDatum 'f' b = Float (decode_f32 b)
decodeDatum 'd' b = Double (decode_f64 b)
decodeDatum 's' b = String (decode_str (take' n b)) where n = size 's' b
decodeDatum 'b' b = Blob (B.unpack (take' n (B.drop 4 b))) where n = size 'b' b
decodeDatum _   _ = error "illegal osc type"

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

decodeData :: [Char] -> B.ByteString -> [Datum]
decodeData cs b =
   zipWith decodeDatum cs $ snd $
   mapAccumL (\bRest c -> swap (B.splitAt (fromIntegral (storage c bRest)) bRest)) b cs

instance Decodable OSC where
    decode b = Message cmd arg
        where n            = storage 's' b
              (String cmd) = decodeDatum 's' b
              m            = storage 's' (drop' n b)
              (String dsc) = decodeDatum 's' (drop' n b)
              arg          = decodeData (drop 1 dsc) (drop' (n + m) b)

take' :: Int -> B.ByteString -> B.ByteString
take' n b = B.take (fromIntegral n) b

drop' :: Int -> B.ByteString -> B.ByteString
drop' n b = B.drop (fromIntegral n) b

