-- | Alegbraic data types for OSC packets and encode and decode
--   functions.
module Sound.OpenSoundControl.OSC ( OSC(..)
                                  , Datum(..)
                                  , encodeOSC
                                  , decodeOSC ) where

import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import Data.Word
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.Cast

-- | The basic elements of OSC messages.
data Datum = Int Int
           | Float Double
           | Double Double
           | String String
           | Blob [Word8]
           | TimeStamp Time
             deriving (Eq, Show)

-- | An OSC packet.
data OSC = Message String [Datum]
         | Bundle Time [OSC]
           deriving (Eq, Show)

-- | OSC bundles can be ordered (time ascending).
instance Ord OSC where
    compare (Bundle a _) (Bundle b _) = compare a b
    compare _ _ = EQ

-- OSC types have single character identifiers.
tag :: Datum -> Char
tag (Int _) = 'i'
tag (Float _) = 'f'
tag (Double _) = 'd'
tag (String _) = 's'
tag (Blob _) = 'b'

-- Command argument types are given by a descriptor.
descriptor :: [Datum] -> Datum
descriptor l = String (',' : map tag l)

-- The number of bytes required to align an OSC value.
align :: Int -> Int
align n = (-n) `mod` 4

-- Align a byte string if required.
extend :: a -> [a] -> [a]
extend p s = s ++ replicate (align (length s)) p

-- Encode an OSC datum.
encode_datum :: Datum -> B.ByteString
encode_datum (Int i) = encode_i32 i
encode_datum (Float f) = encode_f32 f
encode_datum (Double d) = encode_f64 d
encode_datum (String s) = B.pack (extend 0 (str_cstr s))
encode_datum (Blob b) = B.concat [encode_i32 (length b), B.pack (extend 0 b)]

-- Encode an OSC message.
encode_message :: String -> [Datum] -> B.ByteString
encode_message c l = 
    B.concat [ encode_datum (String c)
             , encode_datum (descriptor l)
             , B.concat (map encode_datum l) ]

-- Encode an OSC packet as an OSC blob.
encode_osc_blob :: OSC -> Datum
encode_osc_blob = Blob . B.unpack . encodeOSC

-- Encode an OSC bundle.
encode_bundle_ntpi :: Integer -> [OSC] -> B.ByteString
encode_bundle_ntpi t l =
    B.concat [ bundle_header
             , encode_u64 t
             , B.concat (map (encode_datum . encode_osc_blob) l) ]

-- | Encode an OSC packet.
encodeOSC :: OSC -> B.ByteString
encodeOSC (Message c l) = encode_message c l
encodeOSC (Bundle (NTPi t) l) = encode_bundle_ntpi t l
encodeOSC (Bundle (NTPr t) l) = encode_bundle_ntpi (ntpr_ntpi t) l
encodeOSC (Bundle (UTCr t) l) = encode_bundle_ntpi (utcr_ntpi t) l

-- The plain byte count of an OSC value.
size :: Char -> B.ByteString -> Int
size 'i' _ = 4
size 'f' _ = 4
size 'd' _ = 8
size 't' _ = 8 -- timetag
size 's' b = fromIntegral (fromMaybe
                           (error ("size: no terminating zero: " ++ show b))
                           (B.elemIndex 0 b))
size 'b' b = decode_i32 (B.take 4 b)
size _ _ = error "size: illegal type"

-- The storage byte count of an OSC value.
storage :: Char -> B.ByteString -> Int
storage 's' b = n + align n where n = size 's' b + 1
storage 'b' b = n + align n + 4 where n = size 'b' b
storage c _ = size c B.empty

-- Decode an OSC datum
decode_datum :: Char -> B.ByteString -> Datum
decode_datum 'i' b = Int (decode_i32 b)
decode_datum 'f' b = Float (decode_f32 b)
decode_datum 'd' b = Double (decode_f64 b)
decode_datum 's' b = String (decode_str (b_take n b)) where n = size 's' b
decode_datum 'b' b = Blob (B.unpack (b_take n (B.drop 4 b))) where n = size 'b' b
decode_datum 't' b = TimeStamp $ NTPi (decode_u64 b)
decode_datum t _ = error ("decode_datum: illegal type (" ++ [t] ++ ")")

-- Decode a sequence of OSC datum given a type descriptor string.
decode_datum_seq :: [Char] -> B.ByteString -> [Datum]
decode_datum_seq cs b = zipWith decode_datum cs (snd (mapAccumL f b cs))
    where swap (x,y) = (y,x)
          f b' c = swap (B.splitAt (fromIntegral (storage c b')) b')

-- Decode an OSC message.
decode_message :: B.ByteString -> OSC
decode_message b = Message cmd arg
    where n = storage 's' b
          (String cmd) = decode_datum 's' b
          m = storage 's' (b_drop n b)
          (String dsc) = decode_datum 's' (b_drop n b)
          arg = decode_datum_seq (drop 1 dsc) (b_drop (n + m) b)

-- Decode a sequence of OSC messages, each one headed by its length
decode_message_seq :: B.ByteString -> [OSC]
decode_message_seq b | B.length b == 0 = []
                     | otherwise = m:nxt
                     where s = decode_i32 b
                           m = decode_message $ b_drop 4 b
                           nxt = decode_message_seq $ b_drop (4+s) b

decode_bundle :: B.ByteString -> OSC
decode_bundle b = Bundle timeStamp ms
    where h = storage 's' b -- header
          (String cmd) = decode_datum 's' b -- cmd should be #bundle
          t = storage 't' (b_drop h b) -- time tag
          (TimeStamp timeStamp) = decode_datum 't' (b_drop h b)
          ms = decode_message_seq $ b_drop (h+t) b

-- | Decode an OSC packet.
decodeOSC :: B.ByteString -> OSC
decodeOSC b | bundle_header `B.isPrefixOf` b = decode_bundle b
            | otherwise = decode_message b

b_take :: Int -> B.ByteString -> B.ByteString
b_take n = B.take (fromIntegral n)

b_drop :: Int -> B.ByteString -> B.ByteString
b_drop n = B.drop (fromIntegral n)

bundle_header = encode_datum (String "#bundle")

