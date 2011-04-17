-- | Alegbraic data types for OSC packets and encode and decode
--   functions.
module Sound.OpenSoundControl.OSC.Decode ( decodeOSC ) where

import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import Sound.OpenSoundControl.OSC.Type
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Byte

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
decode_datum 'b' b = Blob (b_take n (B.drop 4 b)) where n = size 'b' b
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
    where h = storage 's' b -- header (should be '#bundle'
          t = storage 't' (b_drop h b) -- time tag
          (TimeStamp timeStamp) = decode_datum 't' (b_drop h b)
          ms = decode_message_seq $ b_drop (h+t) b

-- | Decode an OSC packet.
decodeOSC :: B.ByteString -> OSC
decodeOSC b | bundleHeader `B.isPrefixOf` b = decode_bundle b
            | otherwise = decode_message b

b_take :: Int -> B.ByteString -> B.ByteString
b_take = B.take . fromIntegral

b_drop :: Int -> B.ByteString -> B.ByteString
b_drop = B.drop . fromIntegral
