-- | Base-level decode function for OSC packets (slow).  For ordinary
--   use see 'Sound.OpenSoundControl.Coding.Decode.Binary'.
module Sound.OpenSoundControl.Coding.Decode.Base (decodeOSC) where

import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import Sound.OpenSoundControl.Coding.Byte
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Type

-- The plain byte count of an OSC value.
size :: Char -> B.ByteString -> Int
size ty b =
    case ty of
      'i' -> 4
      'f' -> 4
      'd' -> 8
      't' -> 8 -- timetag
      'm' -> 4 -- MIDI message
      's' -> fromIntegral (fromMaybe
                           (error ("size: no terminating zero: " ++ show b))
                           (B.elemIndex 0 b))
      'b' -> decode_i32 (B.take 4 b)
      _ -> error "size: illegal type"

-- The storage byte count of an OSC value.
storage :: Char -> B.ByteString -> Int
storage ty b =
    case ty of
      's' -> let n = size 's' b + 1 in n + align n
      'b' -> let n = size 'b' b in n + align n + 4
      _ -> size ty B.empty

-- Decode an OSC datum
decode_datum :: Char -> B.ByteString -> Datum
decode_datum ty b =
    case ty of
      'i' -> Int (decode_i32 b)
      'f' -> Float (decode_f32 b)
      'd' -> Double (decode_f64 b)
      's' -> String (decode_str (b_take (size 's' b) b))
      'b' -> Blob (b_take (size 'b' b) (B.drop 4 b))
      't' -> TimeStamp $ NTPi (decode_u64 b)
      'm' -> let [b0,b1,b2,b3] = B.unpack (B.take 4 b) in Midi (b0,b1,b2,b3)
      _ -> error ("decode_datum: illegal type (" ++ [ty] ++ ")")

-- Decode a sequence of OSC datum given a type descriptor string.
decode_datum_seq :: String -> B.ByteString -> [Datum]
decode_datum_seq cs b =
    let swap (x,y) = (y,x)
        f b' c = swap (B.splitAt (fromIntegral (storage c b')) b')
    in zipWith decode_datum cs (snd (mapAccumL f b cs))

-- Decode an OSC message.
decode_message :: B.ByteString -> OSC
decode_message b =
    let n = storage 's' b
        (String cmd) = decode_datum 's' b
        m = storage 's' (b_drop n b)
        (String dsc) = decode_datum 's' (b_drop n b)
        arg = decode_datum_seq (drop 1 dsc) (b_drop (n + m) b)
    in Message cmd arg

-- Decode a sequence of OSC messages, each one headed by its length
decode_message_seq :: B.ByteString -> [OSC]
decode_message_seq b =
    let s = decode_i32 b
        m = decode_message $ b_drop 4 b
        nxt = decode_message_seq $ b_drop (4+s) b
    in if B.length b == 0 then [] else m:nxt

decode_bundle :: B.ByteString -> OSC
decode_bundle b =
    let h = storage 's' b -- header (should be '#bundle')
        t = storage 't' (b_drop h b) -- time tag
        (TimeStamp timeStamp) = decode_datum 't' (b_drop h b)
        ms = decode_message_seq $ b_drop (h+t) b
    in Bundle timeStamp ms

-- | Decode an OSC packet.
decodeOSC :: B.ByteString -> OSC
decodeOSC b =
    if bundleHeader `B.isPrefixOf` b
    then decode_bundle b
    else decode_message b

b_take :: Int -> B.ByteString -> B.ByteString
b_take = B.take . fromIntegral

b_drop :: Int -> B.ByteString -> B.ByteString
b_drop = B.drop . fromIntegral
