-- | Base-level decode function for Osc packets.
--   For ordinary use see 'Sound.Osc.Coding.Decode.Binary'.
module Sound.Osc.Coding.Decode.Base
  (decodeMessage
  ,decodeBundle
  ,decodePacket) where

import Data.Binary {- base -}
import qualified Data.ByteString.Char8 as C {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.List {- base -}
import Data.Maybe {- base -}

import Sound.Osc.Coding.Byte {- hosc -}
import Sound.Osc.Coding.Convert {- hosc -}
import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}
import Sound.Osc.Time {- hosc -}

-- | The plain byte count of an Osc value.
size :: DatumType -> B.ByteString -> Int
size ty b =
    case ty of
      'i' -> 4 -- Int32
      'f' -> 4 -- Float
      'd' -> 8 -- Double
      't' -> 8 -- Time (NTP)
      'm' -> 4 -- MIDI
      's' -> int64_to_int (fromMaybe
                           (error ("size: no terminating zero: " ++ show b))
                           (B.elemIndex 0 b))
      'b' -> decode_i32 (B.take 4 b)
      _ -> error "size: illegal type"

-- | The storage byte count (aligned) of an Osc value.
storage :: DatumType -> B.ByteString -> Int
storage ty b =
    case ty of
      's' -> let n = size 's' b + 1 in n + align n
      'b' -> let n = size 'b' b in n + align n + 4
      _ -> size ty B.empty

-- | Decode an Osc datum
decode_datum :: DatumType -> B.ByteString -> Datum
decode_datum ty b =
    case ty of
      'i' -> Int32 (decode b)
      'h' -> Int64 (decode b)
      'f' -> Float (decode_f32 b)
      'd' -> Double (decode_f64 b)
      's' -> AsciiString (decode_ascii (b_take (size 's' b) b))
      'b' -> Blob (b_take (size 'b' b) (B.drop 4 b))
      't' -> TimeStamp (ntpi_to_ntpr (decode_word64 b))
      'm' ->
        case B.unpack (B.take 4 b) of
          [b0,b1,b2,b3] -> midi (b0,b1,b2,b3)
          _ -> error "decode_datum: illegal midi data"
      _ -> error ("decode_datum: illegal type (" ++ [ty] ++ ")")

-- | Decode a sequence of Osc datum given a type descriptor string.
decode_datum_seq :: Ascii -> B.ByteString -> [Datum]
decode_datum_seq cs b =
    let swap (x,y) = (y,x)
        cs' = C.unpack cs
        f b' c = swap (B.splitAt (int_to_int64 (storage c b')) b')
    in zipWith decode_datum cs' (snd (mapAccumL f b cs'))

-- | Decode an Osc 'Message'.
decodeMessage :: B.ByteString -> Message
decodeMessage b =
    let n = storage 's' b
        cmd = decode_datum 's' b
        m = storage 's' (b_drop n b)
    in case (cmd, decode_datum 's' (b_drop n b)) of
         (AsciiString cmd', AsciiString dsc) ->
           let arg = decode_datum_seq (descriptor_tags dsc) (b_drop (n + m) b)
           in Message (C.unpack cmd') arg
         _ -> error "decodeMessage"

-- | Decode a sequence of length prefixed (Int32) Osc messages.
decode_message_seq :: B.ByteString -> [Message]
decode_message_seq b =
    let s = decode_i32 b
        m = decodeMessage (b_drop 4 b)
        nxt = decode_message_seq (b_drop (4+s) b)
    in if B.length b == 0 then [] else m:nxt

-- | Decode an Osc 'Bundle'.
decodeBundle :: B.ByteString -> BundleOf Message
decodeBundle b =
    let h = storage 's' b -- header (should be '#bundle')
        t = storage 't' (b_drop h b) -- time
    in case decode_datum 't' (b_drop h b) of
         TimeStamp timeStamp -> Bundle timeStamp (decode_message_seq (b_drop (h+t) b))
         _ -> error "decodeBundle"

{- | Decode an Osc 'Packet'.

>>> let b = B.pack [47,103,95,102,114,101,101,0,44,105,0,0,0,0,0,0]
>>> decodePacket b == Packet_Message (Message "/g_free" [Int32 0])
True
-}
decodePacket :: B.ByteString -> PacketOf Message
decodePacket b =
    if bundleHeader `B.isPrefixOf` b
    then Packet_Bundle (decodeBundle b)
    else Packet_Message (decodeMessage b)

-- * Util

-- | 'B.take' with 'Int' count.
b_take :: Int -> B.ByteString -> B.ByteString
b_take = B.take . int_to_int64

-- | 'B.drop' with 'Int' count.
b_drop :: Int -> B.ByteString -> B.ByteString
b_drop = B.drop . int_to_int64
