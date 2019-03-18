-- | Base-level encode function for OSC packets (slow).
--   For ordinary use see 'Sound.OSC.Coding.Encode.Builder'.
module Sound.OSC.Coding.Encode.Base where

import Data.Binary {- base -}
import qualified Data.ByteString.Char8 as C {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}

import Sound.OSC.Coding.Byte {- hosc -}
import Sound.OSC.Coding.Convert {- hosc -}
import Sound.OSC.Datum {- hosc -}
import Sound.OSC.Packet {- hosc -}
import Sound.OSC.Time {- hosc -}

-- | Align byte string, if required.
extend :: Word8 -> B.ByteString -> B.ByteString
extend p s = B.append s (B.replicate (align (B.length s)) p)

-- | Encode OSC 'Datum'.
--
-- MIDI: Bytes from MSB to LSB are: port id, status byte, data1, data2.
encode_datum :: Datum -> B.ByteString
encode_datum dt =
    case dt of
      Int32 i -> encode i
      Int64 i -> encode i
      Float f -> encode_f32 f
      Double d -> encode_f64 d
      TimeStamp t -> encode_word64 $ ntpr_to_ntpi t
      ASCII_String s -> extend 0 (B.snoc (encode_ascii s) 0)
      Midi (MIDI b0 b1 b2 b3) -> B.pack [b0,b1,b2,b3]
      Blob b -> let n = encode (int64_to_int32 (B.length b))
                in B.append n (extend 0 b)

-- | Encode OSC 'Message'.
encodeMessage :: Message -> B.ByteString
encodeMessage (Message c l) =
    B.concat [encode_datum (ASCII_String (C.pack c))
             ,encode_datum (ASCII_String (descriptor l))
             ,B.concat (map encode_datum l) ]

-- | Encode OSC 'Message' as an OSC blob.
encode_message_blob :: Message -> Datum
encode_message_blob = Blob . encodeMessage

-- | Encode OSC 'Bundle'.
encodeBundle :: Bundle -> B.ByteString
encodeBundle (Bundle t m) =
    B.concat [bundleHeader
             ,encode_word64 (ntpr_to_ntpi t)
             ,B.concat (map (encode_datum . encode_message_blob) m)]

-- | Encode OSC 'Packet'.
encodePacket :: Packet -> B.ByteString
encodePacket o =
    case o of
      Packet_Message m -> encodeMessage m
      Packet_Bundle b -> encodeBundle b
