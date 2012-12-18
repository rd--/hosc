-- | Base-level encode function for OSC packets (slow).  For ordinary
--   use see 'Sound.OpenSoundControl.Coding.Encode.Builder'.
module Sound.OpenSoundControl.Coding.Encode.Base (encodeMessage
                                                 ,encodeBundle
                                                 ,encodePacket) where

import qualified Data.ByteString.Lazy as B
import Data.Word
import Sound.OpenSoundControl.Coding.Byte
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Time

-- Command argument types are given by a descriptor.
descriptor :: [Datum] -> Datum
descriptor l = String (',' : map datum_tag l)

-- Align a byte string if required.
extend :: Word8 -> B.ByteString -> B.ByteString
extend p s = B.append s (B.replicate (align (B.length s)) p)

-- Encode an OSC datum.
encode_datum :: Datum -> B.ByteString
encode_datum dt =
    case dt of
      Int i -> encode_i32 i
      Float f -> encode_f32 f
      Double d -> encode_f64 d
      TimeStamp t -> encode_u64 $ ntpr_to_ntpi t
      String s -> extend 0 (B.snoc (encode_str s) 0)
      Midi (b0,b1,b2,b3) -> B.pack [b0,b1,b2,b3]
      Blob b -> let n = encode_i32 (fromIntegral (B.length b))
                in B.append n (extend 0 b)

-- | Encode an OSC 'Message'.
encodeMessage :: Message -> B.ByteString
encodeMessage (Message c l) =
    B.concat [encode_datum (String c)
             ,encode_datum (descriptor l)
             ,B.concat (map encode_datum l) ]

-- Encode an OSC 'Message' as an OSC blob.
encode_message_blob :: Message -> Datum
encode_message_blob = Blob . encodeMessage

-- | Encode an OSC 'Bundle'.
encodeBundle :: Bundle -> B.ByteString
encodeBundle (Bundle t m) =
    B.concat [bundleHeader
             ,encode_u64 (ntpr_to_ntpi t)
             ,B.concat (map (encode_datum . encode_message_blob) m)]

-- | Encode an OSC 'Packet'.
encodePacket :: Packet -> B.ByteString
encodePacket o =
    case o of
      Packet_Message m -> encodeMessage m
      Packet_Bundle b -> encodeBundle b
