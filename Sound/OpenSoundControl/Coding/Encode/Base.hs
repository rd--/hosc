-- | Base-level encode function for OSC packets (slow).  For ordinary
--   use see 'Sound.OpenSoundControl.Coding.Encode.Builder'.
module Sound.OpenSoundControl.Coding.Encode.Base (encodeOSC) where

import qualified Data.ByteString.Lazy as B
import Data.Word
import Sound.OpenSoundControl.Coding.Byte
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Time

-- Command argument types are given by a descriptor.
descriptor :: [Datum] -> Datum
descriptor l = String (',' : map tag l)

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
      TimeStamp t -> encode_u64 $ as_ntpi t
      String s -> extend 0 (B.snoc (encode_str s) 0)
      Midi (b0,b1,b2,b3) -> B.pack [b0,b1,b2,b3]
      Blob b -> let n = encode_i32 (fromIntegral (B.length b))
                in B.append n (extend 0 b)

-- Encode an OSC message.
encode_message :: String -> [Datum] -> B.ByteString
encode_message c l =
    B.concat [encode_datum (String c)
             ,encode_datum (descriptor l)
             ,B.concat (map encode_datum l) ]

-- Encode an OSC packet as an OSC blob.
encode_osc_blob :: OSC -> Datum
encode_osc_blob = Blob . encodeOSC

-- Encode an OSC bundle.
encode_bundle_ntpi :: NTPi -> [OSC] -> B.ByteString
encode_bundle_ntpi t l =
    B.concat [bundleHeader
             ,encode_u64 t
             ,B.concat (map (encode_datum . encode_osc_blob) l) ]

-- | Encode an OSC packet.
encodeOSC :: OSC -> B.ByteString
encodeOSC o =
    case o of
      Message c l -> encode_message c l
      Bundle (NTPi t) l -> encode_bundle_ntpi t l
      Bundle (NTPr t) l -> encode_bundle_ntpi (ntpr_ntpi t) l
      Bundle (UTCr t) l -> encode_bundle_ntpi (utcr_ntpi t) l
