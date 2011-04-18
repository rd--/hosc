-- | Alegbraic data types for OSC packets and encode and decode
--   functions.
module Sound.OpenSoundControl.OSC.Encode ( encodeOSC ) where

import qualified Data.ByteString.Lazy as B
import Data.Word
import Sound.OpenSoundControl.OSC.Type
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Byte

-- Command argument types are given by a descriptor.
descriptor :: [Datum] -> Datum
descriptor l = String (',' : map tag l)

-- Align a byte string if required.
extend :: Word8 -> B.ByteString -> B.ByteString
extend p s = B.append s (B.replicate (align (B.length s)) p)

-- Encode an OSC datum.
encode_datum :: Datum -> B.ByteString
encode_datum (Int i) = encode_i32 i
encode_datum (Float f) = encode_f32 f
encode_datum (Double d) = encode_f64 d
encode_datum (TimeStamp t) = encode_u64 $ as_ntpi t
encode_datum (String s) = extend 0 (B.snoc (encode_str s) 0)
encode_datum (Midi (b0,b1,b2,b3)) = B.pack [b0,b1,b2,b3]
encode_datum (Blob b) = B.append (encode_i32 (fromIntegral (B.length b))) (extend 0 b)

-- Encode an OSC message.
encode_message :: String -> [Datum] -> B.ByteString
encode_message c l = 
    B.concat [ encode_datum (String c)
             , encode_datum (descriptor l)
             , B.concat (map encode_datum l) ]

-- Encode an OSC packet as an OSC blob.
encode_osc_blob :: OSC -> Datum
encode_osc_blob = Blob . encodeOSC

-- Encode an OSC bundle.
encode_bundle_ntpi :: NTPi -> [OSC] -> B.ByteString
encode_bundle_ntpi t l =
    B.concat [ bundleHeader
             , encode_u64 t
             , B.concat (map (encode_datum . encode_osc_blob) l) ]

-- | Encode an OSC packet.
encodeOSC :: OSC -> B.ByteString
encodeOSC (Message c l) = encode_message c l
encodeOSC (Bundle (NTPi t) l) = encode_bundle_ntpi t l
encodeOSC (Bundle (NTPr t) l) = encode_bundle_ntpi (ntpr_ntpi t) l
encodeOSC (Bundle (UTCr t) l) = encode_bundle_ntpi (utcr_ntpi t) l
