-- | Base-level encode function for Osc packets (slow).
--   For ordinary use see 'Sound.Osc.Coding.Encode.Builder'.
module Sound.Osc.Coding.Encode.Base where

import Data.Binary {- base -}
import qualified Data.ByteString.Char8 as C {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}

import Sound.Osc.Coding.Byte {- hosc -}
import Sound.Osc.Coding.Convert {- hosc -}
import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}
import Sound.Osc.Time {- hosc -}

-- | Align byte string, if required.
extend :: Word8 -> B.ByteString -> B.ByteString
extend p s = B.append s (B.replicate (align (B.length s)) p)

{- | Encode Osc 'Datum'.

MidiData: Bytes from MSB to LSB are: port id, status byte, data1, data2.

> encode_datum (blob [1, 2, 3, 4]) == B.pack [0, 0, 0, 4, 1, 2, 3, 4]
-}
encode_datum :: Datum -> B.ByteString
encode_datum dt =
    case dt of
      Int32 i -> encode i
      Int64 i -> encode i
      Float f -> encode_f32 f
      Double d -> encode_f64 d
      TimeStamp t -> encode_word64 $ ntpr_to_ntpi t
      AsciiString s -> extend 0 (B.snoc (encode_ascii s) 0)
      Midi (MidiData b0 b1 b2 b3) -> B.pack [b0,b1,b2,b3]
      Blob b -> let n = encode (int64_to_int32 (B.length b))
                in B.append n (extend 0 b)

{- | Encode Osc 'Message'.

> blob_unpack (encodeMessage (Message "/x" [])) == [47,120,0,0,44,0,0,0]
> blob_unpack (encodeMessage (Message "/y" [float 3.141])) == [47,121,0,0,44,102,0,0,64,73,6,37]

> m = Message "/n_set" [int32 (-1), string "freq", float 440, string "amp", float 0.1]
> e = blob_unpack (encodeMessage m)
> length e == 40
> e == [47,110,95,115,101,116,0,0,44,105,115,102,115,102,0,0,255,255,255,255,102,114,101,113,0,0,0,0,67,220,0,0,97,109,112,0,61,204,204,205]
-}
encodeMessage :: Message -> B.ByteString
encodeMessage (Message c l) =
    B.concat [encode_datum (AsciiString (C.pack c))
             ,encode_datum (AsciiString (descriptor l))
             ,B.concat (map encode_datum l) ]

-- | Encode Osc 'Message' as an Osc blob.
encode_message_blob :: Message -> Datum
encode_message_blob = Blob . encodeMessage

{- | Encode Osc 'Bundle'.

b = Bundle 0.0 [m]
e = blob_unpack (encodeBundle b)
length e == 60
e == [35,98,117,110,100,108,101,0,0,0,0,0,0,0,0,0,0,0,0,40,47,110,95,115,101,116,0,0,44,105,115,102,115,102,0,0,255,255,255,255,102,114,101,113,0,0,0,0,67,220,0,0,97,109,112,0,61,204,204,205]
-}
encodeBundle :: Bundle -> B.ByteString
encodeBundle (Bundle t m) =
    B.concat
    [bundleHeader
    ,encode_word64 (ntpr_to_ntpi t)
    ,B.concat (map (encode_datum . encode_message_blob) m)]

-- | Encode Osc 'Packet'.
encodePacket :: Packet -> B.ByteString
encodePacket o =
    case o of
      Packet_Message m -> encodeMessage m
      Packet_Bundle b -> encodeBundle b
