-- | Optimised encode function for Osc packets.
module Sound.Osc.Coding.Encode.Builder (
  build_packet,
  encodeMessage,
  encodeBundle,
  encodePacket,
  encodePacket_strict,
) where

import Data.Word {- base -}

import qualified Blaze.ByteString.Builder as B {- bytestring -}
import qualified Blaze.ByteString.Builder.Char8 as B {- bytestring -}
import qualified Data.ByteString as S {- bytestring -}
import qualified Data.ByteString.Lazy as L {- bytestring -}

import qualified Sound.Osc.Coding.Byte as Byte {- hosc -}
import qualified Sound.Osc.Coding.Cast as Cast {- hosc -}
import qualified Sound.Osc.Coding.Convert as Convert {- hosc -}
import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}
import Sound.Osc.Time {- hosc -}

-- | Generate a list of zero bytes for padding.
padding :: Int -> [Word8]
padding n = replicate n 0

-- | Nul byte (0) and then zero padding.
nul_and_padding :: Int -> B.Builder
nul_and_padding n = B.fromWord8s (0 : padding (Byte.align n))

-- Encode a string with zero padding.
build_ascii :: Ascii -> B.Builder
build_ascii s = B.fromByteString s <> nul_and_padding (S.length s + 1)

-- Encode a string with zero padding.
build_string :: String -> B.Builder
build_string s = B.fromString s <> nul_and_padding (length s + 1)

-- Encode a byte string with prepended length and zero padding.
build_bytes :: L.ByteString -> B.Builder
build_bytes s =
  B.fromInt32be (Convert.int64_to_int32 (L.length s))
    <> B.fromLazyByteString s
    <> B.fromWord8s (padding (Convert.int64_to_int (Byte.align (L.length s))))

-- Encode an Osc datum.
build_datum :: Datum -> B.Builder
build_datum d =
  case d of
    Int32 i -> B.fromInt32be i
    Int64 i -> B.fromInt64be i
    Float n -> B.fromWord32be (Cast.f32_w32 n)
    Double n -> B.fromWord64be (Cast.f64_w64 n)
    TimeStamp t -> B.fromWord64be (ntpr_to_ntpi t)
    AsciiString s -> build_ascii s
    Midi (MidiData b0 b1 b2 b3) -> B.fromWord8s [b0, b1, b2, b3]
    Blob b -> build_bytes b

-- Encode an Osc 'Message'.
build_message :: Message -> B.Builder
build_message (Message c l) =
  mconcat
    [ build_string c
    , build_ascii (descriptor l)
    , mconcat (map build_datum l)
    ]

-- Encode an Osc 'Bundle'.
build_bundle_ntpi :: Ntp64 -> [Message] -> B.Builder
build_bundle_ntpi t l =
  mconcat
    [ B.fromLazyByteString Byte.bundleHeader
    , B.fromWord64be t
    , mconcat (map (build_bytes . B.toLazyByteString . build_message) l)
    ]

-- | Builder for an Osc 'Packet'.
build_packet :: PacketOf Message -> B.Builder
build_packet o =
  case o of
    Packet_Message m -> build_message m
    Packet_Bundle (Bundle t m) -> build_bundle_ntpi (ntpr_to_ntpi t) m

{-# INLINE encodePacket #-}
{-# INLINE encodeMessage #-}
{-# INLINE encodeBundle #-}
{-# INLINE encodePacket_strict #-}

-- | Encode an Osc 'Packet'.
encodePacket :: PacketOf Message -> L.ByteString
encodePacket = B.toLazyByteString . build_packet

{- | Encode an Osc 'Message', ie. 'encodePacket' of 'Packet_Message'.

>>> let m = [47,103,95,102,114,101,101,0,44,105,0,0,0,0,0,0]
>>> encodeMessage (Message "/g_free" [Int32 0]) == L.pack m
True
-}
encodeMessage :: Message -> L.ByteString
encodeMessage = encodePacket . Packet_Message

{- | Encode an Osc 'Bundle', ie. 'encodePacket' of 'Packet_Bundle'.

>>> let m = [47,103,95,102,114,101,101,0,44,105,0,0,0,0,0,0]
>>> let b = [35,98,117,110,100,108,101,0,0,0,0,0,0,0,0,1,0,0,0,16] ++ m
>>> encodeBundle (Bundle immediately [Message "/g_free" [Int32 0]]) == L.pack b
True
-}
encodeBundle :: BundleOf Message -> L.ByteString
encodeBundle = encodePacket . Packet_Bundle

-- | Encode an Osc 'Packet' to a strict 'S.ByteString'.
encodePacket_strict :: PacketOf Message -> S.ByteString
encodePacket_strict = B.toByteString . build_packet
