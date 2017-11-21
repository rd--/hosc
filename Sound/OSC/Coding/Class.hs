-- | A type-class to provide coding operations to different data types
--   using the same function names.
module Sound.OSC.Coding.Class where

import qualified Data.ByteString as Strict {- bytestring -}
import qualified Data.ByteString.Lazy as Lazy {- bytestring -}

import Sound.OSC.Packet {- hosc -}
import qualified Sound.OSC.Coding.Decode.Binary as Binary {- hosc -}
import qualified Sound.OSC.Coding.Encode.Builder as Builder {- hosc -}

-- | Converting from and to binary packet representations.
class Coding a where
    encodePacket :: Packet -> a -- ^ Decode an OSC packet.
    decodePacket :: a -> Packet -- ^ Encode an OSC packet.

instance Coding Strict.ByteString where
    encodePacket = Builder.encodePacket_strict
    decodePacket = Binary.decodePacket_strict

instance Coding Lazy.ByteString where
    encodePacket = Builder.encodePacket
    decodePacket = Binary.decodePacket

{-
{-# LANGUAGE FlexibleInstances #-}
import qualified Data.ByteString.Lazy.Char8 as Char8 {- bytestring -}
instance Coding String where
    encodePacket = Char8.unpack . encodePacket
    decodePacket = decodePacket . Char8.pack
-}

-- | 'encodePacket' '.' 'Packet_Message'.
encodeMessage :: Coding c => Message -> c
encodeMessage = encodePacket . Packet_Message

-- | 'encodePacket' '.' 'Packet_Bundle'.
encodeBundle :: Coding c => Bundle -> c
encodeBundle = encodePacket . Packet_Bundle

-- | 'packet_to_message' '.' 'decodePacket'.
decodeMessage :: Coding c => c -> Maybe Message
decodeMessage = packet_to_message . decodePacket

-- | 'packet_to_bundle' '.' 'decodePacket'.
decodeBundle :: Coding c => c -> Bundle
decodeBundle = packet_to_bundle . decodePacket
