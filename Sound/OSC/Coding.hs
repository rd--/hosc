{-# LANGUAGE FlexibleInstances,TypeSynonymInstances #-}
-- | A type-class to provide coding operations to different data types
--   using the same function names.
module Sound.OSC.Coding where

import qualified Data.ByteString as S {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.ByteString.Lazy.Char8 as C {- bytestring -}

import Sound.OSC.Packet {- hosc -}
import qualified Sound.OSC.Coding.Decode.Binary as Binary {- hosc -}
import qualified Sound.OSC.Coding.Encode.Builder as Builder {- hosc -}

-- | Converting from and to binary packet representations.
class Coding a where
    encodePacket :: Packet -> a -- ^ Decode an OSC packet.
    decodePacket :: a -> Packet -- ^ Encode an OSC packet.

instance Coding S.ByteString where
    encodePacket = Builder.encodePacket_strict
    decodePacket = Binary.decodePacket_strict

instance Coding B.ByteString where
    encodePacket = Builder.encodePacket
    decodePacket = Binary.decodePacket

instance Coding String where
    encodePacket = C.unpack . encodePacket
    decodePacket = decodePacket . C.pack

-- | An 'encodePacket' and 'decodePacket' pair over 'B.ByteString'.
type Coder = (Packet -> B.ByteString,B.ByteString -> Packet)

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
