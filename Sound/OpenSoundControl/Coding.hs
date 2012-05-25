{-# LANGUAGE FlexibleInstances,TypeSynonymInstances #-}
-- | A type-class to provide coding operations to different data types
--   using the same function names.
module Sound.OpenSoundControl.Coding where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Sound.OpenSoundControl.Type
import qualified Sound.OpenSoundControl.Coding.Decode.Binary as Binary
import qualified Sound.OpenSoundControl.Coding.Encode.Builder as Builder

-- | Converting from and to binary packet representations.
class Coding a where
    -- | Decode an OSC packet.
    encodePacket :: Packet -> a
    -- | Encode an OSC packet.
    decodePacket :: a -> Packet

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

encodeMessage :: Coding c => Message -> c
encodeMessage = encodePacket . Left

encodeBundle :: Coding c => Bundle -> c
encodeBundle = encodePacket . Right

decodeMessage :: Coding c => c -> Maybe Message
decodeMessage = packet_to_message . decodePacket

decodeBundle :: Coding c => c -> Bundle
decodeBundle = packet_to_bundle . decodePacket
