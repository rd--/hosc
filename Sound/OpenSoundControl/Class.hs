-- | Typeclass for encoding and decoding OSC packets.
module Sound.OpenSoundControl.Class where

import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Coding

-- | A type-class for values that can be translated to and from OSC
-- 'Packet's.
class OSC o where
    toPacket :: o -> Packet -- ^ Translation to 'Packet'.
    fromPacket :: Packet -> Maybe o -- ^ Translation from 'Packet'.

instance OSC Message where
    toPacket = Packet_Message
    fromPacket = packet_to_message

instance OSC Bundle where
    toPacket = Packet_Bundle
    fromPacket = Just . packet_to_bundle

instance OSC Packet where
    toPacket = id
    fromPacket = Just

-- | 'encodePacket' '.' 'toPacket'.
encodeOSC :: (Coding c,OSC o) => o -> c
encodeOSC = encodePacket . toPacket

-- | 'fromPacket' '.' 'decodePacket'.
decodeOSC :: (Coding c,OSC o) => c -> Maybe o
decodeOSC = fromPacket . decodePacket
