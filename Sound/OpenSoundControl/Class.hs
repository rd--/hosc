-- | Typeclass for encoding and decoding OSC packets.
module Sound.OpenSoundControl.Class where

import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Coding

class OSC o where
    toPacket :: o -> Packet
    fromPacket :: Packet -> Maybe o

instance OSC Message where
    toPacket = P_Message
    fromPacket = packet_to_message

instance OSC Bundle where
    toPacket = P_Bundle
    fromPacket = Just . packet_to_bundle

instance OSC Packet where
    toPacket = id
    fromPacket = Just

encodeOSC :: (Coding c,OSC o) => o -> c
encodeOSC = encodePacket . toPacket

decodeOSC :: (Coding c,OSC o) => c -> Maybe o
decodeOSC = fromPacket . decodePacket

