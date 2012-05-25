-- | Typeclass for encoding and decoding OSC packets.
module Sound.OpenSoundControl.Class where

import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Coding

class OSC o where
    encodeOSC :: Coding c => o -> c
    decodeOSC :: Coding c => c -> Maybe o

instance OSC Message where
    encodeOSC = encodePacket . P_Message
    decodeOSC = packet_to_message . decodePacket

instance OSC Bundle where
    encodeOSC = encodePacket . P_Bundle
    decodeOSC = Just . packet_to_bundle . decodePacket

instance OSC Packet where
    encodeOSC = encodePacket
    decodeOSC = Just . decodePacket
