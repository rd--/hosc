-- | Typeclass for encoding OSC packets.
module Sound.OpenSoundControl.Class where

import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Coding

class OSC o where
    encodeOSC :: Coding c => o -> c
    decodeOSC :: Coding c => c -> Maybe o

instance OSC Message where
    encodeOSC = encodePacket . Left
    decodeOSC = packet_to_message . decodePacket

instance OSC Bundle where
    encodeOSC = encodePacket . Right
    decodeOSC = Just . packet_to_bundle . decodePacket

{-
LANGUAGE TypeSynonymInstances
instance OSC Packet where
    encodeOSC = encodePacket
    decodeOSC = Just . decodePacket
-}
