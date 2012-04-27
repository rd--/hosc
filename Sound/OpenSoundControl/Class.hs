-- | Typeclass for encoding OSC packets.
module Sound.OpenSoundControl.Class where

import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Coding

class OSC o where
    encodeOSC :: Coding c => o -> c

instance OSC Message where
    encodeOSC = encodePacket . Left

instance OSC Bundle where
    encodeOSC = encodePacket . Right

encodeMessage :: Coding c => Message -> c
encodeMessage = encodeOSC

encodeBundle :: Coding c => Message -> c
encodeBundle = encodeOSC

decodeMessage :: Coding c => c -> Message
decodeMessage = packet_to_message . decodePacket

decodeBundle :: Coding c => c -> Bundle
decodeBundle = packet_to_bundle . decodePacket
