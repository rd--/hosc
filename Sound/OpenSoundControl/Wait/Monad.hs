module Sound.OpenSoundControl.Wait.Monad where

import Data.List
import Data.Maybe
import Sound.OpenSoundControl.Transport.Monad
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Wait

-- * Wait

-- | Wait for a 'Packet' where the supplied predicate is 'True',
-- discarding intervening packets.
waitUntil :: (Transport m) => (Packet -> Bool) -> m Packet
waitUntil f = untilPredicate f recvPacket

-- | Wait for a 'Packet' where the supplied function does not give
-- 'Nothing', discarding intervening packets.
waitFor :: (Transport m) => (Packet -> Maybe a) -> m a
waitFor f = untilMaybe f recvPacket

-- | 'waitUntil' 'packet_is_immediate'.
waitImmediate :: Transport m => m Packet
waitImmediate = waitUntil packet_is_immediate

-- | 'waitFor' 'packet_to_message', ie. an incoming 'Message' or
-- immediate mode 'Bundle' with one element.
waitMessage :: Transport m => m Message
waitMessage = waitFor packet_to_message

-- | A 'waitFor' for variant using 'packet_has_address' to match on
-- the 'Address_Pattern' of incoming 'Packets'.
waitAddress :: Transport m => Address_Pattern -> m Packet
waitAddress s =
    let f o = if packet_has_address s o then Just o else Nothing
    in waitFor f

-- | Variant on 'waitAddress' that returns matching 'Message'.
waitReply :: Transport m => Address_Pattern -> m Message
waitReply s =
    let f = fromMaybe (error "waitReply: message not located?") .
            find (message_has_address s) .
            packetMessages
    in fmap f (waitAddress s)

-- | Variant of 'waitReply' that runs 'messageDatum'.
waitDatum :: Transport m => Address_Pattern -> m [Datum]
waitDatum = fmap messageDatum . waitReply
