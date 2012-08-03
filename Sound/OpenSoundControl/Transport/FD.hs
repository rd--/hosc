-- | An abstract transport layer with implementations for @UDP@ and
-- @TCP@ transport.
module Sound.OpenSoundControl.Transport.FD where

import Control.Exception
import Data.List
import Data.Maybe
import Sound.OpenSoundControl.Class
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Wait

-- | Abstract over the underlying transport protocol.
class Transport t where
   -- | Encode and send an OSC packet.
   sendOSC :: OSC o => t -> o -> IO ()
   -- | Receive and decode an OSC packet.
   recvPacket :: t -> IO Packet
   -- | Close an existing connection.
   close :: t -> IO ()

-- | Bracket OSC communication.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close

-- * Send

-- | Type restricted synonym for 'sendOSC'.
sendMessage :: Transport t => t -> Message -> IO ()
sendMessage = sendOSC

-- | Type restricted synonym for 'sendOSC'.
sendBundle :: Transport t => t -> Bundle -> IO ()
sendBundle = sendOSC

-- * Receive

-- | Variant of 'recvPacket' that runs 'fromPacket'.
recvOSC :: (Transport t,OSC o) => t -> IO (Maybe o)
recvOSC = fmap fromPacket . recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_bundle'.
recvBundle :: (Transport t) => t -> IO Bundle
recvBundle = fmap packet_to_bundle . recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_message'.
recvMessage :: (Transport t) => t -> IO (Maybe Message)
recvMessage = fmap packet_to_message . recvPacket

-- | Variant of 'recvPacket' that runs 'packetMessages'.
recvMessages :: (Transport t) => t -> IO [Message]
recvMessages = fmap packetMessages . recvPacket

-- * Timeout

-- | Variant of 'recvPacket' that implements an /n/ second 'timeout'.
recvPacketTimeout :: (Transport t) => Double -> t -> IO (Maybe Packet)
recvPacketTimeout n fd = timeout_r n (recvPacket fd)

-- * Wait

-- | Wait for a 'Packet' where the supplied predicate is 'True',
-- discarding intervening packets.
waitUntil :: (Transport t) => t -> (Packet -> Bool) -> IO Packet
waitUntil t f = untilPredicate f (recvPacket t)

-- | Wait for a 'Packet' where the supplied function does not give
-- 'Nothing', discarding intervening packets.
waitFor :: (Transport t) => t -> (Packet -> Maybe a) -> IO a
waitFor t f = untilMaybe f (recvPacket t)

-- | 'waitUntil' 'packet_is_immediate'.
waitImmediate :: Transport t => t -> IO Packet
waitImmediate t = waitUntil t packet_is_immediate

-- | 'waitFor' 'packet_to_message', ie. an incoming 'Message' or
-- immediate mode 'Bundle' with one element.
waitMessage :: Transport t => t -> IO Message
waitMessage t = waitFor t packet_to_message

-- | A 'waitFor' for variant using 'packet_has_address' to match on
-- the 'Address_Pattern' of incoming 'Packets'.
waitAddress :: Transport t => t -> Address_Pattern -> IO Packet
waitAddress t s =
    let f o = if packet_has_address s o then Just o else Nothing
    in waitFor t f

-- | Variant on 'waitAddress' that returns matching 'Message'.
waitReply :: Transport t => t -> Address_Pattern -> IO Message
waitReply t s =
    let f = fromMaybe (error "waitReply: message not located?") .
            find (message_has_address s) .
            packetMessages
    in fmap f (waitAddress t s)

-- | Variant of 'waitReply' that runs 'messageDatum'.
waitDatum :: Transport t => t -> Address_Pattern -> IO [Datum]
waitDatum t = fmap messageDatum . waitReply t
