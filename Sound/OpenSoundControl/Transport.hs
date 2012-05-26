-- | An abstract transport layer. hosc provides implementations for
--   UDP and TCP transport.
module Sound.OpenSoundControl.Transport where

import Control.Exception
import Data.List
import Data.Maybe
import Sound.OpenSoundControl.Class
import Sound.OpenSoundControl.Type
import System.Timeout

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

-- | Type specified synonym for 'sendOSC'.
sendMessage :: Transport t => t -> Message -> IO ()
sendMessage = sendOSC

-- | Type specified synonym for 'sendOSC'.
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

-- | Variant of 'recvMessages' that runs 'head'.
recvMsg :: (Transport t) => t -> IO Message
recvMsg = fmap head . recvMessages

-- * Timeout

-- | Real valued variant of 'timeout'.
timeout_r :: Double -> IO a -> IO (Maybe a)
timeout_r t = timeout (floor (t * 1000000))

-- | Variant of 'recvPacket' that implements an /n/ second 'timeout'.
recvPacketTimeout :: (Transport t) => Double -> t -> IO (Maybe Packet)
recvPacketTimeout n fd = timeout_r n (recvPacket fd)

-- * Wait

-- | Repeat action until predicate /f/ is 'True' when applied to
-- result.
untilP :: Monad m => (a -> Bool) -> m a -> m a
untilP f act =
    let g p = if f p then rec else return p
        rec = act >>= g
    in rec

-- | Wait for a 'Packet' where the supplied predicate is 'True',
-- discarding intervening packets.
waitUntil :: (Transport t) => t -> (Packet -> Bool) -> IO Packet
waitUntil t f = untilP f (recvPacket t)

-- | 'waitUntil' 'packet_is_immediate'.
waitImmediate :: Transport t => t -> IO Packet
waitImmediate t = waitUntil t packet_is_immediate

-- | Repeat action until /f/ does not give 'Nothing' when applied to
-- result.
untilM :: Monad m => (a -> Maybe b) -> m a -> m b
untilM f act =
    let g p = case f p of {Nothing -> rec;Just r -> return r}
        rec = act >>= g
    in rec

-- | Wait for a 'Packet' where the supplied function does not give
-- 'Nothing', discarding intervening packets.
waitFor :: (Transport t) => t -> (Packet -> Maybe a) -> IO a
waitFor t f = untilM f (recvPacket t)

-- | 'waitFor' 'packet_to_message', ie. an incoming 'Message' or
-- immediate mode 'Bundle' with one element.
waitMsg :: Transport t => t -> IO Message
waitMsg t = waitFor t packet_to_message

-- | A 'waitFor' for variant using 'packet_has_address' to match on
-- the 'Address_Pattern' of incoming 'Packets'.
waitAddress :: Transport t => t -> Address_Pattern -> IO Packet
waitAddress t s =
    let f o = if packet_has_address s o then Just o else Nothing
    in waitFor t f

-- | Variant on 'waitAddress' that returns matching 'Message'.
waitAddressMessage :: Transport t => t -> Address_Pattern -> IO Message
waitAddressMessage t s =
    let f = fromMaybe (error "waitAddressMessage: message not located?") .
            find (message_has_address s) .
            packetMessages
    in fmap f (waitAddress t s)

-- | Variant of 'waitAddressMessage' that runs 'messageDatum'.
waitAddressDatum :: Transport t => t -> Address_Pattern -> IO [Datum]
waitAddressDatum t = fmap messageDatum . waitAddressMessage t
