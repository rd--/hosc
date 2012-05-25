-- | An abstract transport layer. hosc provides implementations for
--   UDP and TCP transport.
module Sound.OpenSoundControl.Transport where

import Control.Exception
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

-- | Type specified synonym for 'sendOSC'.
sendMessage :: Transport t => t -> Message -> IO ()
sendMessage = sendOSC

-- | Type specified synonym for 'sendOSC'.
sendBundle :: Transport t => t -> Message -> IO ()
sendBundle = sendOSC

-- | Repeat action until /f/ does not give 'Nothing' when applied to
-- result.
untilM :: Monad m => (a -> Maybe b) -> m a -> m b
untilM f act =
    let g p = let q = f p in case q of { Nothing -> rec
                                       ; Just r -> return r }
        rec = act >>= g
    in rec

-- | Real valued variant of 'timeout'.
timeout_r :: Double -> IO a -> IO (Maybe a)
timeout_r t = timeout (floor (t * 1000000))

-- | Variant of 'recvPacket' that implements an /n/ second 'timeout'.
recvPacketTimeout :: (Transport t) => Double -> t -> IO (Maybe Packet)
recvPacketTimeout n fd = timeout_r n (recvPacket fd)

-- | Variant of 'recvPacket' that runs 'packet_to_message_discard'.
recvMessage :: (Transport t) => t -> IO Message
recvMessage = fmap packet_to_message_discard . recvPacket

-- | Wait for a 'Packet' where the supplied function does not give
-- 'Nothing', discarding intervening packets.
waitFor :: (Transport t) => t -> (Packet -> Maybe a) -> IO a
waitFor t f = untilM f (recvPacket t)

-- | A 'waitFor' for variant using 'packet_has_address' to match on
-- the 'Address_Pattern' of incoming 'Packets'.
wait :: Transport t => t -> Address_Pattern -> IO Packet
wait t s =
    let f o = if packet_has_address s o then Just o else Nothing
    in waitFor t f

-- | Variant on 'wait' that returns matching 'Packet' as a 'Message'.
waitMessage :: Transport t => t -> Address_Pattern -> IO Message
waitMessage t = fmap packet_to_message_discard . wait t

-- | Bracket OSC communication.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close
