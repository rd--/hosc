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
   send :: OSC o => t -> o -> IO ()
   -- | Receive and decode an OSC packet.
   recv :: t -> IO Packet
   -- | Close an existing connection.
   close :: t -> IO ()

-- Repeat action until `f' does not give Nothing when applied to result.
untilM :: Monad m => (a -> Maybe b) -> m a -> m b
untilM f act =
    let g p = let q = f p in case q of { Nothing -> rec
                                       ; Just r -> return r }
        rec = act >>= g
    in rec

-- | Real valued variant of 'timeout'.
timeout_r :: Double -> IO a -> IO (Maybe a)
timeout_r t = timeout (floor (t * 1000000))

-- | Variant of 'recv' that implements an /n/ second 'timeout'.
recvT :: (Transport t) => Double -> t -> IO (Maybe Packet)
recvT n fd = timeout_r n (recv fd)

-- | Variant of 'recv' that runs 'packet_to_message'.
recvMessage :: (Transport t) => t -> IO Message
recvMessage = fmap packet_to_message . recv

-- | Wait for an OSC message where the supplied function does not give
--   Nothing, discarding intervening messages.
waitFor :: (Transport t) => t -> (Packet -> Maybe a) -> IO a
waitFor t f = untilM f (recv t)

-- | A 'waitFor' for variant matching on the 'Address_Pattern' of
-- incoming 'Packets'.
wait :: Transport t => t -> Address_Pattern -> IO Packet
wait t s =
    let f o = if packet_has_address s o then Just o else Nothing
    in waitFor t f

-- | Variant on 'wait' that returns matching 'Packet' as a 'Message'.
waitMessage :: Transport t => t -> Address_Pattern -> IO Message
waitMessage t = fmap packet_to_message . wait t

-- | Bracket OSC communication.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close
