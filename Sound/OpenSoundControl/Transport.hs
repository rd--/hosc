-- | An abstract transport layer. hosc provides implementations for
--   UDP and TCP transport.
module Sound.OpenSoundControl.Transport (Transport(..)
                                        ,withTransport
                                        ,recvT
                                        ,waitFor,wait) where

import Control.Exception
import Sound.OpenSoundControl.Type
import System.Timeout

-- | Abstract over the underlying transport protocol.
class Transport t where
   -- | Encode and send an OSC packet.
   send :: t -> OSC -> IO ()
   -- | Receive and decode an OSC packet.
   recv :: t -> IO OSC
   -- | Close an existing connection.
   close :: t -> IO ()

-- Does the OSC message have the specified address.
has_address :: String -> OSC -> Bool
has_address x o =
    case o of
      Message y _ -> x == y
      _ -> False

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

-- | Variant that wraps 'recv' in an /n/ second 'timeout'.
recvT :: Transport t => Double -> t -> IO (Maybe OSC)
recvT n fd = timeout_r n (recv fd)

-- | Wait for an OSC message where the supplied function does not give
--   Nothing, discarding intervening messages.
waitFor :: Transport t => t -> (OSC -> Maybe a) -> IO a
waitFor t f = untilM f (recv t)

-- | A 'waitFor' for variant matching on the address string of
--   incoming messages.
wait :: Transport t => t -> String -> IO OSC
wait t s = waitFor t (\o -> if has_address s o then Just o else Nothing)

-- | Bracket OSC communication.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close
