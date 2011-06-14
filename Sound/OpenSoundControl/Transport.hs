-- | An abstract transport layer with implementations for UDP and TCP
--   transport.
module Sound.OpenSoundControl.Transport ( Transport(..)
                                        , withTransport
                                        , waitFor, wait ) where

import Control.Exception
import Sound.OpenSoundControl.OSC.Type

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
has_address x (Message y _) = x == y
has_address _ _ = False

-- Repeat action until function does not give Nothing when applied to result.
untilM :: Monad m => (a -> Maybe b) -> m a -> m b
untilM f act = recurse
    where g p = let q = f p in case q of { Nothing -> recurse
                                         ; Just r -> return r }
          recurse = act >>= g

-- | Wait for an OSC message where the supplied function does not give
--   Nothing, discarding intervening messages.
waitFor :: Transport t => t -> (OSC -> Maybe a) -> IO a
waitFor t f = untilM f (recv t)

-- | A 'waitFor' for variant matching on address string of messages.
wait :: Transport t => t -> String -> IO OSC
wait t s = waitFor t (\o -> if has_address s o then Just o else Nothing)

-- | Bracket OSC communication.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close
