module Sound.OpenSoundControl.Transport ( Transport(..)
                                        , withTransport
                                        , wait ) where

import Control.Exception
import Sound.OpenSoundControl.OSC

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

-- Repeat action until predicate holds on result.
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p act = recurse
    where recurse = act >>= (\r -> if p r then return r else recurse)

-- | Wait for an OSC message with the specified address, 
--   discarding intervening messages.
wait :: Transport t => t -> String -> IO OSC
wait t s = untilM (has_address s) (recv t)

-- | Bracket OSC communication.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close
