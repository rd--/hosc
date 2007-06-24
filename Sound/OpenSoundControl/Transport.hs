module Sound.OpenSoundControl.Transport
   (Transport(send, recv, close),
    withTransport, wait,)
       where

import Sound.OpenSoundControl.OSC (OSC(..))

import Control.Exception (bracket)



-- | The class for the network protocolls.
class Transport t where
   -- | Encode and send an OSC packet over a UDP\/TCP connection.
   send :: t -> OSC -> IO ()

   -- | Receive and decode an OSC packet over a UDP\/TCP connection.
   recv :: t -> IO OSC

   -- | Close a UDP\/TCP connection.
   close :: t -> IO ()

-- | Does the OSC message have the specified address.
hasAddress :: String -> OSC -> Bool
hasAddress addr (Message s _) = s == addr
hasAddress _    (Bundle _ _)  = False

-- | Repeat action until predicate holds on result.
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p act = recurse
    where recurse = act >>= (\r -> if p r then return r else recurse)

-- | Wait for an OSC message with the specified address, discard intervening messages.
wait :: Transport t => t -> String -> IO OSC
wait t s = untilM (hasAddress s) (recv t)

-- | Bracket UDP\/TCP activity.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close
