module Sound.OpenSoundControl.UDP
   (UDP, Port, udp, send, recv, wait, close, withUDP) where

import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.OSC

import Control.Exception (bracket)
import Control.Monad (liftM)
import qualified Network.Socket as N

type UDP = N.Socket
type Port = N.PortNumber

-- | Make a UDP connection.
udp :: String -> Port -> IO UDP
udp host port = do fd <- N.socket N.AF_INET N.Datagram 0
                   a  <- N.inet_addr host
                   N.connect fd (N.SockAddrInet port a)
                   -- N.setSocketOption fd N.RecvTimeOut 1000
                   return fd

-- | Encode and send an OSC packet over a UDP connection. 
send :: UDP -> OSC -> IO ()
send fd msg = N.send fd (decode_str (encodeOSC msg)) >> return ()

-- | Receive and decode an OSC packet over a UDP connection. 
recv :: UDP -> IO OSC
recv fd = liftM (decodeOSC . encode_str) (N.recv fd 8192)

-- | Does the OSC message have the specified address.
hasAddress :: String -> OSC -> Bool
hasAddress addr (Message s _) = s == addr
hasAddress _    (Bundle _ _)  = False

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p act =
   let recurse =
         do r <- act
            if p r then return r else recurse
   in  recurse

-- | Wait for an OSC message with the specified address, discard intervening messages.
wait :: UDP -> String -> IO OSC
wait fd s = untilM (hasAddress s) (recv fd)

-- | Close a UDP connection.
close :: UDP -> IO ()
close = N.sClose

-- | Bracket UDP activity.
withUDP :: IO UDP -> (UDP -> IO a) -> IO a
withUDP u = bracket u close
