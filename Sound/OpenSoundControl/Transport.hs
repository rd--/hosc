module Sound.OpenSoundControl.Transport (Transport(..), withTransport, 
                                         udp, tcp, tcpServer,
                                         send, recv, wait, close) where

import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.OSC

import Control.Exception (bracket)
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as B
import Data.Int
import Network
import qualified Network.Socket as N
import System.IO

-- | The transport handle data type.
data Transport = TCP Handle
               | UDP N.Socket
                 deriving (Eq, Show)

-- | Make a UDP connection.
udp :: String -> Int -> IO Transport
udp host port = do fd <- N.socket N.AF_INET N.Datagram 0
                   a  <- N.inet_addr host
                   N.connect fd (N.SockAddrInet (fromIntegral port) a)
                   -- N.setSocketOption fd N.RecvTimeOut 1000
                   return (UDP fd)

-- | Make a TCP connection.
tcp :: String -> Int -> IO Transport
tcp host port = do fd <- connectTo host (PortNumber (fromIntegral port))
                   return (TCP fd)

-- | A TCP OSC server.
tcpServer :: Int -> (Transport -> IO ()) -> IO ()
tcpServer p f = do s <- listenOn (PortNumber (fromIntegral p))
                   (sequence_ . repeat) (do (fd, _, _) <- accept s
                                            f (TCP fd)
                                            return ())

-- | Encode and send an OSC packet over a TCP connection.
send :: Transport -> OSC -> IO ()
send (UDP fd) msg = N.send fd (decode_str (encodeOSC msg)) >> return ()
send (TCP fd) msg = do let b = encodeOSC msg
                           n = fromIntegral (B.length b)
                       B.hPut fd (B.append (encode_u32 n) b)
                       hFlush fd

-- | Receive and decode an OSC packet over a UDP connection.
recv :: Transport -> IO OSC
recv (UDP fd) = liftM (decodeOSC . encode_str) (N.recv fd 8192)
recv (TCP fd) = do b0 <- B.hGet fd 4
                   b1 <- B.hGet fd (fromIntegral (decode_u32 b0))
                   return (decodeOSC b1)

-- | Does the OSC message have the specified address.
hasAddress :: String -> OSC -> Bool
hasAddress addr (Message s _) = s == addr
hasAddress _    (Bundle _ _)  = False

-- | Repeat action until predicate holds on result.
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p act =
   let recurse =
         do r <- act
            if p r then return r else recurse
   in  recurse

-- | Wait for an OSC message with the specified address, discard intervening messages.
wait :: Transport -> String -> IO OSC
wait t s = untilM (hasAddress s) (recv t)

-- | Close a UDP connection.
close :: Transport -> IO ()
close (UDP fd) = N.sClose fd
close (TCP fd) = hClose fd

-- | Bracket UDP activity.
withTransport :: IO Transport -> (Transport -> IO a) -> IO a
withTransport u = bracket u close
