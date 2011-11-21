-- | OSC over UDP implementation.
module Sound.OpenSoundControl.Transport.UDP (UDP(..),udpPort
                                            ,openUDP'
                                            ,udpServer'
                                            ,sendTo,recvFrom) where

import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as C (sendTo,recvFrom)
import qualified Network.Socket.ByteString.Lazy as C (send,recv)
import Sound.OpenSoundControl.Coding
import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Type

-- | The UDP transport handle data type.
data UDP = UDP {udpEncode :: OSC -> B.ByteString
               ,udpDecode :: B.ByteString -> OSC
               ,udpSocket :: N.Socket}

-- | Return the port number associated with the UDP socket.
udpPort :: Integral n => UDP -> IO n
udpPort (UDP _ _ fd) = fmap fromIntegral (N.socketPort fd)

instance Transport UDP where
   send  (UDP enc _ fd) msg = C.send fd (enc msg) >> return ()
   recv  (UDP _ dec fd) = liftM dec (C.recv fd 8192)
   close (UDP _ _ fd) = N.sClose fd

-- | Make a UDP connection with specified coder.
openUDP' :: Coder -> String -> Int -> IO UDP
openUDP' (enc,dec) host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  a <- N.inet_addr host
  N.connect fd (N.SockAddrInet (fromIntegral port) a)
  -- N.setSocketOption fd N.RecvTimeOut 1000
  return (UDP enc dec fd)

-- | Trivial udp server with specified coder.
udpServer' :: Coder -> String -> Int -> IO UDP
udpServer' (enc,dec) host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  a  <- N.inet_addr host
  let sa = N.SockAddrInet (fromIntegral port) a
  N.bindSocket fd sa
  return (UDP enc dec fd)

-- | Send variant to send to specified address.
sendTo :: UDP -> OSC -> N.SockAddr -> IO ()
sendTo (UDP enc _ fd) o a = do
  -- Network.Socket.ByteString.Lazy.sendTo does not exist
  let o' = S.pack (B.unpack (enc o))
  C.sendTo fd o' a >> return ()

-- | Recv variant to collect message source address.
recvFrom :: UDP -> IO (OSC, N.SockAddr)
recvFrom (UDP _ dec fd) = do
  -- Network.Socket.ByteString.Lazy.recvFrom does not exist
  (s,a) <- C.recvFrom fd 8192
  let s' = B.pack (S.unpack s)
  return (dec s',a)
