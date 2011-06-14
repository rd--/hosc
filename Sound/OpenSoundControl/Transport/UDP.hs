-- | OSC over UDP implementation.
module Sound.OpenSoundControl.Transport.UDP (UDP(..)
                                            ,openUDP'
                                            ,udpServer'
                                            ,udpPort
                                            ,sendTo,recvFrom) where

import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as C (sendTo,recvFrom)
import qualified Network.Socket.ByteString.Lazy as C (send,recv)
import Sound.OpenSoundControl.OSC.Type
import Sound.OpenSoundControl.Transport

-- | The UDP transport handle data type.
data UDP = UDP {udpEncode :: OSC -> B.ByteString
               ,udpDecode :: B.ByteString -> OSC
               ,udpSocket :: N.Socket}

instance Transport UDP where
   send  (UDP enc _ fd) msg = C.send fd (enc msg) >> return ()
   recv  (UDP _ dec fd) = liftM dec (C.recv fd 8192)
   close (UDP _ _ fd) = N.sClose fd

type Coder = (OSC -> B.ByteString,B.ByteString -> OSC)

-- | Make a UDP connection.
openUDP' :: Coder -> String -> Int -> IO UDP
openUDP' (enc,dec) host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  a <- N.inet_addr host
  N.connect fd (N.SockAddrInet (fromIntegral port) a)
  -- N.setSocketOption fd N.RecvTimeOut 1000
  return (UDP enc dec fd)

-- | Trivial udp server.
udpServer' :: Coder -> String -> Int -> IO UDP
udpServer' (enc,dec) host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  a  <- N.inet_addr host
  let sa = N.SockAddrInet (fromIntegral port) a
  N.bindSocket fd sa
  return (UDP enc dec fd)

-- Network.Socket.ByteString.Lazy.sendTo does not exist
sendTo :: UDP -> OSC -> N.SockAddr -> IO ()
sendTo (UDP enc _ fd) o a = do
  let o' = S.pack (B.unpack (enc o))
  C.sendTo fd o' a >> return ()

-- Network.Socket.ByteString.Lazy.recvFrom does not exist
recvFrom :: UDP -> IO (OSC, N.SockAddr)
recvFrom (UDP _ dec fd) = do
  (s,a) <- C.recvFrom fd 8192
  let s' = B.pack (S.unpack s)
  return (dec s',a)

udpPort :: Integral n => UDP -> IO n
udpPort (UDP _ _ fd) = fmap fromIntegral (N.socketPort fd)
