-- | OSC over UDP implementation.
module Sound.OSC.Transport.FD.UDP where

import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as C (sendTo,recvFrom)
import qualified Network.Socket.ByteString.Lazy as C (send,recv)
import Sound.OpenSoundControl.Class
import Sound.OpenSoundControl.Coding
import Sound.OpenSoundControl.Type
import Sound.OSC.Transport.FD

-- | The UDP transport handle data type.
data UDP = UDP {udpSocket :: N.Socket}

-- | Return the port number associated with the UDP socket.
udpPort :: Integral n => UDP -> IO n
udpPort (UDP fd) = fmap fromIntegral (N.socketPort fd)

instance Transport UDP where
   sendOSC (UDP fd) msg = C.send fd (encodeOSC msg) >> return ()
   recvPacket (UDP fd) = liftM decodePacket (C.recv fd 8192)
   close (UDP fd) = N.sClose fd

-- | Create and initialise UDP socket.
udp_socket :: (N.Socket -> N.SockAddr -> IO t) -> String -> Int -> IO UDP
udp_socket f host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  a <- N.inet_addr host
  let sa = N.SockAddrInet (fromIntegral port) a
  _ <- f fd sa
  return (UDP fd)

-- | Make a 'UDP' connection.
--
-- > let t = openUDP "127.0.0.1" 57110
-- > in withTransport t (\fd -> recvT 0.5 fd >>= print)
openUDP :: String -> Int -> IO UDP
openUDP = udp_socket N.connect
-- N.setSocketOption fd N.RecvTimeOut 1000

-- | Trivial 'UDP' server socket.
--
-- > import Control.Concurrent
--
-- > let {f fd = forever (recvMessage fd >>= print)
-- >     ;t = udpServer "127.0.0.1" 57300}
-- > in void (forkIO (withTransport t f))
--
-- > let t = openUDP "127.0.0.1" 57300
-- > in withTransport t (\fd -> sendMessage fd (message "/n" []))
udpServer :: String -> Int -> IO UDP
udpServer = udp_socket N.bindSocket

-- | Send variant to send to specified address.
sendTo :: OSC o => UDP -> o -> N.SockAddr -> IO ()
sendTo (UDP fd) o a = do
  -- Network.Socket.ByteString.Lazy.sendTo does not exist
  let o' = S.pack (B.unpack (encodeOSC o))
  C.sendTo fd o' a >> return ()

-- | Recv variant to collect message source address.
recvFrom :: UDP -> IO (Packet, N.SockAddr)
recvFrom (UDP fd) = do
  -- Network.Socket.ByteString.Lazy.recvFrom does not exist
  (s,a) <- C.recvFrom fd 8192
  let s' = B.pack (S.unpack s)
  return (decodePacket s',a)
