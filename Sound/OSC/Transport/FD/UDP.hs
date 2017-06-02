-- | OSC over UDP implementation.
module Sound.OSC.Transport.FD.UDP where

import Control.Monad {- base -}
import qualified Network.Socket as N {- network -}
import qualified Network.Socket.ByteString as C {- network -}

import Sound.OSC.Class
import Sound.OSC.Coding
import Sound.OSC.Type
import Sound.OSC.Transport.FD

-- | The UDP transport handle data type.
data UDP = UDP {udpSocket :: N.Socket}

-- | Return the port number associated with the UDP socket.
udpPort :: Integral n => UDP -> IO n
udpPort (UDP fd) = fmap fromIntegral (N.socketPort fd)

instance Transport UDP where
   -- C.L.send is not implemented for W32
   sendOSC (UDP fd) msg = void (C.send fd (encodeOSC msg))
   recvPacket (UDP fd) = liftM decodePacket (C.recv fd 8192)
   close (UDP fd) = N.close fd

-- | Create and initialise UDP socket.
udp_socket :: (N.Socket -> N.SockAddr -> IO ()) -> String -> Int -> IO UDP
udp_socket f host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  a <- N.inet_addr host
  let sa = N.SockAddrInet (fromIntegral port) a
  f fd sa
  return (UDP fd)

-- | Set option, ie. 'N.Broadcast' or 'N.RecvTimeOut'.
set_udp_opt :: N.SocketOption -> Int -> UDP -> IO ()
set_udp_opt k v (UDP s) = N.setSocketOption s k v

-- | Get option.
get_udp_opt :: N.SocketOption -> UDP -> IO Int
get_udp_opt k (UDP s) = N.getSocketOption s k

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
udpServer = udp_socket N.bind

-- | Send variant to send to specified address.
sendTo :: OSC o => UDP -> o -> N.SockAddr -> IO ()
sendTo (UDP fd) o a = do
  -- C.L.sendTo does not exist
  void (C.sendTo fd (encodeOSC o) a)

-- | Recv variant to collect message source address.
recvFrom :: UDP -> IO (Packet, N.SockAddr)
recvFrom (UDP fd) = do
  -- C.L.recvFrom does not exist
  (s,a) <- C.recvFrom fd 8192
  return (decodePacket s,a)
