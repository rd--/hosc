-- | OSC over UDP implementation.
module Sound.OSC.Transport.FD.UDP where

import Control.Monad {- base -}
import qualified Network.Socket as N {- network -}
import qualified Network.Socket.ByteString as C {- network -}

import Sound.OSC.Coding.Class {- hosc -}
import Sound.OSC.Packet {- hosc -}
import Sound.OSC.Transport.FD {- hosc -}

-- | The UDP transport handle data type.
data UDP = UDP {udpSocket :: N.Socket}

-- | Return the port number associated with the UDP socket.
udpPort :: Integral n => UDP -> IO n
udpPort (UDP fd) = fmap fromIntegral (N.socketPort fd)

-- | 'UDP' is an instance of 'Transport'.
instance Transport UDP where
   -- C.L.send is not implemented for W32
   sendPacket (UDP fd) p = void (C.send fd (encodePacket p))
   recvPacket (UDP fd) = liftM decodePacket (C.recv fd 8192)
   close (UDP fd) = N.close fd

-- | Create and initialise UDP socket.
udp_socket :: (N.Socket -> N.SockAddr -> IO ()) -> String -> Int -> IO UDP
udp_socket f host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  i:_ <- N.getAddrInfo Nothing (Just host) (Just (show port))
  let sa = N.addrAddress i
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

-- | Variant of 'udpServer' that doesn't require the host address.
udp_server :: Int -> IO UDP
udp_server p = do
  let hints =
        N.defaultHints
        {N.addrFlags = [N.AI_PASSIVE,N.AI_NUMERICSERV]
        ,N.addrSocketType = N.Datagram}
  a:_ <- N.getAddrInfo (Just hints) Nothing (Just (show p))
  s <- N.socket (N.addrFamily a) (N.addrSocketType a) (N.addrProtocol a)
  N.setSocketOption s N.ReuseAddr 1
  N.bind s (N.addrAddress a)
  return (UDP s)

-- | Send variant to send to specified address.
sendTo :: UDP -> Packet -> N.SockAddr -> IO ()
sendTo (UDP fd) p a = do
  -- C.L.sendTo does not exist
  void (C.sendTo fd (encodePacket p) a)

-- | Recv variant to collect message source address.
recvFrom :: UDP -> IO (Packet, N.SockAddr)
recvFrom (UDP fd) = do
  -- C.L.recvFrom does not exist
  (s,a) <- C.recvFrom fd 8192
  return (decodePacket s,a)
