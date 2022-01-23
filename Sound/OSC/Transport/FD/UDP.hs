-- | OSC over UDP implementation.
module Sound.OSC.Transport.FD.UDP where

import Control.Exception {- base -}
import Data.Bifunctor {- base -}

import qualified Data.ByteString {- bytestring -}
import qualified Network.Socket as N {- network -}
import qualified Network.Socket.ByteString as C {- network -}

import qualified Sound.OSC.Coding.Decode.Binary as Binary {- hosc -}
import qualified Sound.OSC.Coding.Encode.Builder as Builder {- hosc -}
import qualified Sound.OSC.Packet as Packet {- hosc -}
import qualified Sound.OSC.Transport.FD as FD {- hosc -}

-- | The UDP transport handle data type.
newtype UDP = UDP {udpSocket :: N.Socket}

-- | Return the port number associated with the UDP socket.
udpPort :: Integral n => UDP -> IO n
udpPort = fmap fromIntegral . N.socketPort . udpSocket

-- | Send data over UDP using 'C.sendAll'.
udp_send_data :: UDP -> Data.ByteString.ByteString -> IO ()
udp_send_data (UDP fd) = C.sendAll fd

-- | Send packet over UDP using 'C.sendAll'.
udp_send_packet :: UDP -> Packet.Packet -> IO ()
udp_send_packet (UDP fd) p = C.sendAll fd (Builder.encodePacket_strict p)

-- | Receive packet over UDP.
udp_recv_packet :: UDP -> IO Packet.Packet
udp_recv_packet (UDP fd) = fmap Binary.decodePacket_strict (C.recv fd 8192)

-- | Close UDP.
udp_close :: UDP -> IO ()
udp_close (UDP fd) = N.close fd

-- | 'UDP' is an instance of 'FD.Transport'.
instance FD.Transport UDP where
   sendPacket = udp_send_packet
   recvPacket = udp_recv_packet
   close = udp_close

-- | Bracket UDP communication.
with_udp :: IO UDP -> (UDP -> IO t) -> IO t
with_udp u = bracket u udp_close

-- | Create and initialise UDP socket.
udp_socket :: (N.Socket -> N.SockAddr -> IO ()) -> String -> Int -> IO UDP
udp_socket f host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  let hints = N.defaultHints {N.addrFamily = N.AF_INET} -- localhost=ipv4
  i:_ <- N.getAddrInfo (Just hints) (Just host) (Just (show port))
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
openUDP :: String -> Int -> IO UDP
openUDP = udp_socket N.connect

{- | Trivial 'UDP' server socket.

> import Control.Concurrent {- base -}

> let u0 = udpServer "127.0.0.1" 57300
> t0 <- forkIO (FD.withTransport u0 (\fd -> forever (FD.recvMessage fd >>= print)))

> let u1 = openUDP "127.0.0.1" 57300
> FD.withTransport u1 (\fd -> FD.sendMessage fd (Packet.message "/n" []))
-}
udpServer :: String -> Int -> IO UDP
udpServer = udp_socket N.bind

-- | Variant of 'udpServer' that doesn't require the host address.
udp_server :: Int -> IO UDP
udp_server p = do
  let hints =
        N.defaultHints
        {N.addrFamily = N.AF_INET -- localhost=ipv4
        ,N.addrFlags = [N.AI_PASSIVE,N.AI_NUMERICSERV]
        ,N.addrSocketType = N.Datagram}
  a:_ <- N.getAddrInfo (Just hints) Nothing (Just (show p))
  s <- N.socket (N.addrFamily a) (N.addrSocketType a) (N.addrProtocol a)
  N.setSocketOption s N.ReuseAddr 1
  N.bind s (N.addrAddress a)
  return (UDP s)

-- | Send to specified address using 'C.sendAllTo.
sendTo :: UDP -> Packet.Packet -> N.SockAddr -> IO ()
sendTo (UDP fd) p = C.sendAllTo fd (Builder.encodePacket_strict p)

-- | Recv variant to collect message source address.
recvFrom :: UDP -> IO (Packet.Packet, N.SockAddr)
recvFrom (UDP fd) = fmap (first Binary.decodePacket_strict) (C.recvFrom fd 8192)
