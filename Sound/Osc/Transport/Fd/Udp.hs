-- | Osc over Udp implementation.
module Sound.Osc.Transport.Fd.Udp where

import Control.Exception {- base -}
import Control.Monad {- base -}
import Data.Bifunctor {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Network.Socket as N {- network -}
import qualified Network.Socket.ByteString as C {- network -}

import qualified Sound.Osc.Coding.Decode.Binary as Binary {- hosc -}
import qualified Sound.Osc.Coding.Encode.Builder as Builder {- hosc -}
import qualified Sound.Osc.Packet as Packet {- hosc -}
import qualified Sound.Osc.Transport.Fd as Fd {- hosc -}

-- | The Udp transport handle data type.
newtype Udp = Udp {udpSocket :: N.Socket}

-- | Return the port number associated with the Udp socket.
udpPort :: Integral n => Udp -> IO n
udpPort = fmap fromIntegral . N.socketPort . udpSocket

-- | Send data over Udp using 'C.send'.
udp_send_data :: Udp -> B.ByteString -> IO ()
udp_send_data (Udp fd) d = do
  let l = B.length d
  n <- C.send fd d
  when (n /= l) (error (show ("udp_send_data", l, n)))

-- | Send data over Udp using 'C.sendAll'.
udp_sendAll_data :: Udp -> B.ByteString -> IO ()
udp_sendAll_data (Udp fd) = C.sendAll fd

-- | Send packet over Udp.
udp_send_packet :: Udp -> Packet.PacketOf Packet.Message -> IO ()
udp_send_packet udp = udp_sendAll_data udp . Builder.encodePacket_strict

-- | Receive packet over Udp.
udp_recv_packet :: Udp -> IO (Packet.PacketOf Packet.Message)
udp_recv_packet (Udp fd) = fmap Binary.decodePacket_strict (C.recv fd 8192)

udp_recv_packet_or :: Udp -> IO (Either String Packet.Packet)
udp_recv_packet_or (Udp fd) = Binary.decodePacketOr . B.fromStrict <$> C.recv fd 8192

-- | Close Udp.
udp_close :: Udp -> IO ()
udp_close (Udp fd) = N.close fd

-- | 'Udp' is an instance of 'Fd.Transport'.
instance Fd.Transport Udp where
  sendPacket = udp_send_packet
  recvPacket = udp_recv_packet
  recvPacketOr = udp_recv_packet_or
  close = udp_close

-- | Bracket Udp communication.
with_udp :: IO Udp -> (Udp -> IO t) -> IO t
with_udp u = bracket u udp_close

-- | Create and initialise Udp socket.
udp_socket :: (N.Socket -> N.SockAddr -> IO ()) -> String -> Int -> IO Udp
udp_socket f host port = do
  fd <- N.socket N.AF_INET N.Datagram 0
  let hints = N.defaultHints {N.addrFamily = N.AF_INET} -- localhost=ipv4
  i : _ <- N.getAddrInfo (Just hints) (Just host) (Just (show port))
  let sa = N.addrAddress i
  f fd sa
  return (Udp fd)

-- | Set option, ie. 'N.Broadcast' or 'N.RecvTimeOut'.
set_udp_opt :: N.SocketOption -> Int -> Udp -> IO ()
set_udp_opt k v (Udp s) = N.setSocketOption s k v

-- | Get option.
get_udp_opt :: N.SocketOption -> Udp -> IO Int
get_udp_opt k (Udp s) = N.getSocketOption s k

-- | Make a 'Udp' connection.
openUdp :: String -> Int -> IO Udp
openUdp = udp_socket N.connect

{- | Trivial 'Udp' server socket.

> import Control.Concurrent

> let u0 = udpServer "127.0.0.1" 57300
> t0 <- forkIO (Fd.withTransport u0 (\fd -> forever (Fd.recvMessage fd >>= print >> print "Received message, continuing")))
> killThread t0

> let u1 = openUdp "127.0.0.1" 57300
> Fd.withTransport u1 (\fd -> Fd.sendMessage fd (Packet.message "/n" []))
-}
udpServer :: String -> Int -> IO Udp
udpServer = udp_socket N.bind

-- | Variant of 'udpServer' that doesn't require the host address.
udp_server :: Int -> IO Udp
udp_server p = do
  let hints =
        N.defaultHints
          { N.addrFamily = N.AF_INET -- localhost=ipv4
          , N.addrFlags = [N.AI_PASSIVE, N.AI_NUMERICSERV]
          , N.addrSocketType = N.Datagram
          }
  a : _ <- N.getAddrInfo (Just hints) Nothing (Just (show p))
  s <- N.socket (N.addrFamily a) (N.addrSocketType a) (N.addrProtocol a)
  N.setSocketOption s N.ReuseAddr 1
  N.bind s (N.addrAddress a)
  return (Udp s)

-- | Send to specified address using 'C.sendAllTo.
sendTo :: Udp -> Packet.PacketOf Packet.Message -> N.SockAddr -> IO ()
sendTo (Udp fd) p = C.sendAllTo fd (Builder.encodePacket_strict p)

-- | Recv variant to collect message source address.
recvFrom :: Udp -> IO (Packet.PacketOf Packet.Message, N.SockAddr)
recvFrom (Udp fd) = fmap (first Binary.decodePacket_strict) (C.recvFrom fd 8192)
