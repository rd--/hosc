-- | Osc over Udp implementation.
module Sound.Osc.Transport.Fd.Udp where

import Control.Exception {- base -}
import Control.Monad {- base -}
import Data.Bifunctor {- base -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.ByteString.Lazy as ByteString.Lazy {- bytestring -}
import qualified Network.Socket as Socket {- network -}
import qualified Network.Socket.ByteString as Socket.ByteString {- network -}

import qualified Sound.Osc.Coding.Decode.Binary as Binary {- hosc -}
import qualified Sound.Osc.Coding.Encode.Builder as Builder {- hosc -}
import qualified Sound.Osc.Packet as Packet {- hosc -}
import qualified Sound.Osc.Transport.Fd as Fd {- hosc -}

-- | The Udp transport handle data type.
newtype Udp = Udp {udpSocket :: Socket.Socket}

-- | Return the port number associated with the Udp socket.
udpPort :: Integral n => Udp -> IO n
udpPort = fmap fromIntegral . Socket.socketPort . udpSocket

-- | Send data over Udp using 'Socket.ByteString.send'.
udp_send_data :: Udp -> ByteString.ByteString -> IO ()
udp_send_data (Udp fd) d = do
  let l = ByteString.length d
  n <- Socket.ByteString.send fd d
  when (n /= l) (error (show ("udp_send_data", l, n)))

-- | Send data over Udp using 'Socket.ByteString.sendAll'.
udp_sendAll_data :: Udp -> ByteString.ByteString -> IO ()
udp_sendAll_data (Udp fd) = Socket.ByteString.sendAll fd

-- | Send packet over Udp.
udp_send_packet :: Udp -> Packet.PacketOf Packet.Message -> IO ()
udp_send_packet udp = udp_sendAll_data udp . Builder.encodePacket_strict

-- | Receive packet over Udp.
udp_recv_packet :: Udp -> IO (Packet.PacketOf Packet.Message)
udp_recv_packet (Udp fd) = fmap Binary.decodePacket_strict (Socket.ByteString.recv fd 8192)

udp_recv_packet_or :: Udp -> IO (Either String Packet.Packet)
udp_recv_packet_or (Udp fd) =
  Binary.decodePacketOr .
  ByteString.Lazy.fromStrict <$> Socket.ByteString.recv fd 8192

-- | Close Udp.
udp_close :: Udp -> IO ()
udp_close (Udp fd) = Socket.close fd

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
udp_socket :: (Socket.Socket -> Socket.SockAddr -> IO ()) -> String -> Int -> IO Udp
udp_socket f host port = do
  fd <- Socket.socket Socket.AF_INET Socket.Datagram 0
  let hints = Socket.defaultHints {Socket.addrFamily = Socket.AF_INET} -- localhost=ipv4
  i : _ <- Socket.getAddrInfo (Just hints) (Just host) (Just (show port))
  let sa = Socket.addrAddress i
  f fd sa
  return (Udp fd)

-- | Set option, ie. 'Socket.Broadcast' or 'Socket.RecvTimeOut'.
set_udp_opt :: Socket.SocketOption -> Int -> Udp -> IO ()
set_udp_opt k v (Udp s) = Socket.setSocketOption s k v

-- | Get option.
get_udp_opt :: Socket.SocketOption -> Udp -> IO Int
get_udp_opt k (Udp s) = Socket.getSocketOption s k

-- | Make a 'Udp' connection.
openUdp :: String -> Int -> IO Udp
openUdp = udp_socket Socket.connect

{- | Trivial 'Udp' server socket.

> import Control.Concurrent

> let u0 = udpServer "127.0.0.1" 57300
> t0 <- forkIO (Fd.withTransport u0 (\fd -> forever (Fd.recvMessage fd >>= print >> print "Received message, continuing")))
> killThread t0

> let u1 = openUdp "127.0.0.1" 57300
> Fd.withTransport u1 (\fd -> Fd.sendMessage fd (Packet.message "/n" []))
-}
udpServer :: String -> Int -> IO Udp
udpServer = udp_socket Socket.bind

-- | Variant of 'udpServer' that doesn't require the host address.
udp_server :: Int -> IO Udp
udp_server p = do
  let hints =
        Socket.defaultHints
          { Socket.addrFamily = Socket.AF_INET -- localhost=ipv4
          , Socket.addrFlags = [Socket.AI_PASSIVE, Socket.AI_NUMERICSERV]
          , Socket.addrSocketType = Socket.Datagram
          }
  a : _ <- Socket.getAddrInfo (Just hints) Nothing (Just (show p))
  s <- Socket.socket (Socket.addrFamily a) (Socket.addrSocketType a) (Socket.addrProtocol a)
  Socket.setSocketOption s Socket.ReuseAddr 1
  Socket.bind s (Socket.addrAddress a)
  return (Udp s)

-- | Send to specified address using 'Socket.ByteString.sendAllTo.
sendTo :: Udp -> Packet.PacketOf Packet.Message -> Socket.SockAddr -> IO ()
sendTo (Udp fd) p = Socket.ByteString.sendAllTo fd (Builder.encodePacket_strict p)

-- | Recv variant to collect message source address.
recvFrom :: Udp -> IO (Packet.PacketOf Packet.Message, Socket.SockAddr)
recvFrom (Udp fd) = fmap (first Binary.decodePacket_strict) (Socket.ByteString.recvFrom fd 8192)
