-- | Osc over Tcp implementation.
module Sound.Osc.Transport.Fd.Tcp where

import qualified Control.Exception as Exception {- base -}
import qualified Data.ByteString.Lazy as ByteString.Lazy {- bytestring -}
import qualified Network.Socket as Socket {- network -}
import qualified System.IO as Io {- base -}

import qualified Sound.Osc.Coding.Byte as Byte {- hosc -}
import qualified Sound.Osc.Coding.Convert as Convert {- hosc -}
import qualified Sound.Osc.Coding.Decode.Binary as Decode.Binary {- hosc -}
import qualified Sound.Osc.Coding.Encode.Builder as Encode.Builder {- hosc -}
import qualified Sound.Osc.Packet as Packet {- hosc -}
import qualified Sound.Osc.Transport.Fd as Fd {- hosc -}

-- | The Tcp transport handle data type.
newtype Tcp = Tcp {tcpHandle :: Io.Handle}

-- | Send data over Tcp.
tcp_send_data :: Tcp -> ByteString.Lazy.ByteString -> IO ()
tcp_send_data (Tcp fd) d = do
  let n = Convert.int64_to_word32 (ByteString.Lazy.length d)
  ByteString.Lazy.hPut fd (ByteString.Lazy.append (Byte.encode_word32 n) d)
  Io.hFlush fd

-- | Send packet over Tcp.
tcp_send_packet :: Tcp -> Packet.PacketOf Packet.Message -> IO ()
tcp_send_packet tcp p = tcp_send_data tcp (Encode.Builder.encodePacket p)

-- | Receive packet over Tcp.
tcp_recv_packet :: Tcp -> IO (Packet.PacketOf Packet.Message)
tcp_recv_packet (Tcp fd) = do
  b0 <- ByteString.Lazy.hGet fd 4
  b1 <- ByteString.Lazy.hGet fd (Convert.word32_to_int (Byte.decode_word32 b0))
  return (Decode.Binary.decodePacket b1)

tcp_recv_packet_or :: Tcp -> IO (Either String Packet.Packet)
tcp_recv_packet_or (Tcp fd) = do
  b0 <- ByteString.Lazy.hGet fd 4
  b1 <- ByteString.Lazy.hGet fd (Convert.word32_to_int (Byte.decode_word32 b0))
  return (Decode.Binary.decodePacketOr b1)

-- | Close Tcp.
tcp_close :: Tcp -> IO ()
tcp_close = Io.hClose . tcpHandle

-- | 'Tcp' is an instance of 'Transport'.
instance Fd.Transport Tcp where
  sendPacket = tcp_send_packet
  recvPacket = tcp_recv_packet
  recvPacketOr = tcp_recv_packet_or
  close = tcp_close

-- | Bracket Tcp communication.
with_tcp :: IO Tcp -> (Tcp -> IO t) -> IO t
with_tcp u = Exception.bracket u tcp_close

-- | Create and initialise Tcp socket.
tcp_socket :: (Socket.Socket -> Socket.SockAddr -> IO ()) -> Maybe String -> Int -> IO Socket.Socket
tcp_socket f host port = do
  fd <- Socket.socket Socket.AF_INET Socket.Stream 0
  let hints = Socket.defaultHints {Socket.addrFamily = Socket.AF_INET} -- localhost=ipv4
  i : _ <- Socket.getAddrInfo (Just hints) host (Just (show port))
  let sa = Socket.addrAddress i
  _ <- f fd sa
  return fd

-- | Convert 'Socket.Socket' to 'Tcp'.
socket_to_tcp :: Socket.Socket -> IO Tcp
socket_to_tcp fd = fmap Tcp (Socket.socketToHandle fd Io.ReadWriteMode)

-- | Create and initialise Tcp.
tcp_handle :: (Socket.Socket -> Socket.SockAddr -> IO ()) -> String -> Int -> IO Tcp
tcp_handle f host port = tcp_socket f (Just host) port >>= socket_to_tcp

{- | Make a 'Tcp' connection.

> import Sound.Osc.Datum
> import Sound.Osc.Time
> let t = openTcp "127.0.0.1" 57110
> let m1 = Packet.message "/dumpOsc" [Int32 1]
> let m2 = Packet.message "/g_new" [Int32 1]
> Fd.withTransport t (\fd -> let f = Fd.sendMessage fd in f m1 >> pauseThread 0.25 >> f m2)
-}
openTcp :: String -> Int -> IO Tcp
openTcp = tcp_handle Socket.connect

-- | 'Socket.accept' connection at /s/ and run /f/.
tcp_server_f :: Socket.Socket -> (Tcp -> IO ()) -> IO ()
tcp_server_f s f = do
  (fd, _) <- Socket.accept s
  h <- socket_to_tcp fd
  f h

-- | A trivial 'Tcp' /Osc/ server.
tcp_server :: Int -> (Tcp -> IO ()) -> IO ()
tcp_server port f = do
  s <- tcp_socket Socket.bind Nothing port
  Socket.listen s 1
  let repeatM_ = sequence_ . repeat
  repeatM_ (tcp_server_f s f)
