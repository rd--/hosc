-- | Osc over Tcp implementation.
module Sound.Osc.Transport.Fd.Tcp where

import qualified Control.Exception as Exception {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Network.Socket as N {- network -}
import qualified System.IO as IO {- base -}

import qualified Sound.Osc.Coding.Decode.Binary as Binary {- hosc -}
import qualified Sound.Osc.Coding.Encode.Builder as Builder {- hosc -}
import qualified Sound.Osc.Coding.Byte as Byte {- hosc -}
import qualified Sound.Osc.Coding.Convert as Convert {- hosc -}
import qualified Sound.Osc.Packet as Packet {- hosc -}
import qualified Sound.Osc.Transport.Fd as Fd {- hosc -}

-- | The Tcp transport handle data type.
newtype Tcp = Tcp {tcpHandle :: IO.Handle}

-- | Send data over Tcp.
tcp_send_data :: Tcp -> B.ByteString -> IO ()
tcp_send_data (Tcp fd) d = do
  let n = Convert.int64_to_word32 (B.length d)
  B.hPut fd (B.append (Byte.encode_word32 n) d)
  IO.hFlush fd

-- | Send packet over Tcp.
tcp_send_packet :: Tcp -> Packet.Packet -> IO ()
tcp_send_packet tcp p = tcp_send_data tcp (Builder.encodePacket p)

-- | Receive packet over Tcp.
tcp_recv_packet :: Tcp -> IO Packet.Packet
tcp_recv_packet (Tcp fd) = do
  b0 <- B.hGet fd 4
  b1 <- B.hGet fd (Convert.word32_to_int (Byte.decode_word32 b0))
  return (Binary.decodePacket b1)

-- | Close Tcp.
tcp_close :: Tcp -> IO ()
tcp_close = IO.hClose . tcpHandle

-- | 'Tcp' is an instance of 'Transport'.
instance Fd.Transport Tcp where
   sendPacket = tcp_send_packet
   recvPacket = tcp_recv_packet
   close = tcp_close

-- | Bracket UDP communication.
with_tcp :: IO Tcp -> (Tcp -> IO t) -> IO t
with_tcp u = Exception.bracket u tcp_close

-- | Create and initialise Tcp socket.
tcp_socket :: (N.Socket -> N.SockAddr -> IO ()) -> Maybe String -> Int -> IO N.Socket
tcp_socket f host port = do
  fd <- N.socket N.AF_INET N.Stream 0
  let hints = N.defaultHints {N.addrFamily = N.AF_INET} -- localhost=ipv4
  i:_ <- N.getAddrInfo (Just hints) host (Just (show port))
  let sa = N.addrAddress i
  _ <- f fd sa
  return fd

-- | Convert 'N.Socket' to 'Tcp'.
socket_to_tcp :: N.Socket -> IO Tcp
socket_to_tcp fd = fmap Tcp (N.socketToHandle fd IO.ReadWriteMode)

-- | Create and initialise Tcp.
tcp_handle :: (N.Socket -> N.SockAddr -> IO ()) -> String -> Int -> IO Tcp
tcp_handle f host port = tcp_socket f (Just host) port >>= socket_to_tcp

{- | Make a 'Tcp' connection.

> import Sound.Osc.Datum {- hosc -}
> import Sound.Osc.Time {- hosc -}
> let t = openTcp "127.0.0.1" 57110
> let m1 = Packet.message "/dumpOsc" [Int32 1]
> let m2 = Packet.message "/g_new" [Int32 1]
> Fd.withTransport t (\fd -> let f = Fd.sendMessage fd in f m1 >> pauseThread 0.25 >> f m2)

-}
openTcp :: String -> Int -> IO Tcp
openTcp = tcp_handle N.connect

-- | 'N.accept' connection at /s/ and run /f/.
tcp_server_f :: N.Socket -> (Tcp -> IO ()) -> IO ()
tcp_server_f s f = do
  (fd, _) <- N.accept s
  h <- socket_to_tcp fd
  f h

-- | A trivial 'Tcp' /Osc/ server.
tcp_server :: Int -> (Tcp -> IO ()) -> IO ()
tcp_server port f = do
  s <- tcp_socket N.bind Nothing port
  N.listen s 1
  let repeatM_ = sequence_ . repeat
  repeatM_ (tcp_server_f s f)
