-- | OSC over TCP implementation.
module Sound.OSC.Transport.FD.TCP where

import qualified Control.Exception as Exception {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Network.Socket as N {- network -}
import qualified System.IO as IO {- base -}

import qualified Sound.OSC.Coding.Decode.Binary as Binary {- hosc -}
import qualified Sound.OSC.Coding.Encode.Builder as Builder {- hosc -}
import qualified Sound.OSC.Coding.Byte as Byte {- hosc -}
import qualified Sound.OSC.Coding.Convert as Convert {- hosc -}
import qualified Sound.OSC.Packet as Packet {- hosc -}
import qualified Sound.OSC.Transport.FD as FD {- hosc -}

-- | The TCP transport handle data type.
newtype TCP = TCP {tcpHandle :: IO.Handle}

-- | Send data over TCP.
tcp_send_data :: TCP -> B.ByteString -> IO ()
tcp_send_data (TCP fd) d = do
  let n = Convert.int64_to_word32 (B.length d)
  B.hPut fd (B.append (Byte.encode_word32 n) d)
  IO.hFlush fd

-- | Send packet over TCP.
tcp_send_packet :: TCP -> Packet.Packet -> IO ()
tcp_send_packet tcp p = tcp_send_data tcp (Builder.encodePacket p)

-- | Receive packet over TCP.
tcp_recv_packet :: TCP -> IO Packet.Packet
tcp_recv_packet (TCP fd) = do
  b0 <- B.hGet fd 4
  b1 <- B.hGet fd (Convert.word32_to_int (Byte.decode_word32 b0))
  return (Binary.decodePacket b1)

-- | Close TCP.
tcp_close :: TCP -> IO ()
tcp_close = IO.hClose . tcpHandle

-- | 'TCP' is an instance of 'Transport'.
instance FD.Transport TCP where
   sendPacket = tcp_send_packet
   recvPacket = tcp_recv_packet
   close = tcp_close

-- | Bracket UDP communication.
with_tcp :: IO TCP -> (TCP -> IO t) -> IO t
with_tcp u = Exception.bracket u tcp_close

-- | Create and initialise TCP socket.
tcp_socket :: (N.Socket -> N.SockAddr -> IO ()) -> Maybe String -> Int -> IO N.Socket
tcp_socket f host port = do
  fd <- N.socket N.AF_INET N.Stream 0
  let hints = N.defaultHints {N.addrFamily = N.AF_INET} -- localhost=ipv4
  i:_ <- N.getAddrInfo (Just hints) host (Just (show port))
  let sa = N.addrAddress i
  _ <- f fd sa
  return fd

-- | Convert 'N.Socket' to 'TCP'.
socket_to_tcp :: N.Socket -> IO TCP
socket_to_tcp fd = fmap TCP (N.socketToHandle fd IO.ReadWriteMode)

-- | Create and initialise TCP.
tcp_handle :: (N.Socket -> N.SockAddr -> IO ()) -> String -> Int -> IO TCP
tcp_handle f host port = tcp_socket f (Just host) port >>= socket_to_tcp

{- | Make a 'TCP' connection.

> import Sound.OSC.Datum {- hosc -}
> import Sound.OSC.Time {- hosc -}
> let t = openTCP "127.0.0.1" 57110
> let m1 = Packet.message "/dumpOSC" [Int32 1]
> let m2 = Packet.message "/g_new" [Int32 1]
> FD.withTransport t (\fd -> let f = FD.sendMessage fd in f m1 >> pauseThread 0.25 >> f m2)

-}
openTCP :: String -> Int -> IO TCP
openTCP = tcp_handle N.connect

-- | 'N.accept' connection at /s/ and run /f/.
tcp_server_f :: N.Socket -> (TCP -> IO ()) -> IO ()
tcp_server_f s f = do
  (fd, _) <- N.accept s
  h <- socket_to_tcp fd
  f h

-- | A trivial 'TCP' /OSC/ server.
tcp_server :: Int -> (TCP -> IO ()) -> IO ()
tcp_server port f = do
  s <- tcp_socket N.bind Nothing port
  N.listen s 1
  let repeatM_ = sequence_ . repeat
  repeatM_ (tcp_server_f s f)
