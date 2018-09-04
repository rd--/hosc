-- | OSC over TCP implementation.
module Sound.OSC.Transport.FD.TCP where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Network.Socket as N {- network -}
import System.IO {- base -}

import Sound.OSC.Coding.Class {- hosc -}
import Sound.OSC.Coding.Byte {- hosc -}
import Sound.OSC.Transport.FD {- hosc -}

-- | The TCP transport handle data type.
data TCP = TCP {tcpHandle :: Handle}

-- | 'TCP' is an instance of 'Transport'.
instance Transport TCP where
   sendPacket (TCP fd) p =
      do let b = encodePacket p
             n = fromIntegral (B.length b)
         B.hPut fd (B.append (encode_u32 n) b)
         hFlush fd
   recvPacket (TCP fd) =
      do b0 <- B.hGet fd 4
         b1 <- B.hGet fd (fromIntegral (decode_u32 b0))
         return (decodePacket b1)
   close (TCP fd) = hClose fd

-- | Create and initialise TCP socket.
tcp_socket :: (N.Socket -> N.SockAddr -> IO ()) -> Maybe String -> Int -> IO N.Socket
tcp_socket f host port = do
  fd <- N.socket N.AF_INET N.Stream 0
  i:_ <- N.getAddrInfo Nothing host (Just (show port))
  let sa = N.addrAddress i
  _ <- f fd sa
  return fd

socket_to_tcp :: N.Socket -> IO TCP
socket_to_tcp fd = fmap TCP (N.socketToHandle fd ReadWriteMode)

-- | Create and initialise TCP.
tcp_handle :: (N.Socket -> N.SockAddr -> IO ()) -> String -> Int -> IO TCP
tcp_handle f host port = tcp_socket f (Just host) port >>= socket_to_tcp

{- | Make a 'TCP' connection.

> import Sound.OSC.Core
> import Sound.OSC.Transport.FD
> import Sound.OSC.Transport.FD.TCP
> let t = openTCP "127.0.0.1" 57110
> let m1 = message "/dumpOSC" [Int32 1]
> let m2 = message "/g_new" [Int32 1]
> withTransport t (\fd -> let f = sendMessage fd in f m1 >> pauseThread 0.25 >> f m2)

-}
openTCP :: String -> Int -> IO TCP
openTCP = tcp_handle N.connect

-- | A trivial 'TCP' /OSC/ server.
tcpServer' :: Int -> (TCP -> IO ()) -> IO ()
tcpServer' port f = do
  s <- tcp_socket N.bind Nothing port
  N.listen s 1
  (sequence_ . repeat) (do (fd, _) <- N.accept s
                           h <- socket_to_tcp fd
                           f h
                           return ())
