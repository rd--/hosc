-- | OSC over TCP implementation.
module Sound.OSC.Transport.FD.TCP where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import Control.Monad {- base -}
import Network {- network -}
import System.IO {- base -}

import Sound.OSC.Class
import Sound.OSC.Coding
import Sound.OSC.Coding.Byte
import Sound.OSC.Transport.FD

-- | The TCP transport handle data type.
data TCP = TCP {tcpHandle :: Handle}

instance Transport TCP where
   sendOSC (TCP fd) msg =
      do let b = encodeOSC msg
             n = fromIntegral (B.length b)
         B.hPut fd (B.append (encode_u32 n) b)
         hFlush fd
   recvPacket (TCP fd) =
      do b0 <- B.hGet fd 4
         b1 <- B.hGet fd (fromIntegral (decode_u32 b0))
         return (decodePacket b1)
   close (TCP fd) = hClose fd

-- | Make a 'TCP' connection.
openTCP :: String -> Int -> IO TCP
openTCP host =
    liftM TCP .
    connectTo host .
    PortNumber .
    fromIntegral

-- | A trivial 'TCP' /OSC/ server.
tcpServer' :: Int -> (TCP -> IO ()) -> IO ()
tcpServer' p f = do
  s <- listenOn (PortNumber (fromIntegral p))
  (sequence_ . repeat) (do (fd, _, _) <- accept s
                           f (TCP fd)
                           return ())
