module Sound.OpenSoundControl.Transport.TCP (TCP, tcp, tcpServer) where

import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Byte (encode_u32, decode_u32)
import Sound.OpenSoundControl.OSC (encodeOSC, decodeOSC)

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as B
import Network (PortID(PortNumber), connectTo, listenOn, accept)
import System.IO (Handle, hFlush, hClose)

-- | The TCP transport handle data type.
data TCP = TCP Handle deriving (Eq, Show)

instance Transport TCP where
   send (TCP fd) msg =
      do let b = encodeOSC msg
             n = fromIntegral (B.length b)
         B.hPut fd (B.append (encode_u32 n) b)
         hFlush fd

   recv (TCP fd) =
      do b0 <- B.hGet fd 4
         b1 <- B.hGet fd (fromIntegral (decode_u32 b0))
         return (decodeOSC b1)

   close (TCP fd) = hClose fd

-- | Make a TCP connection.
tcp :: String -> Int -> IO TCP
tcp host port = liftM TCP (connectTo host (PortNumber (fromIntegral port)))

-- | A trivial TCP OSC server.
tcpServer :: Int -> (TCP -> IO ()) -> IO ()
tcpServer p f = do s <- listenOn (PortNumber (fromIntegral p))
                   (sequence_ . repeat) (do (fd, _, _) <- accept s
                                            f (TCP fd)
                                            return ())
