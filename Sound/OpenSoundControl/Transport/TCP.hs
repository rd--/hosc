-- | OSC over TCP implementation.
module Sound.OpenSoundControl.Transport.TCP (TCP, openTCP, tcpServer) where

import qualified Data.ByteString.Lazy as B
import Control.Monad
import Network
import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.OSC.Encoding
import System.IO

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
openTCP :: String -> Int -> IO TCP
openTCP host = liftM TCP . connectTo host . PortNumber . fromIntegral

-- | A trivial TCP OSC server.
tcpServer :: Int -> (TCP -> IO ()) -> IO ()
tcpServer p f = do s <- listenOn (PortNumber (fromIntegral p))
                   (sequence_ . repeat) (do (fd, _, _) <- accept s
                                            f (TCP fd)
                                            return ())
