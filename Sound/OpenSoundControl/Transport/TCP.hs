-- | OSC over TCP implementation.
module Sound.OpenSoundControl.Transport.TCP (TCP(..)
                                            ,openTCP'
                                            ,tcpServer') where

import qualified Data.ByteString.Lazy as B
import Control.Monad
import Network
import Sound.OpenSoundControl.Coding
import Sound.OpenSoundControl.Coding.Byte
import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Type
import System.IO

-- | The TCP transport handle data type.
data TCP = TCP {tcpEncode :: OSC -> B.ByteString
               ,tcpDecode :: B.ByteString -> OSC
               ,tcpHandle :: Handle}

instance Transport TCP where
   send (TCP enc _ fd) msg =
      do let b = enc msg
             n = fromIntegral (B.length b)
         B.hPut fd (B.append (encode_u32 n) b)
         hFlush fd
   recv (TCP _ dec fd) =
      do b0 <- B.hGet fd 4
         b1 <- B.hGet fd (fromIntegral (decode_u32 b0))
         return (dec b1)
   close (TCP _ _ fd) = hClose fd

-- | Make a TCP connection using specified coder.
openTCP' :: Coder -> String -> Int -> IO TCP
openTCP' (enc,dec) host =
    liftM (TCP enc dec) .
    connectTo host .
    PortNumber .
    fromIntegral

-- | A trivial TCP OSC server using specified coder.
tcpServer' :: Coder -> Int -> (TCP -> IO ()) -> IO ()
tcpServer' (enc,dec) p f = do
  s <- listenOn (PortNumber (fromIntegral p))
  (sequence_ . repeat) (do (fd, _, _) <- accept s
                           f (TCP enc dec fd)
                           return ())
