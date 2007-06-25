module Sound.OpenSoundControl.Transport.UDP (UDP, udp) where

import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Byte (encode_str, decode_str)
import Sound.OpenSoundControl.OSC (encodeOSC, decodeOSC)

import Control.Monad (liftM)
import qualified Network.Socket as N

-- | The UDP transport handle data type.
data UDP = UDP N.Socket deriving (Eq, Show)

instance Transport UDP where
   send  (UDP fd) msg = N.send fd (decode_str (encodeOSC msg)) >> return ()
   recv  (UDP fd) = liftM (decodeOSC . encode_str) (N.recv fd 8192)
   close (UDP fd) = N.sClose fd

-- | Make a UDP connection.
udp :: String -> Int -> IO UDP
udp host port = do fd <- N.socket N.AF_INET N.Datagram 0
                   a  <- N.inet_addr host
                   N.connect fd (N.SockAddrInet (fromIntegral port) a)
                   -- N.setSocketOption fd N.RecvTimeOut 1000
                   return (UDP fd)
