module Sound.OpenSoundControl.Transport.UDP (UDP, openUDP) where

import Control.Monad
import qualified Network.Socket as N
import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.OSC
import Sound.OpenSoundControl.Transport

-- | The UDP transport handle data type.
data UDP = UDP N.Socket deriving (Eq, Show)

instance Transport UDP where
   send  (UDP fd) msg = N.send fd (decode_str (encodeOSC msg)) >> return ()
   recv  (UDP fd) = liftM (decodeOSC . encode_str) (N.recv fd 8192)
   close (UDP fd) = N.sClose fd

-- | Make a UDP connection.
openUDP :: String -> Int -> IO UDP
openUDP host port = do fd <- N.socket N.AF_INET N.Datagram 0
                       a  <- N.inet_addr host
                       N.connect fd (N.SockAddrInet (fromIntegral port) a)
                       -- N.setSocketOption fd N.RecvTimeOut 1000
                       return (UDP fd)
