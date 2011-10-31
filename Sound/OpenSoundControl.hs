-- | hosc implements a subset of the Open Sound Control byte protocol.
--   The protocol is documented at <http://opensoundcontrol.org/>.
module Sound.OpenSoundControl (module O
                              ,C.encodeOSC,C.decodeOSC
                              ,openUDP,udpServer
                              ,openTCP,tcpServer) where

import qualified Sound.OpenSoundControl.Coding.Decode.Binary as C
import qualified Sound.OpenSoundControl.Coding.Encode.Builder as C
import Sound.OpenSoundControl.Type as O
import Sound.OpenSoundControl.Time as O
import Sound.OpenSoundControl.Transport as O
import Sound.OpenSoundControl.Transport.UDP as O
import Sound.OpenSoundControl.Transport.TCP as O

-- | Make a UDP connection.
openUDP :: String -> Int -> IO UDP
openUDP = openUDP' (C.encodeOSC,C.decodeOSC)

-- | Trivial udp server.
udpServer :: String -> Int -> IO UDP
udpServer = udpServer' (C.encodeOSC,C.decodeOSC)

-- | Make a TCP connection.
openTCP :: String -> Int -> IO TCP
openTCP = openTCP' (C.encodeOSC,C.decodeOSC)

-- | A trivial TCP OSC server.
tcpServer :: Int -> (TCP -> IO ()) -> IO ()
tcpServer = tcpServer' (C.encodeOSC,C.decodeOSC)
