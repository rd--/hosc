-- | hosc implements a subset of the Open Sound Control byte protocol.
--   The protocol is documented at <http://opensoundcontrol.org/>.
module Sound.OpenSoundControl (module Sound.OpenSoundControl.Type
                              ,module Sound.OpenSoundControl.Time
                              ,module Sound.OpenSoundControl.Transport
                              ,module Sound.OpenSoundControl.Transport.UDP
                              ,module Sound.OpenSoundControl.Transport.TCP
                              ,C.encodeOSC,C.decodeOSC
                              ,openUDP,udpServer
                              ,openTCP,tcpServer) where

import qualified Sound.OpenSoundControl.Coding.Decode.Binary as C
import qualified Sound.OpenSoundControl.Coding.Encode.Builder as C
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Transport.UDP
import Sound.OpenSoundControl.Transport.TCP

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
