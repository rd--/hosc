-- | hosc implements a subset of the Open Sound Control byte protocol.
--   The protocol is documented at <http://opensoundcontrol.org/>.
module Sound.OpenSoundControl (module Sound.OpenSoundControl.OSC.Type
                              ,module Sound.OpenSoundControl.Time
                              ,module Sound.OpenSoundControl.Transport
                              ,module Sound.OpenSoundControl.Transport.UDP
                              ,module Sound.OpenSoundControl.Transport.TCP
                              ,encodeOSC,decodeOSC
                              ,openUDP,udpServer
                              ,openTCP,tcpServer) where

import Sound.OpenSoundControl.OSC.Binary
import Sound.OpenSoundControl.OSC.Builder
import Sound.OpenSoundControl.OSC.Type
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Transport.UDP
import Sound.OpenSoundControl.Transport.TCP

-- | Make a UDP connection.
openUDP :: String -> Int -> IO UDP
openUDP = openUDP' (encodeOSC,decodeOSC)

-- | Trivial udp server.
udpServer :: String -> Int -> IO UDP
udpServer = udpServer' (encodeOSC,decodeOSC)

-- | Make a TCP connection.
openTCP :: String -> Int -> IO TCP
openTCP = openTCP' (encodeOSC,decodeOSC)

-- | A trivial TCP OSC server.
tcpServer :: Int -> (TCP -> IO ()) -> IO ()
tcpServer = tcpServer' (encodeOSC,decodeOSC)
