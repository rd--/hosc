-- | An implementation of a subset of the /Open Sound Control/ byte
-- protocol, documented at <http://opensoundcontrol.org/>.
--
-- For the most part this top-level module is the only import
-- required.  It provides the 'Datum' and 'OSC' types, 'encodeOSC' and
-- 'decodeOSC' functions, basic 'UDP' and 'TCP' 'Transport' layers,
-- and basic temporal operations 'utcr' to access the current time and
-- 'pauseThread' to delay the current thread.
--
-- > let o = Bundle immediately [Message "/g_free" [Int 0]]
-- > in decodeOSC (encodeOSC o) == o
module Sound.OpenSoundControl (module Sound.OpenSoundControl.Type
                              ,module Sound.OpenSoundControl.Time
                              ,module Sound.OpenSoundControl.Transport
                              ,module Sound.OpenSoundControl.Transport.UDP
                              ,module Sound.OpenSoundControl.Transport.TCP
                              ,C.encodeOSC,C.decodeOSC
                              ,openUDP,udpServer
                              ,openTCP,tcpServer) where

import Sound.OpenSoundControl.Coding.Decode.Binary as C
import Sound.OpenSoundControl.Coding.Encode.Builder as C
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Time
import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Transport.UDP
import Sound.OpenSoundControl.Transport.TCP

-- | Make a UDP connection.
--
-- > let t = openUDP "127.0.0.1" 57110
-- > in withTransport t (\fd -> recvT 0.5 fd >>= print)
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
