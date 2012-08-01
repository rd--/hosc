-- | An abstract transport layer. hosc provides implementations for
--   UDP and TCP transport.
module Sound.OpenSoundControl.Transport.FD where

import Control.Exception
import Sound.OpenSoundControl.Class
import Sound.OpenSoundControl.Type

-- | Abstract over the underlying transport protocol.
class Transport t where
   -- | Encode and send an OSC packet.
   sendOSC :: OSC o => t -> o -> IO ()
   -- | Receive and decode an OSC packet.
   recvPacket :: t -> IO Packet
   -- | Close an existing connection.
   close :: t -> IO ()

-- | Bracket OSC communication.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close

-- * Send

-- | Type restricted synonym for 'sendOSC'.
sendMessage :: Transport t => t -> Message -> IO ()
sendMessage = sendOSC

-- | Type restricted synonym for 'sendOSC'.
sendBundle :: Transport t => t -> Bundle -> IO ()
sendBundle = sendOSC

-- * Receive

-- | Variant of 'recvPacket' that runs 'fromPacket'.
recvOSC :: (Transport t,OSC o) => t -> IO (Maybe o)
recvOSC = fmap fromPacket . recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_bundle'.
recvBundle :: (Transport t) => t -> IO Bundle
recvBundle = fmap packet_to_bundle . recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_message'.
recvMessage :: (Transport t) => t -> IO (Maybe Message)
recvMessage = fmap packet_to_message . recvPacket

-- | Variant of 'recvPacket' that runs 'packetMessages'.
recvMessages :: (Transport t) => t -> IO [Message]
recvMessages = fmap packetMessages . recvPacket
