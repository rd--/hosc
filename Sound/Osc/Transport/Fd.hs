-- | An abstract transport layer with implementations for @Udp@ and @Tcp@ transport.
module Sound.Osc.Transport.Fd where

import Control.Exception {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}
import qualified Sound.Osc.Wait as Wait {- hosc -}

-- | Abstract over the underlying transport protocol.
class Transport t where
  -- | Encode and send an Osc packet.
  sendPacket :: t -> PacketOf Message -> IO ()

  -- | Receive and decode an Osc packet.
  recvPacket :: t -> IO (PacketOf Message)

  -- | Close an existing connection.
  close :: t -> IO ()

-- | Bracket Osc communication.
withTransport :: Transport t => IO t -> (t -> IO a) -> IO a
withTransport u = bracket u close

-- * Send

-- | 'sendPacket' of 'Packet_Message'.
sendMessage :: Transport t => t -> Message -> IO ()
sendMessage t = sendPacket t . Packet_Message

-- | 'sendPacket' of 'Packet_Bundle'.
sendBundle :: Transport t => t -> BundleOf Message -> IO ()
sendBundle t = sendPacket t . Packet_Bundle

-- * Receive

-- | Variant of 'recvPacket' that runs 'packet_to_bundle'.
recvBundle :: (Transport t) => t -> IO (BundleOf Message)
recvBundle = fmap packet_to_bundle . recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_message'.
recvMessage :: (Transport t) => t -> IO (Maybe Message)
recvMessage = fmap packet_to_message . recvPacket

-- | Variant of 'recvPacket' that runs 'packetMessages'.
recvMessages :: (Transport t) => t -> IO [Message]
recvMessages = fmap packetMessages . recvPacket

-- * Wait

{- | Wait for a 'Packet' where the supplied predicate is 'True',
discarding intervening packets.
-}
waitUntil :: (Transport t) => t -> (PacketOf Message -> Bool) -> IO (PacketOf Message)
waitUntil t f = Wait.untilPredicate f (recvPacket t)

{- | Wait for a 'Packet' where the supplied function does not give
'Nothing', discarding intervening packets.
-}
waitFor :: (Transport t) => t -> (PacketOf Message -> Maybe a) -> IO a
waitFor t f = Wait.untilMaybe f (recvPacket t)

-- | 'waitUntil' 'packet_is_immediate'.
waitImmediate :: Transport t => t -> IO (PacketOf Message)
waitImmediate t = waitUntil t packet_is_immediate

{- | 'waitFor' 'packet_to_message', ie. an incoming 'Message' or
immediate mode 'Bundle' with one element.
-}
waitMessage :: Transport t => t -> IO Message
waitMessage t = waitFor t packet_to_message

{- | A 'waitFor' for variant using 'packet_has_address' to match on
the 'Address_Pattern' of incoming 'Packets'.
-}
waitAddress :: Transport t => t -> Address_Pattern -> IO (PacketOf Message)
waitAddress t s =
  let f o = if packet_has_address s o then Just o else Nothing
  in waitFor t f

-- | Variant on 'waitAddress' that returns matching 'Message'.
waitReply :: Transport t => t -> Address_Pattern -> IO Message
waitReply t s =
  let f =
        fromMaybe (error "waitReply: message not located?")
          . find (message_has_address s)
          . packetMessages
  in fmap f (waitAddress t s)

-- | Variant of 'waitReply' that runs 'messageDatum'.
waitDatum :: Transport t => t -> Address_Pattern -> IO [Datum]
waitDatum t = fmap messageDatum . waitReply t
