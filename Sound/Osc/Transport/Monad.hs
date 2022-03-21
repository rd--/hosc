-- | Monad class implementing an Open Sound Control transport.
module Sound.Osc.Transport.Monad where

import Control.Monad {- base -}
import Control.Monad.IO.Class {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Control.Monad.Trans.Reader as R {- transformers -}

import qualified Sound.Osc.Datum as Datum {- hosc -}
import qualified Sound.Osc.Transport.Fd as Fd {- hosc -}
import qualified Sound.Osc.Packet as Packet {- hosc -}
import qualified Sound.Osc.Wait as Wait {- hosc -}

-- | Sender monad.
class Monad m => SendOsc m where
   -- | Encode and send an Osc packet.
   sendPacket :: Packet.Packet -> m ()

-- | Receiver monad.
class Monad m => RecvOsc m where
   -- | Receive and decode an Osc packet.
   recvPacket :: m Packet.Packet

-- | 'DuplexOsc' is the union of 'SendOsc' and 'RecvOsc'.
class (SendOsc m,RecvOsc m) => DuplexOsc m where

-- | 'Transport' is 'DuplexOsc' with a 'MonadIO' constraint.
class (DuplexOsc m,MonadIO m) => Transport m where

-- | 'SendOsc' over 'ReaderT'.
instance (Fd.Transport t,MonadIO io) => SendOsc (R.ReaderT t io) where
   sendPacket p = R.ReaderT (liftIO . flip Fd.sendPacket p)

-- | 'RecvOsc' over 'ReaderT'.
instance (Fd.Transport t,MonadIO io) => RecvOsc (R.ReaderT t io) where
   recvPacket = R.ReaderT (liftIO . Fd.recvPacket)

-- | 'DuplexOsc' over 'ReaderT'.
instance (Fd.Transport t,MonadIO io) => DuplexOsc (R.ReaderT t io) where

-- | 'Transport' over 'ReaderT'.
instance (Fd.Transport t,MonadIO io) => Transport (R.ReaderT t io) where

-- | Transport connection.
type Connection t a = R.ReaderT t IO a

-- | Bracket Open Sound Control communication.
withTransport :: Fd.Transport t => IO t -> Connection t r -> IO r
withTransport u = Fd.withTransport u . R.runReaderT

-- | 'void' of 'withTransport'.
withTransport_ :: Fd.Transport t => IO t -> Connection t r -> IO ()
withTransport_ u = void . withTransport u

-- * Send

-- | Type restricted synonym for 'sendOsc'.
sendMessage :: SendOsc m => Packet.Message -> m ()
sendMessage = sendPacket . Packet.Packet_Message

-- | Type restricted synonym for 'sendOsc'.
sendBundle :: SendOsc m => Packet.Bundle -> m ()
sendBundle = sendPacket . Packet.Packet_Bundle

-- * Receive

-- | Variant of 'recvPacket' that runs 'packet_to_bundle'.
recvBundle :: (RecvOsc m) => m Packet.Bundle
recvBundle = fmap Packet.packet_to_bundle recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_message'.
recvMessage :: (RecvOsc m) => m (Maybe Packet.Message)
recvMessage = fmap Packet.packet_to_message recvPacket

-- | Erroring variant.
recvMessage_err :: RecvOsc m => m Packet.Message
recvMessage_err = fmap (fromMaybe (error "recvMessage")) recvMessage

-- | Variant of 'recvPacket' that runs 'packetMessages'.
recvMessages :: (RecvOsc m) => m [Packet.Message]
recvMessages = fmap Packet.packetMessages recvPacket

-- * Wait

-- | Wait for a 'Packet' where the supplied predicate is 'True',
-- discarding intervening packets.
waitUntil :: (RecvOsc m) => (Packet.Packet -> Bool) -> m Packet.Packet
waitUntil f = Wait.untilPredicate f recvPacket

-- | Wait for a 'Packet' where the supplied function does not give
-- 'Nothing', discarding intervening packets.
waitFor :: (RecvOsc m) => (Packet.Packet -> Maybe a) -> m a
waitFor f = Wait.untilMaybe f recvPacket

-- | 'waitUntil' 'packet_is_immediate'.
waitImmediate :: RecvOsc m => m Packet.Packet
waitImmediate = waitUntil Packet.packet_is_immediate

-- | 'waitFor' 'packet_to_message', ie. an incoming 'Message' or
-- immediate mode 'Bundle' with one element.
waitMessage :: RecvOsc m => m Packet.Message
waitMessage = waitFor Packet.packet_to_message

-- | A 'waitFor' for variant using 'packet_has_address' to match on
-- the 'Address_Pattern' of incoming 'Packets'.
waitAddress :: RecvOsc m => Packet.Address_Pattern -> m Packet.Packet
waitAddress s =
    let f o = if Packet.packet_has_address s o then Just o else Nothing
    in waitFor f

-- | Variant on 'waitAddress' that returns matching 'Message'.
waitReply :: RecvOsc m => Packet.Address_Pattern -> m Packet.Message
waitReply s =
    let f = fromMaybe (error "waitReply: message not located?") .
            find (Packet.message_has_address s) .
            Packet.packetMessages
    in fmap f (waitAddress s)

-- | Variant of 'waitReply' that runs 'messageDatum'.
waitDatum :: RecvOsc m => Packet.Address_Pattern -> m [Datum.Datum]
waitDatum = fmap Packet.messageDatum . waitReply
