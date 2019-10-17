-- | Monad class implementing an Open Sound Control transport.
module Sound.OSC.Transport.Monad where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Control.Monad.Trans.Reader as R {- transformers -}
import qualified Control.Monad.IO.Class as M {- transformers -}

import qualified Sound.OSC.Datum as Datum {- hosc -}
import qualified Sound.OSC.Transport.FD as FD {- hosc -}
import qualified Sound.OSC.Packet as Packet {- hosc -}
import qualified Sound.OSC.Wait as Wait {- hosc -}

-- | Sender monad.
class Monad m => SendOSC m where
   -- | Encode and send an OSC packet.
   sendPacket :: Packet.Packet -> m ()

-- | Receiver monad.
class Monad m => RecvOSC m where
   -- | Receive and decode an OSC packet.
   recvPacket :: m Packet.Packet

-- | 'DuplexOSC' is the union of 'SendOSC' and 'RecvOSC'.
class (SendOSC m,RecvOSC m) => DuplexOSC m where

-- | 'Transport' is 'DuplexOSC' with a 'MonadIO' constraint.
class (DuplexOSC m,M.MonadIO m) => Transport m where

-- | 'SendOSC' over 'ReaderT'.
instance (FD.Transport t,M.MonadIO io) => SendOSC (R.ReaderT t io) where
   sendPacket p = R.ReaderT (M.liftIO . flip FD.sendPacket p)

-- | 'RecvOSC' over 'ReaderT'.
instance (FD.Transport t,M.MonadIO io) => RecvOSC (R.ReaderT t io) where
   recvPacket = R.ReaderT (M.liftIO . FD.recvPacket)

-- | 'DuplexOSC' over 'ReaderT'.
instance (FD.Transport t,M.MonadIO io) => DuplexOSC (R.ReaderT t io) where

-- | 'Transport' over 'ReaderT'.
instance (FD.Transport t,M.MonadIO io) => Transport (R.ReaderT t io) where

-- | Transport connection.
type Connection t a = R.ReaderT t IO a

-- | Bracket Open Sound Control communication.
withTransport :: FD.Transport t => IO t -> Connection t r -> IO r
withTransport u = FD.withTransport u . R.runReaderT

-- | 'void' of 'withTransport'.
withTransport_ :: FD.Transport t => IO t -> Connection t r -> IO ()
withTransport_ u = void . withTransport u

-- * Send

-- | Type restricted synonym for 'sendOSC'.
sendMessage :: SendOSC m => Packet.Message -> m ()
sendMessage = sendPacket . Packet.Packet_Message

-- | Type restricted synonym for 'sendOSC'.
sendBundle :: SendOSC m => Packet.Bundle -> m ()
sendBundle = sendPacket . Packet.Packet_Bundle

-- * Receive

-- | Variant of 'recvPacket' that runs 'packet_to_bundle'.
recvBundle :: (RecvOSC m) => m Packet.Bundle
recvBundle = liftM Packet.packet_to_bundle recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_message'.
recvMessage :: (RecvOSC m) => m (Maybe Packet.Message)
recvMessage = liftM Packet.packet_to_message recvPacket

-- | Erroring variant.
recvMessage_err :: RecvOSC m => m Packet.Message
recvMessage_err = fmap (fromMaybe (error "recvMessage")) recvMessage

-- | Variant of 'recvPacket' that runs 'packetMessages'.
recvMessages :: (RecvOSC m) => m [Packet.Message]
recvMessages = liftM Packet.packetMessages recvPacket

-- * Wait

-- | Wait for a 'Packet' where the supplied predicate is 'True',
-- discarding intervening packets.
waitUntil :: (RecvOSC m) => (Packet.Packet -> Bool) -> m Packet.Packet
waitUntil f = Wait.untilPredicate f recvPacket

-- | Wait for a 'Packet' where the supplied function does not give
-- 'Nothing', discarding intervening packets.
waitFor :: (RecvOSC m) => (Packet.Packet -> Maybe a) -> m a
waitFor f = Wait.untilMaybe f recvPacket

-- | 'waitUntil' 'packet_is_immediate'.
waitImmediate :: RecvOSC m => m Packet.Packet
waitImmediate = waitUntil Packet.packet_is_immediate

-- | 'waitFor' 'packet_to_message', ie. an incoming 'Message' or
-- immediate mode 'Bundle' with one element.
waitMessage :: RecvOSC m => m Packet.Message
waitMessage = waitFor Packet.packet_to_message

-- | A 'waitFor' for variant using 'packet_has_address' to match on
-- the 'Address_Pattern' of incoming 'Packets'.
waitAddress :: RecvOSC m => Packet.Address_Pattern -> m Packet.Packet
waitAddress s =
    let f o = if Packet.packet_has_address s o then Just o else Nothing
    in waitFor f

-- | Variant on 'waitAddress' that returns matching 'Message'.
waitReply :: RecvOSC m => Packet.Address_Pattern -> m Packet.Message
waitReply s =
    let f = fromMaybe (error "waitReply: message not located?") .
            find (Packet.message_has_address s) .
            Packet.packetMessages
    in liftM f (waitAddress s)

-- | Variant of 'waitReply' that runs 'messageDatum'.
waitDatum :: RecvOSC m => Packet.Address_Pattern -> m [Datum.Datum]
waitDatum = liftM Packet.messageDatum . waitReply
