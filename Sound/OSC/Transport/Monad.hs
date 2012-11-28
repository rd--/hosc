-- | Monad class implementing an Open Sound Control transport.
module Sound.OSC.Transport.Monad where

import Control.Monad (liftM)
import Control.Monad.Trans.Reader {- transformers -}
import Control.Monad.IO.Class as M
import Data.List
import Data.Maybe
import Sound.OpenSoundControl.Class
import qualified Sound.OSC.Transport.FD as T
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Wait

-- | Sender monad.
class Monad m => SendOSC m where
   -- | Encode and send an OSC packet.
   sendOSC :: OSC o => o -> m ()

-- | Receiver monad.
class Monad m => RecvOSC m where
   -- | Receive and decode an OSC packet.
   recvPacket :: m Packet

-- | 'DuplexOSC' is the union of 'SendOSC' and 'RecvOSC'.
class (SendOSC m,RecvOSC m) => DuplexOSC m where

-- | 'Transport' is 'DuplexOSC' with a 'MonadIO' constraint.
class (DuplexOSC m,MonadIO m) => Transport m where

instance (T.Transport t,MonadIO io) => SendOSC (ReaderT t io) where
   sendOSC o = ReaderT (M.liftIO . flip T.sendOSC o)

instance (T.Transport t,MonadIO io) => RecvOSC (ReaderT t io) where
   recvPacket = ReaderT (M.liftIO . T.recvPacket)

instance (T.Transport t,MonadIO io) => DuplexOSC (ReaderT t io) where

instance (T.Transport t,MonadIO io) => Transport (ReaderT t io) where

-- | Transport connection.
type Connection t a = ReaderT t IO a

-- | Bracket Open Sound Control communication.
withTransport :: T.Transport t => IO t -> Connection t a -> IO a
withTransport u = T.withTransport u . runReaderT

-- * Send

-- | Type restricted synonym for 'sendOSC'.
sendMessage :: SendOSC m => Message -> m ()
sendMessage = sendOSC

-- | Type restricted synonym for 'sendOSC'.
sendBundle :: SendOSC m => Bundle -> m ()
sendBundle = sendOSC

-- * Receive

-- | Variant of 'recvPacket' that runs 'fromPacket'.
recvOSC :: (RecvOSC m,OSC o) => m (Maybe o)
recvOSC = liftM fromPacket recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_bundle'.
recvBundle :: (RecvOSC m) => m Bundle
recvBundle = liftM packet_to_bundle recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_message'.
recvMessage :: (RecvOSC m) => m (Maybe Message)
recvMessage = liftM packet_to_message recvPacket

-- | Variant of 'recvPacket' that runs 'packetMessages'.
recvMessages :: (RecvOSC m) => m [Message]
recvMessages = liftM packetMessages recvPacket

-- * Wait

-- | Wait for a 'Packet' where the supplied predicate is 'True',
-- discarding intervening packets.
waitUntil :: (RecvOSC m) => (Packet -> Bool) -> m Packet
waitUntil f = untilPredicate f recvPacket

-- | Wait for a 'Packet' where the supplied function does not give
-- 'Nothing', discarding intervening packets.
waitFor :: (RecvOSC m) => (Packet -> Maybe a) -> m a
waitFor f = untilMaybe f recvPacket

-- | 'waitUntil' 'packet_is_immediate'.
waitImmediate :: RecvOSC m => m Packet
waitImmediate = waitUntil packet_is_immediate

-- | 'waitFor' 'packet_to_message', ie. an incoming 'Message' or
-- immediate mode 'Bundle' with one element.
waitMessage :: RecvOSC m => m Message
waitMessage = waitFor packet_to_message

-- | A 'waitFor' for variant using 'packet_has_address' to match on
-- the 'Address_Pattern' of incoming 'Packets'.
waitAddress :: RecvOSC m => Address_Pattern -> m Packet
waitAddress s =
    let f o = if packet_has_address s o then Just o else Nothing
    in waitFor f

-- | Variant on 'waitAddress' that returns matching 'Message'.
waitReply :: RecvOSC m => Address_Pattern -> m Message
waitReply s =
    let f = fromMaybe (error "waitReply: message not located?") .
            find (message_has_address s) .
            packetMessages
    in liftM f (waitAddress s)

-- | Variant of 'waitReply' that runs 'messageDatum'.
waitDatum :: RecvOSC m => Address_Pattern -> m [Datum]
waitDatum = liftM messageDatum . waitReply
