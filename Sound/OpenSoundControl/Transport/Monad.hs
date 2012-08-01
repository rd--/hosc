-- | Monad class implementing an Open Sound Control transport.
module Sound.OpenSoundControl.Transport.Monad where

import Control.Monad.Trans.Reader {- transformers -}
import Control.Monad.IO.Class as M
import Data.List
import Data.Maybe
import Sound.OpenSoundControl.Class
import qualified Sound.OpenSoundControl.Transport.FD as T
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Wait

class (Functor m,Monad m,MonadIO m) => Transport m where
   -- | Encode and send an OSC packet.
   sendOSC :: OSC o => o -> m ()
   -- | Receive and decode an OSC packet.
   recvPacket :: m Packet

instance (T.Transport t,Functor io,MonadIO io) => Transport (ReaderT t io) where
   sendOSC o = ReaderT (M.liftIO . flip T.sendOSC o)
   recvPacket = ReaderT (M.liftIO . T.recvPacket)

-- | 'M.liftIO'
liftIO :: Transport m => IO a -> m a
liftIO = M.liftIO

-- | Transport connection.
type Connection t a = ReaderT t IO a

-- | Bracket Open Sound Control communication.
withTransport :: T.Transport t => IO t -> Connection t a -> IO a
withTransport u = T.withTransport u . runReaderT

-- * Send

-- | Type restricted synonym for 'sendOSC'.
sendMessage :: Transport m => Message -> m ()
sendMessage = sendOSC

-- | Type restricted synonym for 'sendOSC'.
sendBundle :: Transport m => Bundle -> m ()
sendBundle = sendOSC

-- * Receive

-- | Variant of 'recvPacket' that runs 'fromPacket'.
recvOSC :: (Transport m,OSC o) => m (Maybe o)
recvOSC = fmap fromPacket recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_bundle'.
recvBundle :: (Transport m) => m Bundle
recvBundle = fmap packet_to_bundle recvPacket

-- | Variant of 'recvPacket' that runs 'packet_to_message'.
recvMessage :: (Transport m) => m (Maybe Message)
recvMessage = fmap packet_to_message recvPacket

-- | Variant of 'recvPacket' that runs 'packetMessages'.
recvMessages :: (Transport m) => m [Message]
recvMessages = fmap packetMessages recvPacket

-- * Wait

-- | Wait for a 'Packet' where the supplied predicate is 'True',
-- discarding intervening packets.
waitUntil :: (Transport m) => (Packet -> Bool) -> m Packet
waitUntil f = untilPredicate f recvPacket

-- | Wait for a 'Packet' where the supplied function does not give
-- 'Nothing', discarding intervening packets.
waitFor :: (Transport m) => (Packet -> Maybe a) -> m a
waitFor f = untilMaybe f recvPacket

-- | 'waitUntil' 'packet_is_immediate'.
waitImmediate :: Transport m => m Packet
waitImmediate = waitUntil packet_is_immediate

-- | 'waitFor' 'packet_to_message', ie. an incoming 'Message' or
-- immediate mode 'Bundle' with one element.
waitMessage :: Transport m => m Message
waitMessage = waitFor packet_to_message

-- | A 'waitFor' for variant using 'packet_has_address' to match on
-- the 'Address_Pattern' of incoming 'Packets'.
waitAddress :: Transport m => Address_Pattern -> m Packet
waitAddress s =
    let f o = if packet_has_address s o then Just o else Nothing
    in waitFor f

-- | Variant on 'waitAddress' that returns matching 'Message'.
waitReply :: Transport m => Address_Pattern -> m Message
waitReply s =
    let f = fromMaybe (error "waitReply: message not located?") .
            find (message_has_address s) .
            packetMessages
    in fmap f (waitAddress s)

-- | Variant of 'waitReply' that runs 'messageDatum'.
waitDatum :: Transport m => Address_Pattern -> m [Datum]
waitDatum = fmap messageDatum . waitReply
