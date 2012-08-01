-- | Monad class for monads that implement an Open Sound Control transport.
module Sound.OpenSoundControl.Transport.Monad where

import Control.Monad.Trans.Reader {- transformers -}
import Control.Monad.IO.Class

import Sound.OpenSoundControl.Class
import Sound.OpenSoundControl.Type
import qualified Sound.OpenSoundControl.Transport.FD as T

class (Functor m,Monad m) => Transport m where
   -- | Encode and send an OSC packet.
   sendOSC :: OSC o => o -> m ()
   -- | Receive and decode an OSC packet.
   recvPacket :: m Packet

instance (T.Transport t,Functor io,MonadIO io) => Transport (ReaderT t io) where
   sendOSC o = ReaderT (liftIO . flip T.sendOSC o)
   recvPacket = ReaderT (liftIO . T.recvPacket)

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
