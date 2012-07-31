-- | Monad class for monads that implement an Open Sound Control transport.
module Sound.OpenSoundControl.Transport.Monad where

import Sound.OpenSoundControl.Class
import Sound.OpenSoundControl.Type
import qualified Sound.OpenSoundControl.Transport as T

import Control.Monad.Trans.Reader (ReaderT(ReaderT))
import Control.Monad.IO.Class (MonadIO,liftIO)

class Monad m => Connection m where
   -- | Encode and send an OSC packet.
   sendOSC :: OSC o => o -> m ()
   -- | Receive and decode an OSC packet.
   recvPacket :: m Packet

instance (T.Transport t, MonadIO io) => Connection (ReaderT t io) where
   sendOSC o = ReaderT (liftIO . flip T.sendOSC o)
   recvPacket = ReaderT (liftIO . T.recvPacket)
