{-# LANGUAGE TypeSynonymInstances #-}
-- | A type-class to provide coding operations to different data types
--   using the same function names.
module Sound.OpenSoundControl.Coding (Coding(..) ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Sound.OpenSoundControl.Type (OSC)
import qualified Sound.OpenSoundControl.Coding.Decode.Binary as Binary
import qualified Sound.OpenSoundControl.Coding.Encode.Builder as Builder

-- | Converting from and to binary packet representations.
class Coding a where
    -- | Decode an OSC packet.
    encodeOSC :: OSC -> a
    -- | Encode an OSC packet.
    decodeOSC :: a -> OSC

instance Coding S.ByteString where
    encodeOSC = Builder.encodeOSC'
    decodeOSC = Binary.decodeOSC'

instance Coding B.ByteString where
    encodeOSC = Builder.encodeOSC
    decodeOSC = Binary.decodeOSC

instance Coding String where
    encodeOSC = C.unpack . encodeOSC
    decodeOSC = decodeOSC . C.pack
