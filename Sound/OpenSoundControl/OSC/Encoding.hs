{-# LANGUAGE TypeSynonymInstances #-}
module Sound.OpenSoundControl.OSC.Encoding ( Encoding(..) ) where

import qualified Blaze.ByteString.Builder as Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Sound.OpenSoundControl.OSC.Type (OSC)
import qualified Sound.OpenSoundControl.OSC.Decode as Decode
import qualified Sound.OpenSoundControl.OSC.Builder as Builder

-- | Converting from and to binary packet representations.
class Encoding a where
    -- | Decode an OSC packet.
    encodeOSC :: OSC -> a
    -- | Encode an OSC packet.
    decodeOSC :: a -> OSC

instance Encoding BS.ByteString where
    encodeOSC = Builder.toByteString . Builder.buildOSC
    decodeOSC = Decode.decodeOSC . B.fromChunks . (:[])

instance Encoding B.ByteString where
    encodeOSC = Builder.toLazyByteString . Builder.buildOSC
    decodeOSC = Decode.decodeOSC

instance Encoding String where
    encodeOSC = BC.unpack . encodeOSC
    decodeOSC = decodeOSC . BC.pack
