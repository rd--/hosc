{-# LANGUAGE TypeSynonymInstances #-}
module Sound.OpenSoundControl.OSC.Encoding ( OSCEncoding(..) ) where

import qualified Blaze.ByteString.Builder as Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Sound.OpenSoundControl.OSC.Type (OSC)
import qualified Sound.OpenSoundControl.OSC.Decode as Decode
import qualified Sound.OpenSoundControl.OSC.Builder as Builder

class OSCEncoding a where
    encodeOSC :: OSC -> a
    decodeOSC :: a -> OSC

instance OSCEncoding BS.ByteString where
    encodeOSC = Builder.toByteString . Builder.buildOSC
    decodeOSC = Decode.decodeOSC . B.fromChunks . (:[])

instance OSCEncoding B.ByteString where
    encodeOSC = Builder.toLazyByteString . Builder.buildOSC
    decodeOSC = Decode.decodeOSC

instance OSCEncoding String where
    encodeOSC = BC.unpack . encodeOSC
    decodeOSC = decodeOSC . BC.pack
