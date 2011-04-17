module Sound.OpenSoundControl.NFData () where

import Control.DeepSeq (NFData(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Sound.OpenSoundControl (Datum(..), OSC(..), Time(..))

instance NFData BS.ByteString where
    rnf x1 = x1 `seq` ()

instance NFData B.ByteString where
    rnf x1 = rnf (B.toChunks x1) `seq` ()

instance NFData Time where
    rnf (UTCr x1) = rnf x1 `seq` ()
    rnf (NTPr x1) = rnf x1 `seq` ()
    rnf (NTPi x1) = rnf x1 `seq` ()

instance NFData Datum where
    rnf (Int x1)        = rnf x1 `seq` ()
    rnf (Float x1)      = rnf x1 `seq` ()
    rnf (Double x1)     = rnf x1 `seq` ()
    rnf (String x1)     = rnf x1 `seq` ()
    rnf (Blob x1)       = rnf x1 `seq` ()
    rnf (TimeStamp x1)  = rnf x1 `seq` ()
    rnf (Midi x1)       = rnf x1 `seq` ()

instance NFData OSC where
    rnf (Message x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
    rnf (Bundle x1 x2)  = rnf x1 `seq` rnf x2 `seq` ()
