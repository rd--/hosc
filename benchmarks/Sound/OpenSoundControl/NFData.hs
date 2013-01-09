module Sound.OpenSoundControl.NFData () where

import Control.DeepSeq (NFData(..))
import Sound.OpenSoundControl

instance NFData Datum where
    rnf (Int x1)        = rnf x1 `seq` ()
    rnf (Float x1)      = rnf x1 `seq` ()
    rnf (Double x1)     = rnf x1 `seq` ()
    rnf (String x1)     = rnf x1 `seq` ()
    rnf (Blob x1)       = rnf x1 `seq` ()
    rnf (TimeStamp x1)  = rnf x1 `seq` ()
    rnf (Midi x1)       = rnf x1 `seq` ()

instance NFData Message where
    rnf (Message x1 x2) = rnf x1 `seq` rnf x2 `seq` ()

instance NFData Bundle where
    rnf (Bundle x1 x2)  = rnf x1 `seq` rnf x2 `seq` ()

instance NFData Packet where
    rnf (Packet_Message x1) = rnf x1 `seq` ()
    rnf (Packet_Bundle x1) = rnf x1 `seq` ()
