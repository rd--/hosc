module Sound.Osc.NFData () where

import Control.DeepSeq (NFData(..)) {- deepseq -}

import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}

instance NFData Datum where
    rnf (Int32 x1) = rnf x1 `seq` ()
    rnf (Int64 x1) = rnf x1 `seq` ()
    rnf (Float x1) = rnf x1 `seq` ()
    rnf (Double x1) = rnf x1 `seq` ()
    rnf (AsciiString x1) = rnf x1 `seq` ()
    rnf (Blob x1) = rnf x1 `seq` ()
    rnf (TimeStamp x1) = rnf x1 `seq` ()
    rnf (Midi x1) = rnf x1 `seq` ()

instance NFData MidiData where
    rnf (MidiData x1 x2 x3 x4) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` ()

instance NFData Message where
    rnf (Message x1 x2) = rnf x1 `seq` rnf x2 `seq` ()

instance NFData Bundle where
    rnf (Bundle x1 x2)  = rnf x1 `seq` rnf x2 `seq` ()

instance NFData Packet where
    rnf (Packet_Message x1) = rnf x1 `seq` ()
    rnf (Packet_Bundle x1) = rnf x1 `seq` ()
