-- | An implementation of a subset of the /Open Sound Control/ byte
-- protocol, documented at <http://opensoundcontrol.org/>.
--
-- For the most part this top-level module is the only import
-- required.  It provides the 'Datum' and 'OSC' types, 'encodeOSC' and
-- 'decodeOSC' functions, basic 'UDP' and 'TCP' 'Transport' layers,
-- and basic temporal operations 'utcr' to access the current time and
-- 'pauseThread' to delay the current thread.
--
-- > let o = Bundle immediately [Message "/g_free" [Int 0]]
-- > in decodeOSC (encodeOSC o) == o
module Sound.OpenSoundControl (module O) where

import Sound.OpenSoundControl.Class as O
import Sound.OpenSoundControl.Type as O
import Sound.OpenSoundControl.Time as O
import Sound.OpenSoundControl.Transport as O
import Sound.OpenSoundControl.Transport.UDP as O
import Sound.OpenSoundControl.Transport.TCP as O
