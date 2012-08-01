-- | Composite of non-transport related modules.
--
-- Provides the 'Datum', 'Message', 'Bundle' and 'Packet' types and
-- the 'OSC' type-class.
--
-- The byte encoding functions are 'encodeOSC' and 'decodeOSC'.
--
-- > import Sound.OpenSoundControl
-- >
-- > let o = Bundle immediately [Message "/g_free" [Int 0]]
-- > in decodeOSC (encodeOSC o) == o
module Sound.OpenSoundControl (module M) where

import Sound.OpenSoundControl.Class as M
import Sound.OpenSoundControl.Coding as M
import Sound.OpenSoundControl.Type as M
import Sound.OpenSoundControl.Time as M
import Sound.OpenSoundControl.Wait as M
