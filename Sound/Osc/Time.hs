-- | Osc related timing functions.
--   Osc timestamps are 64-bit @Ntp@ values, <http://ntp.org/>.
module Sound.Osc.Time where

import Data.Word {- base -}

import qualified Data.Time as Time {- time -}
import qualified Data.Time.Clock as Clock {- time -}
import qualified Data.Time.Clock.POSIX as Clock.Posix {- time -}

import Sound.Osc.Coding.Convert {- hosc -}

-- * Temporal types

-- | Type for binary (integeral) representation of a 64-bit Ntp timestamp (ie. ntpi).
--   The Ntp epoch is January 1, 1900.
--   Ntp v4 also includes a 128-bit format, which is not used by Osc.
type Ntp64 = Word64

-- | @Ntp@ time in real-valued (fractional) form.
type NtpReal = Double

-- | @Unix/Posix@ time in real-valued (fractional) form.
--   The Unix/Posix epoch is January 1, 1970.
type PosixReal = Double

-- * Time conversion

{- | Convert an NtpReal timestamp to an Ntp64 timestamp.

>>> ntpr_to_ntpi 0
0

> fmap ntpr_to_ntpi time
-}
ntpr_to_ntpi :: NtpReal -> Ntp64
ntpr_to_ntpi t = round (t * (2 ^ (32::Int)))

{- | Convert an 'Ntp64' timestamp to a real-valued Ntp timestamp.

>>> ntpi_to_ntpr 0
0.0
-}
ntpi_to_ntpr :: Ntp64 -> NtpReal
ntpi_to_ntpr t = word64_to_double t / 2^(32::Int)

{- | Difference (in seconds) between /Ntp/ and /Posix/ epochs.

>>> ntp_posix_epoch_diff / (24 * 60 * 60)
25567.0

>>> 25567 `div` 365
70
-}
ntp_posix_epoch_diff :: Num n => n
ntp_posix_epoch_diff = (70 * 365 + 17) * 24 * 60 * 60

-- | Convert a PosixReal timestamp to an Ntp64 timestamp.
posix_to_ntpi :: PosixReal -> Ntp64
posix_to_ntpi t = ntpr_to_ntpi (t + ntp_posix_epoch_diff)

-- | Convert @Unix/Posix@ to @Ntp@.
posix_to_ntpr :: Num n => n -> n
posix_to_ntpr = (+) ntp_posix_epoch_diff

-- | Convert @Ntp@ to @Unix/Posix@.
ntpr_to_posix :: Num n => n -> n
ntpr_to_posix = (+) (negate ntp_posix_epoch_diff)

-- | Convert 'Ntp64' to @Unix/Posix@.
ntpi_to_posix :: Ntp64 -> PosixReal
ntpi_to_posix = ntpr_to_posix . ntpi_to_ntpr

-- | Convert 'Time' to 'Clock.Posix.POSIXTime'.
ntpr_to_posixtime :: NtpReal -> Clock.Posix.POSIXTime
ntpr_to_posixtime = realToFrac . ntpr_to_posix

-- | Convert 'Clock.Posix.POSIXTime' to 'Time'.
posixtime_to_ntpr :: Clock.Posix.POSIXTime -> NtpReal
posixtime_to_ntpr = posix_to_ntpr . realToFrac

-- * 'Data.Time' inter-operation.

-- | The time at 1970-01-01:00:00:00 which is the Unix/Posix epoch.
posix_epoch :: Time.UTCTime
posix_epoch =
    let d = Time.fromGregorian 1970 1 1
        s = fromInteger 0 -- Time.secondsToDiffTime
    in Time.UTCTime d s

-- | Convert 'Time.UTCTime' to @Unix/Posix@.
utc_to_posix :: Fractional n => Time.UTCTime -> n
utc_to_posix t = realToFrac (Time.diffUTCTime t posix_epoch)

-- * Clock operations

-- | utc_to_posix of Clock.getCurrentTime.
getCurrentTimeAsPosix :: IO PosixReal
getCurrentTimeAsPosix = fmap utc_to_posix Clock.getCurrentTime

{- | realToFrac of Clock.Posix.getPOSIXTime

> get_ct = getCurrentTimeAsPosix
> get_pt = getPosixTimeAsPosix
> (ct,pt) <- get_ct >>= \t0 -> get_pt >>= \t1 -> return (t0,t1)
> print (pt - ct,pt - ct < 1e-5)
-}
getPosixTimeAsPosix :: IO PosixReal
getPosixTimeAsPosix = fmap realToFrac Clock.Posix.getPOSIXTime

-- | Read current real-valued @Ntp@ timestamp.
currentTime :: IO NtpReal
currentTime = fmap posixtime_to_ntpr Clock.Posix.getPOSIXTime
