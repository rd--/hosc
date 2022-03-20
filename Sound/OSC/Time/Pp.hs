-- | Pretty printing
module Sound.OSC.Time.Pp where

import qualified Data.Time as Time {- time -}
import qualified Data.Time.Clock as Clock {- time -}
import qualified Data.Time.Clock.POSIX as Clock.Posix {- time -}

import Sound.OSC.Time {- hosc -}

-- | Detailed 37-character ISO 8601 format, including fractional seconds and '+0000' suffix.
iso_8601_fmt :: String
iso_8601_fmt = "%Y-%m-%dT%H:%M:%S,%q+0000"

{- | Parse time according to 'iso_8601_fmt'

> iso_8601_to_utctime "2015-11-26T00:29:37,145875000000+0000"
-}
iso_8601_to_utctime :: String -> Maybe Time.UTCTime
iso_8601_to_utctime = Time.parseTimeM True Time.defaultTimeLocale iso_8601_fmt

-- | UTC time in 'iso_8601_fmt'.
--
-- > tm <- fmap (utctime_to_iso_8601 . Clock.Posix.posixSecondsToUTCTime) Clock.Posix.getPOSIXTime
-- > (length tm,sum [4+1+2+1+2,1,2+1+2+1+2,1,12,1,4],sum [10,1,8,1,12,1,4]) == (37,37,37)
utctime_to_iso_8601 :: Time.UTCTime -> String
utctime_to_iso_8601 = Time.formatTime Time.defaultTimeLocale iso_8601_fmt

{- | ISO 8601 of 'Time'.

> tm <- fmap ntpr_to_iso_8601 time
> import System.Process {- process -}
> rawSystem "date" ["-d",tm]

> t = 15708783354150518784
> s = "2015-11-26T00:22:19,366058349609+0000"
> ntpr_to_iso_8601 (ntpi_to_ntpr t) == s
-}
ntpr_to_iso_8601 :: NtpReal -> String
ntpr_to_iso_8601 = utctime_to_iso_8601 . Clock.Posix.posixSecondsToUTCTime . ntpr_to_posixtime

{- | 'Time' of ISO 8601.

> t = 15708783354150518784
> s = "2015-11-26T00:22:19,366058349609+0000"
> fmap ntpr_to_ntpi (iso_8601_to_ntpr s) == Just t
-}
iso_8601_to_ntpr :: String -> Maybe NtpReal
iso_8601_to_ntpr = fmap (posixtime_to_ntpr . Clock.Posix.utcTimeToPOSIXSeconds) . iso_8601_to_utctime

{- | Alias for 'ntpr_to_iso_8601'.

> time_pp immediately == "1900-01-01T00:00:00,000000000000+0000"
> fmap time_pp time
-}
time_pp :: NtpReal -> String
time_pp = ntpr_to_iso_8601
