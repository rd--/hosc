-- hosc does not directly support a timeout at recv, however the
-- standard libraries have the 'System.Timeout.timeout' function that
-- works for interupting socket reads.

import Sound.OpenSoundControl
import qualified System.Timeout as T

timeout :: Double -> IO a -> IO (Maybe a)
timeout t =
    let i = floor (t * 1000000)
    in T.timeout i

main :: IO ()
main = do
  fd <- openUDP "127.0.0.1" 57110
  r <- timeout 0.5 (recv fd)
  print r
