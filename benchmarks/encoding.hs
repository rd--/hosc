import Criterion.Main

import qualified Data.ByteString.Lazy as B
import Sound.OpenSoundControl (Datum(..), OSC(..), Time(..))
import qualified Sound.OpenSoundControl.OSC.Encode as Encode
import qualified Sound.OpenSoundControl.OSC.Encoding as Encoding
import Sound.OpenSoundControl.NFData ()

type EncodingFunc = OSC -> B.ByteString

main :: IO ()
main =
    defaultMain [
         bench "OSC.Encoding" $ nf (Encode.encodeOSC :: EncodingFunc)   b
       , bench "OSC.Builder"  $ nf (Encoding.encodeOSC :: EncodingFunc) b
       ]
    where
        m = Message "/fooblah" [Float 42, Int 16, String "yeah", Blob (B.pack [0..128])]
        b = Bundle (NTPr pi) (replicate 12 m)
