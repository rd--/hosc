import Criterion.Main

import qualified Data.ByteString.Lazy as B
import           Sound.OpenSoundControl (Datum(..), OSC(..), Time(..))
import           Sound.OpenSoundControl.NFData ()
import qualified Sound.OpenSoundControl.OSC.Binary as Binary
import qualified Sound.OpenSoundControl.OSC.Builder as Builder
import qualified Sound.OpenSoundControl.OSC.Decode as Decode
import qualified Sound.OpenSoundControl.OSC.Encode as Encode

type EncodingFunc = OSC -> B.ByteString
type DecodingFunc = B.ByteString -> OSC

main :: IO ()
main =
    defaultMain [
        bgroup "encodeOSC" [
            bench "Encode"  $ nf (Encode.encodeOSC :: EncodingFunc)  b
          , bench "Builder" $ nf (Builder.encodeOSC :: EncodingFunc) b
          ]
      , bgroup "decodeOSC" [
            bench "Decode" $ nf (Decode.decodeOSC :: DecodingFunc) p
          , bench "Binary" $ nf (Binary.decodeOSC :: DecodingFunc) p
          ]
      ]
    where
        m = Message "/fooblah" [Float 42, Int 16, String "yeah", Blob (B.pack [0..128])]
        b = Bundle (NTPr pi) (replicate 12 m)
        p = Encode.encodeOSC b
