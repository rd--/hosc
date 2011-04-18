{-# LANGUAGE ScopedTypeVariables #-}

import           Sound.OpenSoundControl (OSC)
import           Sound.OpenSoundControl.Arbitrary ()
import qualified Sound.OpenSoundControl.OSC.Decode as Decode
import qualified Sound.OpenSoundControl.OSC.Encode as Encode
import qualified Sound.OpenSoundControl.OSC.Binary as Binary
import qualified Sound.OpenSoundControl.OSC.Builder as Builder
import           Test.Framework (Test, defaultMain)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests =
    [ testProperty "encodeOSC (Builder)" $ \(osc :: OSC) ->
        Builder.encodeOSC osc == Encode.encodeOSC osc
    , testProperty "encodeOSC/decodeOSC" $ \(osc :: OSC) ->
        Decode.decodeOSC (Encode.encodeOSC osc) == osc
    , testProperty "decodeOSC (Get)" $ \(osc :: OSC) ->
        Binary.decodeOSC (Encode.encodeOSC osc) == osc
    ]

main :: IO ()
main = defaultMain tests
