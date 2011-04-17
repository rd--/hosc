{-# LANGUAGE ScopedTypeVariables #-}

import qualified Blaze.ByteString.Builder as Builder
import           Sound.OpenSoundControl (OSC)
import           Sound.OpenSoundControl.Arbitrary ()
import qualified Sound.OpenSoundControl.OSC.Encode as Encode
import qualified Sound.OpenSoundControl.OSC.Builder as Builder
import           Test.Framework (Test, defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests =
    [ testGroup "Sound.OpenSoundControl.ByteString"
        [ testProperty "encodeOSC" $ \(osc :: OSC) ->
            Builder.toLazyByteString (Builder.buildOSC osc) == Encode.encodeOSC osc
        -- , testProperty "encodeOSC/decodeOSC" $ \(osc :: OSC) -> OSC.decodeOSC (OSC.encodeOSC osc) == osc
        ]
    ]

main :: IO ()
main = defaultMain tests
