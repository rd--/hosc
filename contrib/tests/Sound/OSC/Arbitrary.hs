module Sound.OSC.Arbitrary () where

import qualified Data.ByteString.Char8 as C {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Test.QuickCheck {- QuickCheck -}

import Sound.OSC {- hosc -}
import Sound.OSC.Coding.Convert {- hosc -}

-- | Avoid floating point representation/conversion errors
genTime :: Gen Time
genTime = ntpi_to_ntpr <$> arbitrary

-- | Geneate only ASCII strings
genString :: Gen String
genString = map (toEnum . word8_to_int) <$> resize 128 (listOf (arbitrary `suchThat` (/= 0)))

genASCII :: Gen ASCII
genASCII = fmap C.pack genString

genMIDI :: Gen MIDI
genMIDI = do
  (p,q,r,s) <- arbitrary
  return (MIDI p q r s)

instance Arbitrary Datum where
    arbitrary = oneof [
        Int32 <$> arbitrary
      , Float <$> realToFrac <$> (arbitrary :: Gen Float)
      , Double <$> arbitrary
      , ASCII_String <$> genASCII
      , Blob <$> B.pack <$> resize 128 arbitrary
      , TimeStamp <$> genTime
      , Midi <$> genMIDI
      ]

genMessage :: Gen Message
genMessage = message <$> ("/"++) <$> genString <*> resize 32 (listOf1 arbitrary)

instance Arbitrary Packet where
    arbitrary = oneof [
        Packet_Message <$> genMessage
      , p_bundle <$> genTime <*> resize 32 (listOf1 genMessage)
      ]
