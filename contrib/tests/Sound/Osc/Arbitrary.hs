module Sound.Osc.Arbitrary () where

import qualified Data.ByteString.Char8 as C {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}

import Test.QuickCheck {- QuickCheck -}

import Sound.Osc {- hosc -}
import Sound.Osc.Coding.Convert {- hosc -}

-- | Avoid floating point representation/conversion errors
genTime :: Gen Time
genTime = ntpi_to_ntpr <$> arbitrary

-- | Geneate only ASCII strings
genString :: Gen String
genString = map (toEnum . word8_to_int) <$> resize 128 (listOf (arbitrary `suchThat` (/= 0)))

genAscii :: Gen Ascii
genAscii = fmap C.pack genString

genMidiData :: Gen MidiData
genMidiData = do
  (p,q,r,s) <- arbitrary
  return (MidiData p q r s)

instance Arbitrary Datum where
    arbitrary = oneof [
        Int32 <$> arbitrary
      , Float <$> realToFrac <$> (arbitrary :: Gen Float)
      , Double <$> arbitrary
      , AsciiString <$> genAscii
      , Blob <$> B.pack <$> resize 128 arbitrary
      , TimeStamp <$> genTime
      , Midi <$> genMidiData
      ]

genMessage :: Gen Message
genMessage = message <$> ("/"++) <$> genString <*> resize 32 (listOf1 arbitrary)

instance Arbitrary Packet where
    arbitrary = oneof [
        Packet_Message <$> genMessage
      , p_bundle <$> genTime <*> resize 32 (listOf1 genMessage)
      ]
