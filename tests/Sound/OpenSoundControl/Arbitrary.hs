module Sound.OpenSoundControl.Arbitrary () where

import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Sound.OpenSoundControl (Datum(..), OSC(..), Time(..), NTPi)
import Test.QuickCheck

instance Arbitrary Time where
    arbitrary = oneof [
        UTCr <$> realToFrac   <$> (arbitrary :: Gen (NonNegative Double))
      , NTPr <$> realToFrac   <$> (arbitrary :: Gen (NonNegative Double))
      , NTPi <$> fromIntegral <$> (arbitrary :: Gen (Positive NTPi))
      ]

instance Arbitrary Datum where
    arbitrary = oneof [
        Int       <$> arbitrary
      , Float     <$> realToFrac <$> (arbitrary :: Gen Float)
      , Double    <$> arbitrary
      , String    <$> genString
      , Blob      <$> B.pack <$> resize 128 arbitrary
      , TimeStamp <$> arbitrary
      , Midi      <$> arbitrary
      ]

genString :: Gen String
genString = resize 128 (listOf (arbitrary `suchThat` (/= '\0')))

genMessage :: Gen OSC
genMessage = Message <$> ("/"++) <$> genString <*> resize 32 (listOf1 arbitrary)

instance Arbitrary OSC where
    arbitrary = oneof [
        genMessage
      , Bundle <$> arbitrary <*> resize 32 (listOf1 genMessage) ]
