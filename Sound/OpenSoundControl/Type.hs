-- | Alegbraic data types for OSC datum and packets.
module Sound.OpenSoundControl.Type ( OSC(..)
                                   , Datum(..)
                                   , message
                                   , bundle
                                   , tag ) where

import qualified Data.ByteString.Lazy as B
import Data.Word
import Sound.OpenSoundControl.Time

-- | The basic elements of OSC messages.
data Datum = Int Int
           | Float Double
           | Double Double
           | String String
           | Blob B.ByteString
           | TimeStamp Time
           | Midi (Word8,Word8,Word8,Word8)
             deriving (Eq, Read, Show)

-- | An OSC packet.
data OSC = Message String [Datum]
         | Bundle Time [OSC]
           deriving (Eq, Read, Show)

-- | OSC bundles can be ordered (time ascending).  Bundles and
--   messages compare EQ.
instance Ord OSC where
    compare (Bundle a _) (Bundle b _) = compare a b
    compare _ _ = EQ

-- | Single character identifier of an OSC datum.
tag :: Datum -> Char
tag dt =
    case dt of
      Int _ -> 'i'
      Float _ -> 'f'
      Double _ -> 'd'
      String _ -> 's'
      Blob _ -> 'b'
      TimeStamp _ -> 't'
      Midi _ -> 'm'

-- | Bundle constructor.
--
-- Signals an error when @xs@ is empty.
bundle :: Time -> [OSC] -> OSC
bundle t xs =
    case xs of
      [] -> error "bundle: empty?"
      _ -> Bundle t xs

-- | Message constructor
--
-- Signals an error when the address @a@ doesn't conform to the OSC
-- specification.
message :: String -> [Datum] -> OSC
message a xs =
    case a of
      ('/':_) -> Message a xs
      _ -> error "message: ill-formed address"
