-- | Alegbraic data types for OSC packets.
module Sound.OpenSoundControl.OSC.Type ( OSC(..)
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
tag (Int _) = 'i'
tag (Float _) = 'f'
tag (Double _) = 'd'
tag (String _) = 's'
tag (Blob _) = 'b'
tag (TimeStamp _) = 't'
tag (Midi _) = 'm'

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
-- Signals an error when the address @a@ doesn't conform to the OSC specification.
message :: String -> [Datum] -> OSC
message a xs =
    case a of
      ('/':_) -> Message a xs
      _ -> error "message: ill-formed address"
