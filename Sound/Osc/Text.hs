-- | A simple and unambigous text encoding for Osc.
module Sound.Osc.Text where

import Numeric {- base -}
import Text.Printf {- base -}

import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet  {- hosc3 -}
import Sound.Osc.Time  {- hosc3 -}

-- | Precision value for floating point numbers.
type FpPrecision = Maybe Int

{- | Variant of 'showFFloat' that deletes trailing zeros.

> map (showFloatWithPrecision (Just 4)) [1, 2.0, pi] == ["1.0", "2.0", "3.1416"]
-}
showFloatWithPrecision :: RealFloat n => FpPrecision -> n -> String
showFloatWithPrecision p n =
    let s = showFFloat p n ""
        s' = dropWhile (== '0') (reverse s)
    in case s' of
         '.':_ -> reverse ('0' : s')
         _ -> reverse s'

-- | Hex encoded byte sequence.
showBytes :: [Int] -> String
showBytes = concatMap (printf "%02x")

{- | Escape whites space (space, tab, newline) and the escape character (backslash).

> mapM_ (putStrLn .  escapeString) ["str", "str ", "st r", "s\tr", "s\\tr", "\nstr"]
-}
escapeString :: String -> String
escapeString txt =
  case txt of
    [] -> []
    c:txt' -> if c `elem` "\\\t\n " then '\\'  : c : escapeString txt' else c : escapeString txt'

{- | Printer for 'Datum'.

> let d = [Int32 1,Float 1.2,string "str",midi (0,0x90,0x40,0x60),blob [12,16], TimeStamp 100.0]
> map (showDatum (Just 5)) d == ["1","1.2","str","00904060","0c10","429496729600"]
-}
showDatum :: FpPrecision -> Datum -> String
showDatum p d =
    case d of
      Int32 n -> show n
      Int64 n -> show n
      Float n -> showFloatWithPrecision p n
      Double n -> showFloatWithPrecision p n
      AsciiString s -> escapeString (ascii_to_string s)
      Blob s -> showBytes (blob_unpack_int s)
      TimeStamp t -> show (ntpr_to_ntpi t)
      Midi m -> showBytes (midi_unpack_int m)

{- | Printer for Message.

> let format = showMessage (Just 4)
> format (Message "/c_set" [Int32 1, Float 2.3])
> format (Message "/s_new" [string "sine", Int32 (-1), Int32 1, Int32 1])
> format (Message "/addr" [Int32 1, Int64 2, Float 3, Double 4, string "five", blob [6, 7], midi (8, 9, 10, 11)])
-}
showMessage :: FpPrecision -> Message -> String
showMessage precision aMessage =
  unwords
  [messageAddress aMessage
  ,messageSignature aMessage
  ,unwords (map (showDatum precision) (messageDatum aMessage))]

{- | Printer for Bundle

> let format = showBundle (Just 4)
> format (Bundle 1 [Message "/c_set" [Int32 1, Float 2.3, Int64 4, Double 5.6], Message "/memset" [string "addr", blob [7, 8]]])
-}
showBundle :: FpPrecision -> Bundle -> String
showBundle precision aBundle =
  unwords
  ["#bundle"
  ,show (ntpr_to_ntpi (bundleTime aBundle))
  ,unwords (map (showMessage precision) (bundleMessages aBundle))]

showPacket :: FpPrecision -> Packet -> String
showPacket precision = at_packet (showMessage precision) (showBundle precision)
