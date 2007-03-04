module Sound.OpenSoundControl.TCP
   (TCP, tcp, send, recv, wait, close, withTCP) where

import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.OSC

import Control.Exception (bracket)
import Data.Int
import System.IO
import Network
import qualified Data.ByteString.Lazy as B

type TCP = Handle

-- | Make a TCP connection.
tcp :: String -> Int -> IO TCP
tcp host port = connectTo host (PortNumber (fromIntegral port))

-- | Encode and send an OSC packet over a TCP connection. 
send :: TCP -> OSC -> IO ()
send fd msg = do let b = encodeOSC msg
                     n = fromIntegral (B.length b)
                 B.hPut fd (B.append (encode_u32 n) b)
                 hFlush fd

-- | Receive and decode an OSC packet over a UDP connection. 
recv :: TCP -> IO OSC
recv fd = do b0 <- B.hGet fd 4
             b1 <- B.hGet fd (fromIntegral (decode_u32 b0))
             return (decodeOSC b1)

-- | Does the OSC message have the specified address.
hasAddress :: String -> OSC -> Bool
hasAddress addr (Message s _) = s == addr
hasAddress _    (Bundle _ _)  = False

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p act =
   let recurse =
         do r <- act
            if p r then return r else recurse
   in  recurse

-- | Wait for an OSC message with the specified address, discard intervening messages.
wait :: TCP -> String -> IO OSC
wait fd s = untilM (hasAddress s) (recv fd)

-- | Close a UDP connection.
close :: TCP -> IO ()
close = hClose

-- | Bracket UDP activity.
withTCP :: IO TCP -> (TCP -> IO a) -> IO a
withTCP u = bracket u close
