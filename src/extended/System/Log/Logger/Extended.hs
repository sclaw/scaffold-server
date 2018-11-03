
module System.Log.Logger.Extended
       (
           module System.Log.Logger
         , debugMColored
         , infoMColored
         , noticeMColored
         , warningMColored
         , errorMColored
         , criticalMColored
         , alertMColored
         , emergencyMColored
       )
       where

import System.Log.Logger
import Data.Monoid.Colorful


debugMColored :: String -> String -> IO ()
debugMColored logger msg = getTerm >>= (`printColoredIO` Fg (RGB 210 84 16) (Value (logger `debugM` msg)))

infoMColored :: String -> String -> IO ()
infoMColored logger msg = getTerm >>= (`printColoredIO` Fg (RGB 3 141 255) (Value (logger `infoM` msg)))

noticeMColored :: String -> String -> IO ()
noticeMColored logger msg = getTerm >>= (`printColoredIO` Fg (RGB 154 205 50) (Value (logger `noticeM` msg)))

warningMColored :: String -> String -> IO ()
warningMColored logger msg = getTerm >>= (`printColoredIO` Fg (RGB 255 165 0) (Value (logger `warningM` msg)))

errorMColored :: String -> String -> IO ()
errorMColored logger msg = getTerm >>= (`printColoredIO` Fg (RGB 255 0 0) (Value (logger `errorM` msg)))

criticalMColored :: String -> String -> IO ()
criticalMColored logger msg = getTerm >>= (`printColoredIO` Fg Red (Value (logger `criticalM` msg)))

alertMColored :: String -> String -> IO ()
alertMColored logger msg = getTerm >>= (`printColoredIO` Fg Red (Value (logger `alertM` msg)))

emergencyMColored :: String -> String -> IO ()
emergencyMColored logger msg = getTerm >>= (`printColoredIO` Fg Red (Value (logger `emergencyM` msg)))