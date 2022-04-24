
module Main where

import System.Process qualified as SP
import System.Environment
import Control.Applicative
import Data.Functor ((<&>))
import Data.List.Extra (trim, splitOn)
import Data.Maybe (catMaybes, fromJust)
import Data.List (find)
import Data.Bifunctor qualified as BFu
import Data.Char (isDigit)
import Control.Monad (forever) 
import Control.Concurrent (threadDelay)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Status.Plugins.BatteryInfo
import Status.Plugins.CpuInfo
import Status.Plugins.MemInfo
import Status.Plugins.WirelessInfo 
import Status.Plugins.Clock
import Status.Plugins.AudioInfo
import Status.Config
import Status.Units
import Data.Concatable
import Text.Format qualified as F
import Text.Format ((~~), (~%))
import Data.String
import Data.Text qualified as T
main :: IO ()
main =
    do
        config' <- completeSettings <$> getConfig

        hSetBuffering stdout LineBuffering
        case config' of 
            Right config -> 
                do 
                    forever $ do 
                        printInfo config
                        threadDelay 5000000
            Left config -> 
                print config
formatGeneral :: F.Format -> String -> String -> String -> String -> String -> String -> String
formatGeneral format cpuinfo meminfo batteryinfo wirelessinfo clockinfo audioinfo = 
    format 
    ~~ ("cpu" ~% cpuinfo) 
    ~~ ("memory" ~% meminfo) 
    ~~ ("battery" ~% batteryinfo) 
    ~~ ("wireless" ~% wirelessinfo) 
    ~~ ("clock" ~% clockinfo) 
    ~~ ("audio" ~% audioinfo)

printInfo config = 
    let 
        format = fromString (T.unpack $ settingsFormat config) 
    in 
        do 
            meminfo <- memUsage config
            cpuinfo <- cpuUsage config
            batteryinfo <- showBatteryInfo config <$> batteryInfo config
            wirelessinfo <- getDisplayWirelessInfo config 
            clockinfo <- getTime config 
            audioinfo <- getAudioStr config  
            putStrLn $ formatGeneral format cpuinfo meminfo batteryinfo wirelessinfo clockinfo audioinfo 

