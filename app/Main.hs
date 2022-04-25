
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
import Control.Concurrent (threadDelay, forkIO, forkFinally)
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
import Control.Concurrent.MVar
import Data.Bool (bool)
main :: IO ()
main =
    do
        config' <- completeSettings <$> getConfig

        hSetBuffering stdout LineBuffering
        case config' of 
            Right config -> 
                do
                    deadMansMVar <- newEmptyMVar 
                    goodMvar <- newMVar (SysInfo "" "" "" "" "" "")
                    forkFinally (timerThread config goodMvar) (\_ -> putMVar deadMansMVar ())
                    takeMVar deadMansMVar
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

data SysInfo = SysInfo String String String String String String
data SysInfoMask = SysInfoMask Bool Bool Bool Bool Bool Bool
printInfo :: Settings -> MVar SysInfo -> SysInfoMask -> IO ()
printInfo config mvar (SysInfoMask bmem bcpu bbat bwi bclc baux) = 
    let 
        format = fromString (T.unpack $ settingsFormat config) 
    in 
        do 
            (SysInfo mmem mcpu mbat mwi mclc maux) <- takeMVar mvar
            meminfo <- bool (memUsage config) (pure mmem) bmem
            cpuinfo <- bool (cpuUsage config) (pure mcpu) bcpu
            batteryinfo <- bool (showBatteryInfo config <$> batteryInfo config) (pure mbat) bbat
            wirelessinfo <- bool (getDisplayWirelessInfo config) (pure mwi) bwi 
            clockinfo <- bool (getTime config) (pure mclc) bclc  
            audioinfo <- bool (getAudioStr config) (pure maux) baux
            putMVar mvar (SysInfo meminfo cpuinfo batteryinfo wirelessinfo clockinfo audioinfo) 
            putStrLn $ formatGeneral format cpuinfo meminfo batteryinfo wirelessinfo clockinfo audioinfo 
timerThread :: Settings -> MVar SysInfo -> IO ()
timerThread config mvar = 
    forever $ do 
        printInfo config mvar (SysInfoMask False False False False False False)
        threadDelay 5000000

