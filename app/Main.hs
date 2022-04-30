
module Main where

import System.Process qualified as SP
import System.Environment
import Control.Applicative
import Data.Functor ((<&>))
import Data.List.Extra (trim, splitOn)
import Data.Maybe (catMaybes, fromJust, fromMaybe, maybe)
import Data.List (find)
import Data.Bifunctor qualified as BFu
import Data.Char (isDigit)
import Control.Monad (forever) 
import Control.Concurrent (threadDelay, forkIO, forkFinally)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering), Handle, hGetContents)
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
import Data.HashMap.Strict qualified as HM
import Data.Stringly
import Status.Plugins.DBusInfo
import Status.Display
main :: IO ()
main =
    do
        config' <- completeSettings <$> getConfig

        hSetBuffering stdout LineBuffering
        case config' of 
            Right config -> 
                do
                    deadMansMVar <- newEmptyMVar
                    setupDisplay (settingsMode config) 
                    forkFinally (timerThread config) (\_ -> putMVar deadMansMVar ())
                    forkIO (setupClients config )
                    takeMVar deadMansMVar
            Left config -> 
                print config 
timerThread :: Settings -> IO ()
timerThread c@Settings{..} = 
    forever $ do 
        sysinfo <- takeMVar sysinfoMvar 
        batt <- process settingsBattery 
        cpu  <- process settingsCpu 
        mem  <- process settingsMemory 
        wireless <- process settingsWireless 
        clock <- process settingsClock 
        audio <- process settingsAudio 
        let sysinfo' = HM.unionWith (const id) sysinfo (HM.fromList [
                    ("battery", batt), 
                    ("cpu", cpu),
                    ("memory", mem),
                    ("wireless", wireless),
                    ("clock", clock),
                    ("audio", audio)])
        displaySysinfo c sysinfo' 
        putMVar sysinfoMvar sysinfo' 
        threadDelay 5000000




