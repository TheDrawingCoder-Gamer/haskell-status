
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
import Status.Formatter
import Data.Stringly
main :: IO ()
main =
    do
        config' <- completeSettings <$> getConfig

        hSetBuffering stdout LineBuffering
        case config' of 
            Right config -> 
                do
                    deadMansMVar <- newEmptyMVar 
                    goodMvar <- newMVar (SystemInfo Nothing Nothing Nothing Nothing Nothing Nothing HM.empty)
                    forkFinally (timerThread config goodMvar) (\_ -> putMVar deadMansMVar ())
                    takeMVar deadMansMVar
            Left config -> 
                print config
   
timerThread :: Settings -> MVar SystemInfo -> IO ()
timerThread config mvar = 
    forever $ do 
        printInfo config mvar (SystemMask False False False False False False HM.empty)
        threadDelay 5000000




