module Status.Formatter where

import Text.Format qualified as F
import Status.Config
import Text.Format ((~~), (~%))
import Data.HashMap.Strict qualified as HM
import Control.Concurrent.MVar
import Data.Text qualified as T
import Data.String
import Status.Plugins.WirelessInfo 
import Status.Plugins.BatteryInfo 
import Status.Plugins.AudioInfo
import Status.Plugins.CpuInfo 
import Status.Plugins.MemInfo 
import Status.Plugins.Clock 
import Data.Bifunctor qualified as BFu 
import Data.Bool (bool)
newtype FormatMap b  = FormatMap { fromFormatMap :: HM.HashMap String b } 

formatGeneral :: F.Format -> SystemInfo -> String
formatGeneral format (SystemInfo 
                        (Just meminfo) 
                        (Just cpuinfo) 
                        (Just batteryinfo)
                        (Just wirelessinfo)
                        (Just clockinfo)
                        (Just  audioinfo)
                        fifoinfos) = 
    format 
    ~~ ("cpu" ~% cpuinfo) 
    ~~ ("memory" ~% meminfo) 
    ~~ ("battery" ~% batteryinfo) 
    ~~ ("wireless" ~% wirelessinfo) 
    ~~ ("clock" ~% clockinfo) 
    ~~ ("audio" ~% audioinfo)
    ~~ FormatMap fifoinfos

formatGeneral _ _ = ""

printInfo :: Settings -> MVar SystemInfo -> SystemMask -> IO ()
printInfo config mvar (SystemMask bmem bcpu bbat bwi bclc baux bfifo) = 
    let 
        format = fromString (T.unpack $ settingsFormat config) 
    in 
        do 
            (SystemInfo mmem mcpu mbat mwi mclc maux mfifo) <- takeMVar mvar
            let 
                meminf = memUsage config 
                cpuinf = cpuUsage config 
                battinf = showBatteryInfo config <$> batteryInfo config 
                wiinf = getDisplayWirelessInfo config
                clkinf = getTime config 
                auxinf = getAudioStr config  
            meminfo <- bool meminf (maybe meminf pure mmem) bmem
            cpuinfo <- bool cpuinf (maybe cpuinf pure mcpu) bcpu
            batteryinfo <- bool battinf (maybe battinf pure mbat) bbat
            wirelessinfo <- bool wiinf (maybe wiinf pure mwi) bwi 
            clockinfo <- bool clkinf (maybe clkinf pure mclc) bclc  
            audioinfo <- bool auxinf (maybe auxinf pure maux) baux
            let sysinfo =  SystemInfo 
                            (Just meminfo) 
                            (Just cpuinfo) 
                            (Just batteryinfo)
                            (Just wirelessinfo)
                            (Just clockinfo)
                            (Just audioinfo)
                            mfifo
            putMVar mvar sysinfo
            putStrLn $ formatGeneral format sysinfo 

instance F.Formattable b => F.Hole (FormatMap b) where 
   hole (FormatMap x) = 
       let 
           daList = HM.toList x 
       in 
           map (uncurry (~%) . BFu.first ("dbus" ++)) daList
