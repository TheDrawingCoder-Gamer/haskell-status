module Status.Plugins.CpuInfo where 

import Status.Config 
import qualified Data.Bifunctor as BFu
import Status.Helpers
import qualified Data.Map.Strict as M
import Data.Monoid (First(..))
import Data.Functor.Identity 
import Text.Format qualified as F
import Text.Format ((~~), (~%)) 
import Data.String
import Data.Text qualified as T
data CPUInfo = CPUInfo 
    { cpuUser :: First Int 
    , cpuNice :: First Int 
    , cpuSystem :: First Int 
    , cpuIdle :: First Int } 
cpuUsage :: Settings -> IO String 
cpuUsage config@Settings{settingsCpu=CpuSettings{cpuFormatPrecision=precision}} = 
    do 
        daData <- map read . take 4 . tail . words . head . lines <$> readFile "/proc/stat" :: IO [Int] 
        let (curUsage, idleUsage) = BFu.bimap sum head $ splitAt 3 daData 
        let percentage = show (roundTo precision $ (*100) $ fromIntegral curUsage / fromIntegral idleUsage) ++ "%"
        pure $ displayCpu config percentage
displayCpu :: Settings -> String -> String 
displayCpu Settings{settingsCpu=CpuSettings{cpuFormat}} usage =
    fromString (T.unpack cpuFormat) ~~ ("usage" ~% usage)
     
    
