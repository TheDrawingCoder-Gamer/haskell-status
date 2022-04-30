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
import Status.Display
data CPUInfo = CPUInfo 
    { cpuUser :: First Int 
    , cpuNice :: First Int 
    , cpuSystem :: First Int 
    , cpuIdle :: First Int } 
cpuUsage :: CPUSettings -> IO String 
cpuUsage config@CpuSettings{cpuFormatPrecision=precision} = 
    do 
        daData <- map read . take 4 . tail . words . head . lines <$> readFile "/proc/stat" :: IO [Int] 
        let (curUsage, idleUsage) = BFu.bimap sum head $ splitAt 3 daData 
        let percentage = show (roundTo precision $ (*100) $ fromIntegral curUsage / fromIntegral idleUsage) ++ "%"
        pure $ displayCpu config percentage
displayCpu :: CPUSettings -> String -> String 
displayCpu CpuSettings{cpuFormat=FormatSettings{formatText=fmat}} usage =
    fromString (T.unpack fmat) ~~ ("usage" ~% usage)
    
instance Processor CPUSettings where 
    process conf@CpuSettings{cpuFormat} = do 
        name <- displayCpu conf <$> cpuUsage conf 
        pure $ processFilledFormat (T.pack name) cpuFormat 
    
