module Status.Plugins.Clock where 

import Data.Time qualified as Ti
import Data.Time.Format qualified as TF
import Status.Config
import Data.Functor.Identity
import Data.Text qualified as T
import Status.Display 
getTime :: CClockSettings -> IO String 
getTime ClockSettings{clockFormat=FormatSettings{formatText=cformat}} =  
    TF.formatTime TF.defaultTimeLocale (T.unpack cformat) <$> Ti.getZonedTime

instance Processor CClockSettings where 
    process conf@ClockSettings{clockFormat} = do 
        time <- getTime conf 
        pure $ processFilledFormat (T.pack time) clockFormat
        
