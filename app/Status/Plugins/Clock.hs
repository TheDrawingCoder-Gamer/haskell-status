module Status.Plugins.Clock where 

import Data.Time qualified as Ti
import Data.Time.Format qualified as TF
import Status.Config
import Data.Functor.Identity
import Data.Text qualified as T 
getTime :: Settings -> IO String 
getTime Settings{settingsClock=ClockSettings{clockFormat=cformat}} =  
    TF.formatTime TF.defaultTimeLocale (T.unpack cformat) <$> Ti.getZonedTime

