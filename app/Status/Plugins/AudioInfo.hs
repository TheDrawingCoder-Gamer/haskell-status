module Status.Plugins.AudioInfo where 

import System.Process
import Data.List.Extra (trim) 
import Status.Config
import Text.Format ((~~), (~%))
import Data.String
import Data.Text qualified as T
import Control.Lens.Tuple
import Control.Lens (set, view, (%~))
import System.Exit
import Status.Display
import Control.Lens qualified as L
import Control.Monad.Representable.State
import Data.Functor ((<&>))
import System.Audio.Pulse
data AudioInfo = AudioInfo 
    { audioVolume :: Int
    , audioMute   :: Bool } 
getAudioInfo :: IO AudioInfo 
getAudioInfo = do 
    pulse <- connectPulse "haskellstatus" 
    dev   <- pulseDefaultSink pulse 
    AudioInfo
        <$> deviceVolumePercent dev 
        <*> deviceMute dev 


showMuted :: Bool -> String
showMuted True = "muted" 
showMuted False = "unmuted"
getAudioStr :: AudioSettings -> IO (Either T.Text T.Text) 
getAudioStr AudioSettings{
        audioFormat=FormatSettings{formatText=format}, 
        audioFormatMuted=FormatSettings{formatText=formatMuted}} = do
    info <- getAudioInfo
    let volStr = show (audioVolume info) ++ "%"
    pure $  if audioMute info then 
                Left . fromString $ fromString (T.unpack formatMuted)
                    ~~ ("volume" ~% volStr) 
                    ~~ ("muted"  ~% showMuted True) 
            else 
                Right . fromString $ fromString (T.unpack format) 
                    ~~ ("volume" ~% volStr) 
                    ~~ ("muted"  ~% showMuted False)

instance Processor AudioSettings where 
    process conf@AudioSettings {
        audioFormat
       ,audioFormatMuted} = do 
        info <- getAudioStr conf 
        pure $ case info of 
            Left x -> processFilledFormat x audioFormatMuted
            Right x -> processFilledFormat x audioFormat
