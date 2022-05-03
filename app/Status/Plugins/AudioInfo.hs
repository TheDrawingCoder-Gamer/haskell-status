module Status.Plugins.AudioInfo where 

import System.Process
import Data.List.Extra (trim) 
import Status.Config
import Text.Format ((~~), (~%))
import Data.String
import Data.Text qualified as T
import Control.Lens.Tuple
import Control.Lens (set, view)
import System.Exit
import Status.Display
data AudioInfo = AudioInfo 
    { audioVolume :: Int
    , audioMute   :: Bool } 
getAudioInfo :: IO AudioInfo 
getAudioInfo = do 
    vol <- read @Int . trim . view _2 <$> readProcessWithExitCode "pamixer" ["--get-volume"] []
    muted <-  (==ExitSuccess) . view _1 <$> readProcessWithExitCode "pamixer" ["--get-mute"] []
    pure $ AudioInfo vol muted 

parseExoticBool "true" = True 
parseExoticBool "false" = False 
parseExoticBool _       = error "parseExoticBool: no parse"

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
