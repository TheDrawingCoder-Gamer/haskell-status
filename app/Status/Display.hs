module Status.Display where 
import Status.Config 
import Data.Text qualified as T
import Data.Functor.Identity 
import Status.Display.Swaybar qualified as Swaybar
import Status.Display.Plain qualified as Plain
import Data.Maybe (fromMaybe, mapMaybe)
import Data.HashMap.Strict qualified as HM
import Data.Text.IO qualified as T (putStrLn)
import Control.Applicative ((<|>))
import Data.Monoid (Any(..))
class Processor a where 
    process :: a -> IO Block

processFilledFormat :: T.Text -> FormatSettings' Identity -> Block
processFilledFormat str FormatSettings{formatColor=color, formatMarkup=markup} = 
    Block str color markup

maybeBlock :: Maybe Block -> Block 
maybeBlock = fromMaybe defaultBlock
displaySysinfo :: Settings -> SystemInfo -> IO () 
displaySysinfo Settings{settingsBlocks, settingsMode} sysinfo = 
    let 
        blocks = getJustLast settingsBlocks 
        betterBlocks =mapMaybe (sysinfo HM.!?) blocks 
        
    in
        T.putStrLn $ displayBlocks settingsMode betterBlocks
        
setupDisplay Swaybar = 
    Swaybar.setupDisplay
setupDisplay Plain = 
    Plain.setupDisplay

displayBlocks Swaybar = Swaybar.displayBlocks 
displayBlocks Plain = Plain.displayBlocks


