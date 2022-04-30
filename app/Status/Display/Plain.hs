module Status.Display.Plain where 

import Status.Config
import Data.Text qualified as T
setupDisplay :: IO () 
setupDisplay = pure ()

displayBlock Block{blockFullText=text} = 
    text 
displayBlocks blocks = 
    T.intercalate " | " (map displayBlock blocks)
