module Status.Display.Swaybar (setupDisplay, displayBlock, displayBlocks) where 

import Status.Config
import Data.Text qualified as T 
setupDisplay = do 
    putStrLn "{ \"version\": 1 }"
    putStrLn "[" 

displayBlock :: Block -> T.Text 
displayBlock Block{blockColor=color, blockFullText=text, blockMarkup=markup} = 

    "{ \"full_text\": \"" <> text <> fancyColor <> fancyColor <> "\"}"
    where 
        fancyColor = 
            case color of 
                Just x -> 
                    "," <> "\"color\":\"" <> showColor x <> "ff\""
                Nothing -> 
                    ""
        fancyMarkup = 
            if markup then 
                ",\"markup\": \"pango\""
            else 
                ""

displayBlocks :: [Block] -> T.Text 
displayBlocks blocks = 
    let 
        renderedBlocks = map displayBlock blocks 
    in 
        "[" <> T.intercalate "," renderedBlocks <> "],"
