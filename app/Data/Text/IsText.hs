module Data.Text.IsText where 

import Data.Text (unpack, Text)
import Data.String (fromString, IsString)
fromText :: IsString a => Text -> a
fromText = fromString . unpack
