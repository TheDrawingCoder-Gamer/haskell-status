{-# LANGUAGE FlexibleInstances #-}
module Data.Stringly where 

import Data.String 
import Data.Text (Text)
import Data.Text qualified as T 
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Text.Lazy qualified as LT
class IsString s => Stringly s where 
    toString :: s -> String 
    fromText :: Text -> s 
    fromText = fromString . toString
    fromLazyText :: Lazy.Text -> s 
    fromLazyText = fromString . toString

instance Stringly String where 
    toString = id
    fromText = toString 
    fromLazyText = toString

instance Stringly Text where 
    toString = T.unpack
    fromText = id 
    fromLazyText = LT.toStrict

instance Stringly Lazy.Text where 
    toString = LT.unpack 
    fromText = LT.fromStrict 
    fromLazyText = id 


