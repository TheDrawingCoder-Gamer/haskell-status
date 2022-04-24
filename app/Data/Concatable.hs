{-# LANGUAGE DefaultSignatures #-}
module Data.Concatable where 

import Data.Text qualified as T 
import Data.Text.Lazy qualified as TL
import Data.ByteString.Char8 qualified as BS8 
import Data.ByteString.Lazy.Char8 qualified as BSL8 
class Concatable a where 
    (<+>) :: a -> a -> a 
    default (<+>) :: Semigroup a => a -> a -> a 
    (<+>) = (<>)

instance Concatable [a] 
instance Concatable T.Text 
instance Concatable TL.Text
instance Concatable BS8.ByteString 
instance Concatable BSL8.ByteString


 
