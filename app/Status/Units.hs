{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Status.Units where 

import Text.Read (readMaybe)
import Data.Text qualified as T
import Toml qualified
import Status.Helpers
data WithAuto = 
    Specifically ByteUnit 
    | Automatic deriving (Show)
data ByteUnit 
    = B
    | KiB
    | MiB 
    | GiB 
    | TiB deriving (Enum, Bounded, Show, Ord, Eq, Read)
data Bytes = Bytes Float ByteUnit
convertBytes :: Bytes -> WithAuto -> Bytes
convertBytes (Bytes n from) (Specifically to) 
    | from == to = Bytes n to 
    | from < to = convertBytes (Bytes (n / 1024) (succ from)) (Specifically to)
    | from > to = convertBytes (Bytes (n * 1024) (pred from)) (Specifically to)
    | otherwise = undefined -- complete pattern match
 
convertBytes (Bytes n from) Automatic
    | n > 1024 = convertBytes (Bytes (n / 1024) (succ from)) Automatic
    | otherwise = Bytes n from

showBytes p (Bytes n k)  = 
    show (roundTo p n) ++ " " ++ show k

readBytePref :: T.Text -> Either T.Text WithAuto
readBytePref "auto" = 
    Right Automatic 
readBytePref s = 
    case readMaybe (T.unpack s) of 
        Just v -> Right $ Specifically v 
        Nothing -> Left $ T.append "Failed Parse on " s 

showBytePref :: WithAuto -> T.Text
showBytePref Automatic = "auto" 
showBytePref (Specifically s) = T.pack $ show s

bytePrefCodec = Toml.textBy showBytePref readBytePref
