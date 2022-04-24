{-# LANGUAGE FlexibleInstances, UndecidableInstances, KindSignatures, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, DerivingVia, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Status.Format where 

import Data.String
import Text.ParserCombinators.ReadPrec qualified as RP
import Text.ParserCombinators.ReadP
import Text.Read qualified as R
import Data.Kind
import Data.Text qualified as T
import Generic.Data
import Data.Monoid (First(..))
import Status.Helpers
import Data.Stringly 
import Data.Char (chr, ord, isDigit, isAsciiLower, isAsciiUpper)
import GHC.Stack (HasCallStack)
import Data.Text.Lazy qualified as Lazy (Text)
import Numeric qualified
{- 
 - Formatting
 - This format only supports named arguments -}
newtype NoDupes a = NoDupes { unNoDupes :: Maybe a } 
    deriving Generic 
    deriving newtype Monoid

instance Semigroup (NoDupes a) where 
    (NoDupes (Just _))   <> (NoDupes (Just _))   = error "Duplicated field" 
    a@(NoDupes (Just _)) <> _                    = a 
    _                    <> b@(NoDupes (Just _)) = b 
    _                    <> _                    = NoDupes Nothing
data FormatFlags = FormatFlags 
    { flagBase      :: First Int 
    , flagPrecision :: First Int 
    } deriving (Generic)
      deriving (Semigroup, Monoid) via (Generically FormatFlags)
data FormatArg = NamedArg String (FormatFlags -> Formatted)
data FormattedPart = FormattedPart { formattedFlags :: FormatFlags, formattedValue :: String}
newtype Formatted = Formatted { formattedParts :: [FormattedPart] }
instance IsString FormattedPart where 
    fromString = FormattedPart mempty 
instance IsString Formatted where 
    fromString = Formatted . pure . fromString 
class Hole a where 
    hole :: a -> [FormatArg]

class Formattable f where 
    formattable :: f -> FormatFlags -> Formatted 
    default formattable :: Show f => f -> FormatFlags -> Formatted
    formattable x _ = fromString . show $ x
newtype StringlyInstance a = StringlyInstance a deriving newtype (Stringly, IsString)
instance Stringly s => Formattable (StringlyInstance s) where 
    formattable x _ = fromString . toString $ x
 
instance {-# OVERLAPPING #-} Formattable Bool
deriving via (StringlyInstance T.Text)    instance Formattable T.Text
deriving via (StringlyInstance Lazy.Text) instance Formattable Lazy.Text
deriving via (StringlyInstance String   ) instance Formattable String
instance Formattable Int where 
    formattable x (FormatFlags (First (Just b)) _) 
        | b < 2 || b > 36 = fromString $ show x 
        | otherwise = 
            fromString $ Numeric.showIntAtBase (toInteger b) intToDigit' (toInteger x) "" 
    formattable x _ =
        fromString $ show x 
instance Formattable Float where 
    formattable x (FormatFlags _ (First (Just p))) = 
       fromString . show . roundTo p $ x
    formattable x _ = fromString . show $ x
parseFlag :: ReadP FormatFlags
parseFlag =
    do 
        c <- choice [char 'b', char 'p']
        n <- RP.readPrec_to_P R.readPrec 0 :: ReadP Int 
        pure $ case c of 
            'b' -> 
                 FormatFlags (First (Just n)) mempty
            'p' -> 
                 FormatFlags mempty (First (Just n))
            _ -> 
                error "unreachable"
intToDigit' :: HasCallStack => Int -> Char 
intToDigit' i 
    | i >= 0  && i < 10 = chr (ord '0' + i)
    | i >= 10 && i < 36 = chr (ord 'a' + i - 10)
    | otherwise = error ("intToDigit': Invalid int " ++ show i) 

digitToInt' :: HasCallStack => Char -> Int 
digitToInt' c 
    | isDigit c      = ord c - ord '0'
    | isAsciiLower c = ord c - ord 'a' + 10
    | isAsciiUpper c = ord c - ord 'A' + 10
    | otherwise      = error ("digitToInt': Invalid digit " ++ show c)

 
