module Status.Helpers (roundTo, fromLastJust, clamp, decodeFileSafe) where
 
import Data.Monoid (Last(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromJust)
import Data.Text.IO qualified as TIO 
import Data.Text qualified as T
import Toml qualified 
import Control.Monad.Trans.Except qualified as EX
import System.Directory qualified as D 
import System.IO (FilePath)
import Data.Functor ((<&>))
roundTo :: (Integral i, RealFrac f) => i -> f -> f
roundTo p n = fromInteger (round (n * (10 ^ p))) / 10 ^ p

fromLastJust :: Last a -> a
fromLastJust = fromJust . getLast

clamp :: (Ord o) => (o, o) -> o -> o 
clamp (mi, ma) n = max mi (min ma n)

decodeFileSafe codec file = 
    fmap (fmap (Toml.decode codec . T.pack))  (readFileSafe file)
readFileSafe :: (MonadIO m) => FilePath -> m (Maybe String) 
readFileSafe file = do 
    exists <- liftIO $ D.doesFileExist file 
    if exists then 
        liftIO $ readFile file <&> Just
    else 
        pure Nothing 
