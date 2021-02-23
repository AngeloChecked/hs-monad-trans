
module Exercise where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity 
import Control.Monad.IO.Class


rDec :: Num a => Reader a a
rDec = reader $ \n -> n-1 

rShow :: Show a => ReaderT a Identity String
--rShow = ReaderT $ pure . show 
rShow = show <$> ask 



rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \s -> do  
                       print $ "Hi: " ++ show s
                       return (s+1)

 -- newtype StateT s m a = 
    --     StateT { runStateT :: s -> m (a, s)}
sPrintIncAccum :: (Num a,Show a) => StateT a IO String 
sPrintIncAccum = StateT $ \a -> do 
                            putStrLn $ "Hi: " ++ show a
                            return (show a, a+1) 

