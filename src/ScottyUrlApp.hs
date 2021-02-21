{-# LANGUAGE OverloadedStrings #-}
module ScottyUrlApp where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef ( writeIORef, readIORef, newIORef, IORef )
import qualified Data.Map as M

import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
    Config { counts :: IORef (M.Map Text Integer)
           , prefix :: Text
           }

type Scotty =
    ScottyT Text (ReaderT Config IO)
type Handler =
    ActionT Text (ReaderT Config IO)

bumpBoomp :: Text 
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = let i = M.findWithDefault 0 k m  
                in (M.insert k (i+1) m, i+1) 

app :: Scotty ()
app = 
    get "/:key" $ do
        unprefixed <- param "key" 
        config <- lift ask  
        let key' = mappend (prefix config) unprefixed
        (newMap, newInteger) <- liftIO $ bumpBoomp key' <$> readIORef (counts config)   
        liftIO $ writeIORef (counts config) newMap
        html $ mconcat [ "<h1>Suceess! Count was: "
                       , TL.pack $ show newInteger
                       , "</h1>"
                       ]

main :: IO ()
main = do
    counter <- newIORef M.empty
    let config = Config counter "pefix"
        runR r = runReaderT r config 
    scottyT 3000 runR app

    