{-# LANGUAGE OverloadedStrings #-}
module Scotty where

import Web.Scotty

import Web.Scotty.Internal.Types (ActionT (..))
import Data.Monoid (mconcat)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy hiding (get)
import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import Data.Maybe (fromMaybe) 
import Control.Monad.Trans.Maybe

import Control.Monad.Trans.Except 
import qualified Data.Text.Lazy as TL

--import Web.Scotty.Trans

-- get :: RoutePattern 
--    -> ActionM ()
--    -> ScottyM ()

--import Web.Scotty.Trans
--instance MonadTrans (ActionT e) where
  --  lift = ActionT . lift . lift . lif


-- main = scotty 3000 $ do
  --  get "/:word" $ do
   --     beam <- param "word"

-- lift :: (Monad m) => m a -> t m a
-- lift :: (Monad m, MonadTrans t) => m a -> t m a
-- lift :: (MonadTrans t) => IO a -> t IO a
-- lift :: IO a -> ActionM a 
-- lift :: IO () -> ActionM ()
        -- let hello = putStrLn "hello"
        -- (lift :: IO a -> ActionM a) hello

        -- html $ mconcat [ "<h1>Scotty, "
        --                , beam
        --                , " me up!</h1>"
        --                ]
        
-- main = scotty 3000 $ do
--     get "/:word" $ do
--         beam <- param "word" 

--         (ActionT 
--             . (ExceptT . liftM Right) 
--             . lift . lift) (putStrLn "hello")

--         html $ mconcat [ "<h1>Scotty, "
--                        , beam
--                        , " me up! </h1>"
--                        ]

--instance MonadTrans (ReaderT r) where 
 --   lift = liftReaderT

-- liftReaderT :: m a -> ReaderT r m a 
-- liftReaderT m = ReaderT (const m)


-- main = scotty 3000 $ do
--     get "/:word" $ do
--         beam <- param "word" 

--         (ActionT 
--             . (ExceptT . fmap Right) 
--             . liftReaderT 
--             . lift) (putStrLn "hello")

--         html $ mconcat [ "<h1>Scotty, "
--                        , beam
--                        , " me up! </h1>"
--                        ]

-- instance MonadTrans (StateT s) where
--     lift m = StateT $ \s -> do
--         a <- m
--         return (a,s)

-- main = scotty 3000 $ do
    -- get "/:word" $ do
    --     beam <- param "word"
    
    --     (ActionT 
    --         . (ExceptT . fmap Right)
    --         . ReaderT . const  
    --         . \m -> StateT (\s -> do
    --                         a <- m
    --                         return (a, s)) 
    --         ) (putStrLn "hello")

    --     html $ 
    --         mconcat [
    --             "<h1>Scotty, ",
    --             beam,
    --             " me up!</h1>"
    --         ]

-- main = scotty 3000 $ do
--     get "/:word" $ do
--         beam <- param "word"
--         liftIO (putStrLn "hello")

--         html $ 
--             mconcat [ "<h1>Scotty,"
--                     , beam
--                     , " me up!</h1>"
--                     ]

---------------------------------------------------
-- param'' :: Parsable a => Text -> ActionM (Maybe a)
-- param'' k = rescue (Just <$> param k) 
--                  (const (return Nothing))

-- param' :: Parsable a => Text -> MaybeT ActionM a
-- param' k = MaybeT $ 
--               rescue (Just <$> param k) 
--                      (const (return Nothing))

-- type Reco =
--     (Integer, Integer, Integer, Integer)

-- main = scotty 3000 $ do 
--   get "/:word" $ do
--     beam' <- param'' "word" 

--     let beam = fromMaybe "" beam'
--     reco <- runMaybeT $ do
--       a <- param' "1"
--       liftIO $ print a
    
--       b <- param' "2"
--       c <- param' "3"
--       d <- param' "4"
--       (lift . lift) $ print b
--       return ((a, b, c, d):: Reco)

--     liftIO $ print reco 
--     html $ mconcat ["<h1>Scotty, "
--             , beam
--             , " me up!</h1>"
--             ]

----------------------------------------------------------
-- param' :: Parsable a => Text -> ActionM (Either String a)
-- param' k = rescue (Right <$> param k) 
--                   (const (return (Left $ "The key: " ++ show k ++ " was missing!")))

-- main = scotty 3000 $ do 
--   get "/:word" $ do
--     beam <- param "word"
--     a <- param' "1"
--     let a' = either (const 0) id a
--     liftIO $ print (a :: Either String Int)
--     liftIO $ print (a' :: Int)
--     html $ mconcat ["<h1>Scotty, "
--                    , beam
--                    , " me up!</h1>"
--                    ]

param' :: Parsable a => Text -> ExceptT String ActionM a
param' k = ExceptT $ rescue (Right <$> param k)
                     (const (return (Left $ "The key: " ++ show k ++ " was missing!")))
                     
type Reco = (Integer, Integer, Integer, Integer)
tshow = TL.pack . show

main = scotty 3000 $ do 
  get "/" $ do
    reco <- runExceptT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b 
      return ((a, b, c, d) :: Reco)
    case reco of
      (Left e) -> text (TL.pack e) 
      (Right r) ->
        html $ mconcat ["<h1>Success! Reco was: "
                       , tshow r
                       , "</h1>"
                       ]