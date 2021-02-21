module LiftMore where

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
import Control.Monad.Trans.Class
import Control.Monad
import MonadTransformers 
import Control.Monad.IO.Class

instance MonadTrans (EitherT e) where
    -- lift :: Monad m => m a -> t m a
    lift m = EitherT (Right <$> m) 

    -- newtype StateT s m a = 
    --     StateT { runStateT :: s -> m (a, s)}
instance MonadTrans (StateT s) where
    -- lift :: Monad m => m a -> t m a
    lift m = StateT $ \s -> do
                       a <- m 
                       return (a, s) 

liftReaderT :: m a -> ReaderT r m a 
liftReaderT m = ReaderT (const m)

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (ReaderT r) where 
  lift = liftReaderT

instance (MonadIO m) => MonadIO (MaybeT m) where
    --   liftIO :: IO a -> m a
  liftIO = lift . liftIO 

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO 

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO



newtype MySum = MySum Int

class MyMonoid a where
    neutro :: a
    schiaccia :: a -> a -> a

instance MyMonoid MySum where
    neutro = MySum 0
    schiaccia (MySum a) (MySum b) = MySum (a + b)