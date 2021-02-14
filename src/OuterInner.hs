module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- We only need to use return once, 
-- because it's one big monad.
embedded :: MaybeT 
            (ExceptT String 
                (ReaderT () IO))
            Int
embedded = return 1

maybeUnwrap :: ExceptT String 
        (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO 
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwarp :: ()
                -> IO (Either String 
                        (Maybe Int))
readerUnwarp = runReaderT eitherUnwrap

-- Prelude> readerUnwrap ()
-- Right (Just 1)

embedded' :: MaybeT 
            (ExceptT String 
                (ReaderT () IO))
            Int
embedded' = MaybeT . ExceptT . ReaderT $ const (return $ Right (Just 1)) 

