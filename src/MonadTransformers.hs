
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module MonadTransformers where
import ComposingType

-- 26.2 MaybeT

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a)}

-- instance (Functor f, Functor g) => Functor  (Compose' f g) where
  --  fmap f (Compose' fga) =
   --     Compose $ (fmap . fmap) f fga 

--instance (Applicative f, Applicative g) => (Applicative f g) where
 --   pure x = Compose (pure (pure x))

 --   Compose f <*> Compose x =
  --      Compose ((<*>) <$> f <*> x)

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
        MaybeT $ (fmap . fmap) f ma 

instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))

    (MaybeT fab) <*> (MaybeT mma) =
        MaybeT $ (<*>) <$> fab <*> mma 

-- (<*>) :: (MaybeT m) (a -> b) -> (MaybeT m) a -> (MaybeT m) b

-- (<*>) :: f (a -> b) -> f a -> f b
-- fab :: m (Maybe (a -> b))
-- (<*>) <$> fab :: m (Maybe a -> Maybe b) 
-- mma :: m (Maybe a)

-- (<*>) <$> g (a -> b) :: (g a -> g b)

innerMost :: [Maybe (Identity (a -> b))] 
             -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second' :: [ Maybe (Identity a -> Identity b) ]
           -> [ Maybe (Identity a) ->  Maybe (Identity b) ]
second' = fmap (<*>)

final' :: [ Maybe (Identity a) -> Maybe (Identity b) ]
            -> [Maybe (Identity a)]
            -> [Maybe (Identity b)]
final' = (<*>)

lmiApply :: [] (Maybe (Identity (a -> b))) 
            -> [] (Maybe (Identity a))
            -> [] (Maybe (Identity b))
lmiApply f x =
    let -- a :: [Maybe (Identity a -> Identity b)] 
        a = (fmap . fmap) (<*>) f
        -- b :: [Maybe (Identity a) -> Maybe (Identity b)] 
        b = fmap (<*>) a
    in  b <*> x
    --final' (second' (inerMost f)) x

--  a = (fmap . fmap) (<*>) f :: [] (Maybe (Identity a -> Identity b))
--  b = ((<*>) <$> a) :: [] ((Maybe (Identity a)) -> [] ((Maybe Identity b))) 
--  b <*> x  

instance (Monad m) => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a 
            -> (a -> MaybeT m b)
            -> MaybeT m b
    (MaybeT ma) >>= f = 
        MaybeT $ do
            -- ma :: m (Maybe a)
            -- v :: Maybe a
            v <- ma
            case v of
                Nothing -> return Nothing 
                Just y -> runMaybeT (f y) 
    -- y :: a
    -- f :: a -> MaybeT m b
    -- f y :: MaybeT m b
    -- runMaybeT (f y) :: m (Maybe b)

x = MaybeT [Just 1] >>= (\x -> MaybeT (pure (pure (x+1))))

newtype EitherT e m a = 
    EitherT { runEitherT :: m (Either e a) }

-- 1. Write the Functor instance for EitherT

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT r) = EitherT $ (fmap . fmap) f r 

-- 2. Write the Applicative instance for EitherT:

-- pure :: a -> (EitherT e m) a
instance Applicative m => Applicative (EitherT e m) where
     pure a = EitherT $ pure (Right a)  
     --                       pure

     -- f :: m (Either e (a -> b))
     -- f :: m ((Either e) a -> (Either e) b)
     -- a :: m (Either e a)
     (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
    return = pure

    -- v :: EitherT e m a
    -- f :: (a -> EitherT e m a)
    (EitherT v) >>= f = EitherT $ do
                x <- v 
                case x of
                    Right a -> runEitherT (f a) 
                    Left e -> pure (Left e) 
                         
swapEither :: Either e a -> Either a e
swapEither (Right r) = Left r 
swapEither (Left r) = Right r 

swapEitherT :: (Functor m) => EitherT e m a 
                           -> EitherT a m e
swapEitherT (EitherT r) = 
    -- r :: m (Either e a)
    EitherT $ swapEither <$> r

eitherT :: Monad m =>
            (a -> m c)
         -> (b -> m c)
         -> EitherT a m b
         -> m c
eitherT f g (EitherT r) = 
        -- r :: m (Either a b)
        r >>= \case
              Right b ->  g b
              Left a -> f a 

newtype ReaderT r m a = 
    ReaderT { runReaderT :: r -> m a }

instance (Functor  m) => Functor (ReaderT r m) where
                        fmap f (ReaderT rma) = 
                            ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
            pure a = ReaderT $ pure (pure a)

            (ReaderT f) <*> ReaderT rma =
                -- f :: r -> m a :: (r ->) (a -> m)
                -- (<*>) :: f (a -> b) -> f a -> f b
                -- (<*>) <$> f ::  ((r ->) (a -> m)) -> ((r ->) (m -> b))
                -- rma :: (r ->) (a -> m)
                ReaderT $ ((<*>) <$> f) <*> rma

instance (Monad m) => Monad (ReaderT r m) where
        return = pure

        (ReaderT rma) >>= f =
             ReaderT $ \r -> do
                 -- rma :: r -> m a
                 -- a :: a
                 a <- rma r
                 -- f :: a -> ReaderT r m b
                 runReaderT (f a) r

newtype StateT s m a = 
    StateT { runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
    --  fmap :: (a -> b) -> f a -> f b
    --  fmap :: (a -> b) -> (StateT s m) a -> (StateT s m) b 
    --  s :: (s ->) m (a, s)
        fmap f (StateT smas) =
             StateT (\s -> 
                        -- mas :: m (a, s)
                        let mas = smas s 
                        in fmap (\(a,s') -> (f a, s')) mas
                    )

instance (Monad m) => Applicative (StateT s m ) where
       pure a = StateT $ \s -> pure (a,s) 

       (StateT f) <*> (StateT smas) = 
           StateT $ (\s ->
              let mas = smas s
                  ff = f s
                  fff = (<*>) <$> ff 
              in fff <*> mas 
           )


