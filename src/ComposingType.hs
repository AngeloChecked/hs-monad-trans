{-# LANGUAGE InstanceSigs #-}

module ComposingType where 
import Control.Monad

newtype Compose' f g a = 
    Compose' { getCompose :: f (g a) }
    deriving (Eq, Show)

-- Compose' [] Maybe Int
-- Compose' [Just 1, Nothing]


instance (Functor f, Functor g) => 
            Functor (Compose' f g) where
    fmap f (Compose' fga) = Compose' $ (fmap . fmap) f fga

newtype One f a =
    One (f a)
    deriving (Eq, Show)

instance Functor f =>
            Functor (One f) where
    fmap f (One fa) = One $ fmap f fa 

newtype Three f g h a = 
    Three (f (g (h a)))
    deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) =>
        Functor  (Three f g h) where
            fmap f (Three fgha) =
                Three $ (fmap . fmap . fmap) f fgha

v :: Compose' []
            Maybe 
            (Compose' Maybe [] Integer)
v = Compose' [Just (Compose' $ Just [1])]

instance (Applicative f, Applicative g) =>
        Applicative (Compose' f g) where
    pure :: a -> Compose' f g a
    pure a = Compose' $ pure (pure a)

    (<*>) :: Compose' f g (a->b)
            -> Compose' f g a
            -> Compose' f g b
    (Compose' f) <*> (Compose' a) = 
        Compose' $ (<*>) <$> f <*> a

-- Compose' [] Maybe Int
-- [Just 1, Nothing] 
-- (<*>) <$> [Just 1, Nothing] 
-- (<*>) :: f (a -> b) -> f a -> f b
-- [Maybe a -> Maybe b] <*> [Just 1, Nothing] 

instance (Foldable f, Foldable g) => Foldable (Compose' f g) where 
        foldMap f (Compose' fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose' f g) where
        traverse f (Compose' fga) = Compose' <$> (traverse . traverse) f fga


class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b) 
        -> (c -> d) 
        -> p a c 
        -> p b d
    bimap f g = first f . second g
    
    first :: (a -> b) -> p a c -> p b c 
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
    bimap f g (Deux a c) = Deux (f a) (g c)
    first f = bimap f id 
    second g = bimap id g 

newtype Const a b = Const a

instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c) 

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b) 

newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a 

data Quadriceps a b c d =
    Quadrizzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadrizzz a b c d) = Quadrizzz a b (f c) (g d)

data Either' a b =
    Left' a 
    | Right' b

instance Bifunctor Either' where
    bimap _ g (Right' b) = Right' (g b) 
    bimap f _ (Left' b) = Left' (f b) 

newtype MaybeIO a =
    MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a =
    MaybeList { runMaybeList :: [Maybe a] }

newtype Identity a = 
    Identity { runIdentity :: a }
    deriving (Eq, Show)

newtype IdentityT f a = 
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor Identity where 
    fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = 
        Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)

    (IdentityT fab) <*> (IdentityT fa) =
        IdentityT (fab <*> fa)

-- instance (Monad m) => Monad (IdentityT m) where
  --  return = pure
   -- (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

sumR :: Int -> IdentityT [] Int
sumR = pure . (+1)

f :: IdentityT [] Int
f = IdentityT [1, 2, 3] >>= sumR

instance (Monad m) => Monad (IdentityT m) where
    return = pure
    (>>=) :: IdentityT m a 
            -> (a -> IdentityT m b)
            -> IdentityT m b 
 -- (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f 
    m >>= k = IdentityT $ runIdentityT . k =<< runIdentityT m





    