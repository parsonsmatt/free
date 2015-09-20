module FreeT where

data FreeF f a x 
    = Pure a
    | Free (f x)


freefmap :: (Monad m, Functor f)
         => (a -> b) 
         -> FreeF f a (FreeT f m a) 
         -> FreeF f b (FreeT f m b)
freefmap f (Pure a) =
    Pure (f a)
freefmap f (Free fx) =
    Free (fmap (fmap f) fx)

newtype FreeT f m a
    = FreeT
    { runFreeT :: m (FreeF f a (FreeT f m a))
    }

instance (Functor f, Monad m) => Functor (FreeT f m) where
    fmap f = 
        FreeT . fmap (freefmap f) . runFreeT

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure =
        FreeT . return . Pure

    FreeT mf <*> fma =
        FreeT $ do
            ff <- mf
            fa <- runFreeT fma
            return $ case ff of
                          Pure f ->
                              freefmap f fa
                          Free fx ->
                              Free (fmap (<*> fma) fx)
                              

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return =
        pure

    FreeT mfa >>= f =
        FreeT $ do
            fa <- mfa
            runFreeT $ case fa of
                            Pure a  -> f a
                            Free fx -> FreeT . return . Free $ fmap (>>= f) fx
