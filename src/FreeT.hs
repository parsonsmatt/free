module FreeT where

data FreeF f a x 
    = Pure a
    | Free (f x)


newtype FreeT f m a
    = FreeT
    { runFreeT :: m (FreeF f a (FreeT f m a))
    }

instance (Functor f, Monad m) => Functor (FreeT f m) where
    fmap f (FreeT m) =
        FreeT $ do
            fa <- m
            return $ case fa of
                          Pure a ->
                              Pure (f a)
                          Free fx ->
                              Free (fmap (fmap f) fx)

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure =
        FreeT . return . Pure

    FreeT mf <*> FreeT ma =
        FreeT $ do
            ff <- mf
            fa <- ma
            return $ case ff of
                          Pure f ->
                              case fa of
                                   Pure a ->
                                       Pure (f a)
                                   Free fx ->
                                       Free $ fmap (fmap f) fx
                          Free fx ->
                              Free (fmap (<*> (FreeT ma)) fx)
                              

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return =
        pure

    FreeT mfa >>= f =
        FreeT $ do
            fa <- mfa
            runFreeT $ case fa of
                            Pure a  -> f a
                            Free fx -> FreeT . return . Free $ fmap (>>= f) fx
