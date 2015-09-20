module Free  where

data Free f a
    = Pure a
    | Free (f (Free f a))

freeMaybe :: Free Maybe Int
freeMaybe =
    Free (Just (Pure 1))

freeMaybeF :: Free Maybe (Int -> Int)
freeMaybeF =
    Free (Just (Pure (+1)))

instance Functor f => Functor (Free f) where
    fmap f (Pure a) =
        Pure (f a)
    fmap f (Free fa) =
        Free (fmap (fmap f) fa)


instance Functor f => Applicative (Free f) where
    pure =
        Pure
    Pure f <*> fa =
        f <$> fa
    Free fs <*> fa =
        Free (fmap (<*> fa) fs)

instance Functor f => Monad (Free f) where
    return =
        Pure
    Pure a >>= f =
        f a
    Free fa >>= f =
        Free (fmap (f =<<) fa)

data Toy b n
    = Output b n
    | Bell n
    | Done

instance Functor (Toy b) where
    fmap f (Output x n) = Output x (f n)
    fmap f (Bell n) = Bell (f n)
    fmap _ Done = Done

data IncompleteException = IncompleteException

subroutine :: Free (Toy Char) IncompleteException
subroutine = Free (Output 'a' (Pure IncompleteException))

program :: Free (Toy Char) a
program = subroutine >>= return (Free (Bell (Free Done)))

liftF :: Functor f => f a -> Free f a 
liftF command = Free (fmap Pure command)

bell = liftF $ Bell ()
output x = liftF $ Output x ()
done = liftF Done

subroutine' :: Free (Toy Char) a
subroutine' = do
    bell
    output 'a'
    done

interpret :: (Show b) => Free (Toy b) r -> IO ()
interpret (Free (Output a n)) =
    print a >> interpret n
interpret (Free (Bell n)) =
    print "<DING>" >> interpret n
interpret (Free Done) =
    return ()

toJavaScript :: (Show b, Show r) => Free (Toy b) r -> String
toJavaScript (Free Done) =
    ""
toJavaScript (Free (Output a n)) =
    "console.log(" ++ show a ++ "); " ++ toJavaScript n
toJavaScript (Free (Bell n)) =
    "alert(\"RING\"); " ++ toJavaScript n
