                  
--      ReaderMaybe (\x -> (\(ReaderMaybe rdr) -> rdr x) >>= f(reader x))


  -- rPoint a = Some a
  -- rBind (Some m) k = k m
  -- rBind Null _     = Null
  --bind Null _ = Null
  --bind m k = rBind m (\ma -> ma >>= k )
  --rJoin Null = Null
  --rJoin (Some x) = x >>= \r -> case r of
  --  Some y -> a




{-

data  Mybe a  =  Null | Some a
  deriving (Eq, Ord, Show)

instance Functor Mybe where
  fmap f (Some x) = Some $ f x
  fmap f Null     = Null

instance Monad Mybe where
  return a = Some a
  
  (Some x) >>= f = f x
  Null     >>= _ = Null
  
newtype Reader r a = Reader {
  runReader :: r -> a
}

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

instance Functor (Reader r) where
  fmap f r = Reader $ \x -> f (runReader r x)
  

instance Monad (Reader r) where
  return a = Reader $ \_ -> a
  m >>= f = Reader $ \r -> runReader (f (runReader m r)) r

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Monad m) => Functor (ReaderT r m) where
  fmap f m = ReaderT $ \r -> (runReaderT m r) >>= return . f

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_  -> return a
    m >>= k  = ReaderT $ \r -> (runReaderT m r) >>= \a -> runReaderT (k a) r

-}

-----


{-    


instance RelMonad (Reader r) where
  rPoint a = Reader $ \_  -> a
  rBind m k = Reader $ \r -> runReader (k (runReader m r)) r
  --rJoin m = error "a"
  

--newtype RelMon r m a = r (m a)
{-instance (RelMonad r, Monad m) => Monad (RelMon r m) where
  return a = rPoint . return $ a
  m >>= k  = error "pending"
-}

nReturn :: Monad m => RelMonad r => a -> r (m a)
nReturn = rPoint . return
--nReturn 3 :: Mybe (Reader r Int)

--nBind :: Monad m => RelMonad r => r (m a) -> (a -> r (m b)) -> r (m b)
--nBind r k = rBind r (\x -> )

newtype ReaderR r m a = ReaderR { runReaderR :: r -> m a }

class RelMonad2 r where
  rPoint2 :: (Monad m) => m a -> r a
  rBind2  :: (Monad m) => r a -> (m a -> m (r b)) -> r b
-}

--instance (Monad m) =>RelMonad2 (ReaderR r m) where
--  rPoint2 a = ReaderR $ \_ -> a
--readerRPoint2 :: (Monad m) => m a -> ReaderR r m a
--readerRPoint2 a = ReaderR $ \_ -> a
--readerRBind2 :: (Monad m) => ReaderR r m a -> (m a -> m (ReaderR r m b)) -> ReaderR r m b
--readerRBind2 m k = ReaderR $ \r -> runReaderR (k (runReaderR m r)) r >>= id

--nReturn2 :: Monad m => RelMonad2 r => a -> r a
--nReturn2 = rPoint2 . return

--nBind2 :: Monad m => RelMonad2 r => r a -> (a -> r b) -> r b
--nBind2 m k = rBind m (\a -> a >>= (return . k)


--point
--rPoint3 :: Monad m => Monad r => m a -> r (m a)
--bind
--rBind3  :: Monad m => Monad r => r (m a) -> (m a -> r (m b)) -> r (m b)

--squash :: Monad m => Monad r => m (r (m a)) -> r (m a)

{-squashO :: Monad r => Maybe (r (Maybe a)) -> r (Maybe a)
squashO (Just x) = x
squashO Nothing  = return Nothing
--lPoint :: Monad m => Monad r => a -> r (m a)
--lBind  :: Monad m => Monad r => r (m a) -> (a -> r (m b)) -> r (m b)
lBind :: Monad r => r (Maybe a) -> (a -> r (Maybe b)) -> r (Maybe b)
lBind m k = m >>= (\ma -> squashO (ma >>= return . k))

class Squashable m where
  squash :: Monad r => m (r (m a)) -> r (m a)

instance Squashable Maybe where
  squash (Just x) = x
  squash Nothing  = return Nothing

--instance Squashable (Reader r) where
--  squash m = Reader $ \r -> (runReader m r >>= \a -> ru
-}



