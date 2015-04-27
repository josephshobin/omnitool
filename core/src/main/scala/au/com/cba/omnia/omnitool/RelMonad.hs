--   Copyright 2015 Commonwealth Bank of Australia

 {-# LANGUAGE 
      FlexibleInstances,
      MultiParamTypeClasses,
      UndecidableInstances,
      -- FunctionalDependencies,
      -- AllowAmbiguousTypes,
      FlexibleContexts,
      OverlappingInstances,
      TypeFamilies, 
      -- KindSignatures, 
      ScopedTypeVariables #-}

    
module RelMonad where

-----------------------------------------------

class (Monad m) => RelMonad0 m r where
    retRel :: m a -> r a
    (>><=) :: r a -> (m a -> r b) -> r b
                 
class (RelMonad0 m r, Monad r) => RelMonad m r

-----------------------------------------------    
              
type family RBase (r :: * -> *) :: * -> *  -- r derives a monad instance from RBase
              
class Squash r where
    squash :: (RBase r) (r a) -> r a
                 
instance (RelMonad0 (RBase r) r, Squash r) => Monad r where
    ra >>= f  =  ra >><= (\ma -> squash (ma >>= (\a -> return (f a))))
    return    =  retRel . (return :: a -> RBase r a)
    

-----------------------------------------------             
-- Implement functions particular to  'RelMonad Maybe _'

-- Compose with a possibly failing operation f 
andThen :: (RelMonad Maybe r) => r a -> (a -> Maybe b) -> r b
andThen ra f = ra >><= (\ma -> (retRel (ma >>= f)))
               
-- In general, here's a bunch more functions e.g., rMap, or, ensures...
-- ...

--------------------------------------------------
-- Implement retRel and >><= for a particular r, instantiating andThen, etc.

type Conf = [(String, String)]
data ReadMay a = ReadMay (Conf -> Maybe a)

type instance RBase ReadMay = Maybe    

instance RelMonad0 Maybe ReadMay where
    retRel x                = ReadMay (const x)
    (ReadMay reader) >><= f = ReadMay (\conf -> case f(reader conf) of
                                                  (ReadMay res) -> res conf)

instance Squash ReadMay where
    squash Nothing = ReadMay (\_ -> Nothing)
    squash (Just rdMay) = rdMay
    
instance RelMonad Maybe ReadMay                                                                         

-----------------------------------------------             
-- Implement functions particular to 'RelMonad [] _'

-- Concatenate
(++<) :: (RelMonad [] r) => r a -> r a -> r a
rxs ++< rys =
    rxs >><= (\xs -> (rys >><= (\ys -> (retRel (xs ++ ys)))))
               
-- In general, here's a bunch more functions e.g., rmap, rfold, empty, ... 
-- ...


--------------------------------------------------
-- Implement retRel and >><= for a particular r, instantiating the above.

data ReadChoice a = ReadChoice (Conf -> [a])
instance Show a => Show (ReadChoice a) where
    show (ReadChoice reader) = (reader []) >>= show

type instance RBase ReadChoice = []
                               
instance RelMonad0 [] ReadChoice where
    retRel x                   = ReadChoice (const x)
    (ReadChoice reader) >><= f = ReadChoice (\conf -> case f(reader conf) of
                                                      (ReadChoice res) -> res conf)
instance Squash ReadChoice where
    squash xs = ReadChoice (\conf -> concat (map (\(ReadChoice x) -> x conf) xs))
                   
instance RelMonad [] ReadChoice                                                                         

------------------------------------------------

-- Monad transformer version
--import Control.Monad.Trans.Reader

-- type ReaderTMaybe = ReaderT Int Maybe 

-- type instance RBase ReaderTMaybe = Maybe    
-- instance RelMonad ReaderTMaybe where
--   retRel x = ReaderT $ \_ -> x
--   reader >><= f = ReaderT (\r -> case f (runReaderT reader r) of
--                                   Just inner -> runReaderT inner r
--                                   Nothing    -> Nothing
--                           )
-----------------------------------------------

-- Specific to (RBase r)=Execution

-- ??

---------

-- data MyBe = MyBe Maybe 
-- type instance RBase MyBe = Execution
-- instance RelMonad MyBe where
--     retRel 
