--   Copyright 2014 Commonwealth Bank of Australia

 {-# LANGUAGE 
      FlexibleInstances,
      MultiParamTypeClasses,
      UndecidableInstances,
      FunctionalDependencies,
      --AllowAmbiguousTypes,
      FlexibleContexts,
      OverlappingInstances,
      TypeFamilies, KindSignatures #-}

    
module RelMonad where

-----------------------------------------------
-- RelMonad via Rel r = the monad r is relative to 
type family Rel (r :: * -> *) :: * -> *
        
class RelMonad (r :: * -> *) where
  retRel :: (Rel r) a -> r a
  (>><=) :: r a -> ((Rel r) a -> (Rel r) (r b)) -> r b        
    
------------------------------------------------
-- Derive Monad from RelMonad
instance (RelMonad r, Monad (Rel r)) => Monad r where
  ra >>= f  =  ra >><= (\ma -> ma >>= (\a -> return(f a)))
  return x  =  retRel (return x)

-----------------------------------------------             
-- Implement functions particular to (Rel r)=Maybe

andThen :: (RelMonad r, Rel r ~ Maybe) => r a -> (a -> Maybe b) -> r b
andThen ra f = ra >><= (\ma -> return (retRel (ma >>= f)))
-- In general, here's a bunch more functions e.g., rMap, or, ensures...
-- ...

--------------------------------------------------
-- Implement retRel and >><= for a particular r, instantiating andThen, etc.

type Conf = [(String, String)]
data ReadMay a = ReadMay (Conf -> Maybe a)

type instance Rel ReadMay = Maybe               
instance RelMonad ReadMay where
  retRel x =
    ReadMay (const x)
  (ReadMay reader) >><= f =
    ReadMay (\conf -> case f(reader conf) of
                            Just (ReadMay res) -> (res conf)
                            Nothing -> Nothing)
------------------------------------------------

-- Monad transformer version
--import Control.Monad.Trans.Reader

-- type ReaderTMaybe = ReaderT Int Maybe 

-- type instance Rel ReaderTMaybe = Maybe    
-- instance RelMonad ReaderTMaybe where
--   retRel x = ReaderT $ \_ -> x
--   reader >><= f = ReaderT (\r -> case f (runReaderT reader r) of
--                                   Just inner -> runReaderT inner r
--                                   Nothing    -> Nothing
--                           )
-----------------------------------------------

-- Specific to (Rel r)=Execution

-- ??

---------

-- data MyBe = MyBe Maybe 
-- type instance Rel MyBe = Execution
-- instance RelMonad MyBe where
--     retRel 
