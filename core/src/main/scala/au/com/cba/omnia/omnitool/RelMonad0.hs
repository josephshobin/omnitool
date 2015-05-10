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

class (Monad m) => RelMonad m r where
    retRel :: m a -> r a
    (>><=) :: r a -> (m a -> r b) -> r b
    joinRR :: r (r a) -> r a
                 
-- Relative monad laws (just like the standard monad laws)
--   retRel x >><= f    =  f x                          -- idL
--   x >><= retRel      =  x                            -- idR
--   (x >><= f) >><= g  =  x >><= (\y => f y >><= g)    -- Assoc

--   ma >>=[m] (\a -> f a >><= g)  =  (ma >>=[m] f) >><= g    when f :: a -> r a

-----------------------------------------------    
              
type family RBase (r :: * -> *) :: * -> *  -- r derives a monad instance from RBase
              
--class JoinRel r where
--    joinRel :: r (r a) -> r a

-- Squash laws  
--     squash (return[m] rx)  =  rx
--     squash (ma >>=[m] (return[m].return[r]))  =  retRel ma  (TODO: Check instances)

--      ???   squash (ma >>=[m] (\a -> return[m] (f a))) >><= (\mb -> squash (mb >>=[m] (return[m].g )))

--            squash (ma >>=[m] (\a -> return[m] (f a))) >><= g
--         =  squash (ma >>=[m] (\a -> return[m] (f a) >><= g))    [f : a -> r a]



--     if f :: a -> m a
--     squash (f (return[r]  >><= (\ma -> 


instance (RelMonad (RBase r) r) => Monad r where
    ra >>= f  =  ra >><= (\ma -> joinRR (retRel (ma >>= return.f)))
    return    =  retRel . (return :: a -> RBase r a)

-- Deriving the monad laws. TODO

--   return(r) x >>= f
--         =  retRel(return[m] x) >><= (\ma -> squash (ma >>=[m] (return[m].f)))
--         =  squash ((return[m] x) >>=[m] (return[m].f))
--         =  squash (return[m](f x))
--         =  f x


--   ra >>=[r] return[r]
--         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].return[r])))
--         =  ra >><= (\ma -> retRel ma)
--         =  ra

--   ra >>= (\a -> f a >>= g)       [where f: a -> r a] 
--         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].(\a -> f a >>=[r] g))))
--         =  ra >><= (\ma -> squash (ma >>=[m] (\a -> return[m] (f a >>=[r] g))))

--         =  ra >><= (\ma -> squash (ma >>=[m] (\a -> return[m] (
--                f a >><= (\mb -> squash (mb >>=[m] (return[m].g )))  
--            ))))

--         Sufficient to show:                          [where MB = (\mb -> squash (mb >>=[m] (return[m].g ))) ]
--             =  squash (ma >>=[m] (\a -> return[m] (f a >><= MB)))

--                via >>=[m] associates with >><=  (???)

--             =  squash (ma >>=[m] (\a -> return[m] (f a))) >><= MB

--         =  ra >><= (
--              \ma -> squash (ma >>=[m] (\a -> return[m] (f a))) >><=
--                (\mb -> squash (mb >>=[m] (return[m].g)))
--            )

--         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].f)) >><=
--                    (\mb -> squash (mb >>=[m] (return[m].g)))))

--         =  (ra >><= (\ma -> squash (ma >>=[m] (return[m].f)))) >><=
--              (\mb -> squash (mb >>=[m] (return[m].g)))

--         =  (ra >><= (\ma -> squash (ma >>=[m] (return[m].f)))) >>=[r] g
--         =  (ra >>= f) >>= g
    
-----------------------------------------------


-- class (Monad m) => RelMonad0 m r where
--     retRel :: m a -> r a
--     (>><=) :: r a -> (m a -> r b) -> r b
                 
-- class (RelMonad0 m r, Monad r) => RelMonad m r

-- -- Relative monad laws (just like the standard monad laws)
-- --   retRel x >><= f    =  f x                          -- idL
-- --   x >><= retRel      =  x                            -- idR
-- --   (x >><= f) >><= g  =  x >><= (\y => f y >><= g)    -- Assoc

-- --   ma >>=[m] (\a -> f a >><= g)  =  (ma >>=[m] f) >><= g    when f :: a -> r a

-- -----------------------------------------------    
              
-- type family RBase (r :: * -> *) :: * -> *  -- r derives a monad instance from RBase
              
-- class Squash r where
--     squash :: (RBase r) (r a) -> r a

-- -- Squash laws  
-- --     squash (return[m] rx)  =  rx
-- --     squash (ma >>=[m] (return[m].return[r]))  =  retRel ma  (TODO: Check instances)

-- --      ???   squash (ma >>=[m] (\a -> return[m] (f a))) >><= (\mb -> squash (mb >>=[m] (return[m].g )))

-- --            squash (ma >>=[m] (\a -> return[m] (f a))) >><= g
-- --         =  squash (ma >>=[m] (\a -> return[m] (f a) >><= g))    [f : a -> r a]



-- --     if f :: a -> m a
-- --     squash (f (return[r]  >><= (\ma -> 


-- instance (RelMonad0 (RBase r) r, Squash r) => Monad r where
--     ra >>= f  =  ra >><= (\ma -> squash (ma >>= return.f))
--     return    =  retRel . (return :: a -> RBase r a)

-- -- Deriving the monad laws.

-- --   return(r) x >>= f
-- --         =  retRel(return[m] x) >><= (\ma -> squash (ma >>=[m] (return[m].f)))
-- --         =  squash ((return[m] x) >>=[m] (return[m].f))
-- --         =  squash (return[m](f x))
-- --         =  f x


-- --   ra >>=[r] return[r]
-- --         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].return[r])))
-- --         =  ra >><= (\ma -> retRel ma)
-- --         =  ra

-- --   ra >>= (\a -> f a >>= g)       [where f: a -> r a] 
-- --         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].(\a -> f a >>=[r] g))))
-- --         =  ra >><= (\ma -> squash (ma >>=[m] (\a -> return[m] (f a >>=[r] g))))

-- --         =  ra >><= (\ma -> squash (ma >>=[m] (\a -> return[m] (
-- --                f a >><= (\mb -> squash (mb >>=[m] (return[m].g )))  
-- --            ))))

-- --         Sufficient to show:                          [where MB = (\mb -> squash (mb >>=[m] (return[m].g ))) ]
-- --             =  squash (ma >>=[m] (\a -> return[m] (f a >><= MB)))

-- --                via >>=[m] associates with >><=  (???)

-- --             =  squash (ma >>=[m] (\a -> return[m] (f a))) >><= MB

-- --         =  ra >><= (
-- --              \ma -> squash (ma >>=[m] (\a -> return[m] (f a))) >><=
-- --                (\mb -> squash (mb >>=[m] (return[m].g)))
-- --            )

-- --         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].f)) >><=
-- --                    (\mb -> squash (mb >>=[m] (return[m].g)))))

-- --         =  (ra >><= (\ma -> squash (ma >>=[m] (return[m].f)))) >><=
-- --              (\mb -> squash (mb >>=[m] (return[m].g)))

-- --         =  (ra >><= (\ma -> squash (ma >>=[m] (return[m].f)))) >>=[r] g
-- --         =  (ra >>= f) >>= g


-- ---------------------------------
-- --  A second attempt with an m-r-bind


-- class (Monad m) => RelMonad m r where
--     retRel :: m a -> r a
--     (>><=) :: r a -> (m a -> m (r b)) -> r b

-- -- B{x} = return[m] x | my >>=[m] \y -> B{x}
-- --
-- -- Relative monad laws (just like the standard monad laws)
-- -- 0. retRel x >><= (\y -> return ((f y)) =  f x

-- --          and  more generally:
-- -- 1. retRel x >><= (\y -> B{f y}) =  B{f x}                          -- idL   [OLD: where f :: m a -> m (r b) ]

-- -- 2. x >><= return[m].retRel      =  x                            -- idR
-- -- 3. (x >><= return[m].f) >><= g  =  x >><= return[m].(\y => f y >><= g)    -- Assoc

-- -- ????
-- --     retRel x >><= (\ma -> ma >>=[m] return[m].f)
-- --  =  x >>=[m] return[m].f)

-- --------------------------------
-- --   return[m] x >>=[m] f    =  f x                             -- idL
-- --   x >>=[m] return[m]   =  x                                  -- idR
-- --   (x >>=[m] f) >>=[m] g  =  x >>=[m] (\y -> f y >>=[m] g)    -- Assoc

-- -----------------------------------------
-- -----------------------------------------------    
              
-- type family RBase (r :: * -> *) :: * -> *  -- r derives a monad instance from RBase            

-- instance (RelMonad (RBase r) r) => Monad r where
--     ra >>= f  =  ra >><= (\ma -> ma >>= return.f)
--     return    =  retRel . (return :: a -> RBase r a)

-- -- Deriving the monad laws.

-- --    return[r] x >>=[r] f
-- --  = retRel(return[m] x) >>=[r] f
-- --  = retRel(return[m] x) >><= (\ma -> ma >>=[m] \a -> return[m](f a))

-- --  = return[m] x >>=[m] return[m].f
-- --  = return[m](f x)



-- --------- TODO


-- --   ra >>=[r] return[r]
-- --         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].return[r])))
-- --         =  ra >><= (\ma -> retRel ma)
-- --         =  ra

-- --   ra >>= (\a -> f a >>= g)       [where f: a -> r a] 
-- --         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].(\a -> f a >>=[r] g))))
-- --         =  ra >><= (\ma -> squash (ma >>=[m] (\a -> return[m] (f a >>=[r] g))))

-- --         =  ra >><= (\ma -> squash (ma >>=[m] (\a -> return[m] (
-- --                f a >><= (\mb -> squash (mb >>=[m] (return[m].g )))  
-- --            ))))

-- --         Sufficient to show:                          [where MB = (\mb -> squash (mb >>=[m] (return[m].g ))) ]
-- --             =  squash (ma >>=[m] (\a -> return[m] (f a >><= MB)))

-- --                via >>=[m] associates with >><=  (???)

-- --             =  squash (ma >>=[m] (\a -> return[m] (f a))) >><= MB

-- --         =  ra >><= (
-- --              \ma -> squash (ma >>=[m] (\a -> return[m] (f a))) >><=
-- --                (\mb -> squash (mb >>=[m] (return[m].g)))
-- --            )

-- --         =  ra >><= (\ma -> squash (ma >>=[m] (return[m].f)) >><=
-- --                    (\mb -> squash (mb >>=[m] (return[m].g)))))

-- --         =  (ra >><= (\ma -> squash (ma >>=[m] (return[m].f)))) >><=
-- --              (\mb -> squash (mb >>=[m] (return[m].g)))

-- --         =  (ra >><= (\ma -> squash (ma >>=[m] (return[m].f)))) >>=[r] g
-- --         =  (ra >>= f) >>= g




                 
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
