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
      ScopedTypeVariables,
      RankNTypes #-}
    
module RelMonad where

import Control.Monad

-- Same precedence as '>>='    
infix 1 >%=

class (Monad m) => RelMonad m r where
    retRel :: m a -> r a
    (>%=) :: r a -> (m a -> m (r b)) -> r b
    

-----------------------------------------------------------------------------    

data Result a = Success a
              | Failure String
                deriving (Show, Eq)

class Monad m => MonadResult m where
    succeedR :: forall a. a -> m a
    succeedR = return
    failR :: String -> m a
    fold :: forall a b. (a -> m b) -> (String -> m b) -> m a -> m b
    

setMessage :: MonadResult m => forall a. String -> m a -> m a
setMessage msg = fold succeedR (const (failR msg))

-- type family RMVia :: (* -> *) -> (* -> *) -> (* -> *)

-- class RMVia r1 r3 ~ r2 => RMCompose r1 r2 r3    
    
-- instance (RMVia r1 r3 ~ r2, RMCompose r1 r2 r3, RelMonad r1 r2, RelMonad r2 r3) => RelMonad r1 r3 where
--     retRel     = (retRel :: r2 a -> r3 a).(retRel :: r1 a -> r2 a)
--     r3x >%= f  =  r3x >%= \(r2x::r2 a) ->
--                   r2x >%= \(r1x::r1 a) ->
--                   ((r1tor1r2 (f r1x)) :: r1 (r2 (r3 bb)))
--         where r1tor1r2 :: r1 (r3 bb) -> r1 (r2 (r3 bb))
--               r1tor1r2 r1b = r1b >>= return.(return :: r3 bb -> r2 (r3 bb))

--instance (RMVia r1 r3 ~ r2, RMCompose r1 r2 r3, RelMonad r1 r2, RelMonad r2 r3) => RelMonad r1 r3 where

--newtype RMon r1 r2 = RMon { getRMon :: RelMonad r1 r2 }

data RMonadI m r = RMonadI
    { _rRet  :: forall a.   m a -> r a
    , _rBind :: forall a b. r a -> (m a -> m (r b)) -> r b
    }    

-- type RBind r1 r2 = forall a b. r2 a -> (r1 a -> r1 (r2 b)) -> r2 b

rmCompose :: Monad r1 => RMonadI r1 r2 -> RMonadI r2 r3 -> RMonadI r1 r3
rmCompose (RMonadI ret1to2 (>%%=)) (RMonadI ret2to3 (>%%%=)) =
    RMonadI { _rRet  = ret2to3.ret1to2
            , _rBind =
                \x3 f ->
                    x3 >%%%= \x2 ->
                    x2 >%%=  \x1 ->
                    f x1 >>= \y  ->
                    return (ret1to2 (return y))
            }

    
    
-- bindCompose :: (Monad r1, Monad r2) => (forall a. r1 a -> r2 a) -> (forall a. r2 a -> r3 a) ->
--                RBind r1 r2 -> RBind r2 r3 -> RBind r1 r3
-- bindCompose ret12 ret23 (>%%=) (>%%%=) r3x f = 
--     r3x >%%%= \(r2x::r2 a) ->
--     r2x >%%= \(r1x::r1 a) ->
--         ((f r1x) >>= return.return)

-- :: r1 (r2 (r3 b))))
--    where -- r1tor1r2 :: forall bb r1 r2 r3. r1 (r3 bb) -> r1 (r2 (r3 bb))
--          r1tor1r2 r1b = r1b >>= return.(return :: r3 bb -> r2 (r3 bb))
--    where r1tor1r2 :: forall bb r1 r2 r3. r1 (r3 bb) -> r1 (r2 (r3 bb))
--          r1tor1r2 r1b = r1b >>= return.(return :: r3 bb -> r2 (r3 bb))


-- instance MonadResult m => MonadResult (readerT m) where
--     failR s = (\ _ -> failR s)
--     fold f g ma = ReaderT ( \cf -> fold (\a -> runReaderT(f a) cf) (\s -> runReaderT (g s) cf) (ma cf) )

-----------------------------------------------------------------------------
    

--    joinR :: forall a. r (r a) -> r a
                 
-- Relative monad laws (just like the standard monad laws)
--   retRel x >%= f    =  f x                          -- idL
--   x >%= retRel      =  x                            -- idR
--   (rx >%= f) >%= g  =  rx >%= (\mx => f mx >%= g)    -- Assoc

----------------------------------------------
-- Other rough rules to consider:
--   ma >>=[m] (\a -> f a >%= g)  =  (ma >>=[m] f) >%= g   -- AssocMR when f :: a -> r a
-----------------------------------------------    

-- 'r' derives a monad instance from 'MBase r'
type family MBase (r :: * -> *) :: * -> *  
              
class JoinR r where
    joinR :: r (r a) -> r a

-- Laws
--    'joinR rra  =  rra >>=[r] id
--                =  rra >%= \mra -> joinR (retRel mra)'

-- --   'joinR.return[r] = id   :: r a -> r a'

--    `joinR (joinR rrrx) = joinR (rrrx >=[r] return[r].joinR)  --  for rrrx :: R (R (R A))
--          i.e. `joinR (joinR rrrx) = joinR (fmap joinR rrrx)


--   ROUGH
--     'joinR (retRel (return[m] rx))  =  rx'     --  i.e. 'joinR.return[r] = id :: r a -> r a'

--     'joinR (retRel (ma >>=[m] return[m].retRel.return[m])) = retRel ma

-- ????
--   Derivation:    retRel ma >>=[r] return[r] =  


-- This version works with Join
-- instance (JoinR r, RelMonad (MBase r) r) => Monad r where
    
--     ra >>= f  =  ra >%= \ma -> joinR (retRel (ma >>= retMRB.f))
--         where retMRB = return :: r b -> (MBase r) (r b)
                       
--     return    =  retRel . (return :: a -> MBase r a)

instance (JoinR r, RelMonad (MBase r) r) => Monad r where
    
    ra >>= f  =  ra >%= \ma -> (ma >>= retMRB.f)
        where retMRB = return :: r b -> (MBase r) (r b)
                       
    return    =  retRel . (return :: a -> MBase r a)
                 
                                  
-- -- Equivalent, suggested by Jacob, requires GHC 7.10 (right?) or adding Functor m to RelMonad m r
-- -- It makes a comparison with MTL easier.
-- ra >>= f  =  ra >%= (\ma -> joinR (retRel (fmapMRB f ma )))  
--     where fmapMRB = fmap   :: (a -> r b) -> ((MBase r) a -> (MBase r) (r b))
                 
                 
-- Derivations of the monad laws. 

--   return[r] xma >>= f
--         =  retRel (return[m] x) >%=[r] \ma -> joinR (retRel (ma >>=[m] return[m].f))
--         =  joinR (retRel (return[m] x >>=[m] return[m].f))
--         =  joinR (retRel (return[m] (f x)))
--         =  f x                               -- law.joinR

--   ra >>=[r] return[r]
--         =  ra >%= \ma -> joinR (retRel (ma >>=[m] return[m].retRel.return[m]))
--         =  ra >%= \ma -> joinR (retRel (return[m] (retRel(ma >>=[m] return[m]))))  -- TODO: Derived rule?
--         =  ra >%= \ma -> joinR (retRel (return[m] (retRel ma)))                    -- law.joinR

--         =  ra >%= \ma -> retRel ma             -- OR: direct via law.joinR2 (?)
--         =  ra



-- TODO


--   ra >>= (\a -> f a >>= g)       [where f: a -> r a] 
--         =  ra >%= \ma -> joinR (retRel (ma >>=[m] \a -> return[m] (f a >>=[r] g)))

--         =  ra >%= \ma -> joinR (retRel (ma >>=[m] \a -> return[m] (
--                f a >%= \mb -> joinR (retRel (mb >>=[m] (return[m].g )))  
--            )))

--         Sufficient to show:                          [where MB = (\mb -> squash (mb >>=[m] (return[m].g ))) ]
--             =  joinR (retRel (ma >>=[m] (\a -> return[m] (f a >%= MB))))

--                via >>=[m] associates with >%=  (???)

--             =  joinR (retRel (ma >>=[m] (\a -> return[m] (f a)))) >%= MB

--         =  ra >%= (
--              \ma -> squash (ma >>=[m] (\a -> return[m] (f a))) >%=
--                (\mb -> squash (mb >>=[m] (return[m].g)))
--            )

--         =  ra >%= \ma -> joinR (retRel (ma >>=[m] return[m].f)) >%=
--                   \mb -> joinR (retRel (mb >>=[m] return[m].g))

--         =  (ra >%= \ma -> joinR (retRel (ma >>=[m] return[m].f))) >%=
--                    \mb -> joinR (retRel (mb >>=[m] return[m].g))

--         =  (ra >%= \ma -> joinR (retRel (ma >>=[m] return[m].f))) >>=[r] g
--         =  (ra >>= f) >>= g
    
-----------------------------------------------


-- class (Monad m) => RelMonad0 m r where
--     retRel :: m a -> r a
--     (>%=) :: r a -> (m a -> r b) -> r b
                 
-- class (RelMonad0 m r, Monad r) => RelMonad m r

-- -- Relative monad laws (just like the standard monad laws)
-- --   retRel x >%= f    =  f x                          -- idL
-- --   x >%= retRel      =  x                            -- idR
-- --   (x >%= f) >%= g  =  x >%= (\y => f y >%= g)    -- Assoc

-- --   ma >>=[m] (\a -> f a >%= g)  =  (ma >>=[m] f) >%= g    when f :: a -> r a

-- -----------------------------------------------    
              
-- type family MBase (r :: * -> *) :: * -> *  -- r derives a monad instance from MBase
              
-- class Squash r where
--     squash :: (MBase r) (r a) -> r a

-- -- Squash laws  
-- --     squash (return[m] rx)  =  rx
-- --     squash (ma >>=[m] (return[m].return[r]))  =  retRel ma  (TODO: Check instances)

-- --      ???   squash (ma >>=[m] (\a -> return[m] (f a))) >%= (\mb -> squash (mb >>=[m] (return[m].g )))

-- --            squash (ma >>=[m] (\a -> return[m] (f a))) >%= g
-- --         =  squash (ma >>=[m] (\a -> return[m] (f a) >%= g))    [f : a -> r a]



-- --     if f :: a -> m a
-- --     squash (f (return[r]  >%= (\ma -> 


-- instance (RelMonad0 (MBase r) r, Squash r) => Monad r where
--     ra >>= f  =  ra >%= (\ma -> squash (ma >>= return.f))
--     return    =  retRel . (return :: a -> MBase r a)

-- -- Deriving the monad laws.

-- --   return(r) x >>= f
-- --         =  retRel(return[m] x) >%= (\ma -> squash (ma >>=[m] (return[m].f)))
-- --         =  squash ((return[m] x) >>=[m] (return[m].f))
-- --         =  squash (return[m](f x))
-- --         =  f x


-- --   ra >>=[r] return[r]
-- --         =  ra >%= (\ma -> squash (ma >>=[m] (return[m].return[r])))
-- --         =  ra >%= (\ma -> retRel ma)
-- --         =  ra

-- --   ra >>= (\a -> f a >>= g)       [where f: a -> r a] 
-- --         =  ra >%= (\ma -> squash (ma >>=[m] (return[m].(\a -> f a >>=[r] g))))
-- --         =  ra >%= (\ma -> squash (ma >>=[m] (\a -> return[m] (f a >>=[r] g))))

-- --         =  ra >%= (\ma -> squash (ma >>=[m] (\a -> return[m] (
-- --                f a >%= (\mb -> squash (mb >>=[m] (return[m].g )))  
-- --            ))))

-- --         Sufficient to show:                          [where MB = (\mb -> squash (mb >>=[m] (return[m].g ))) ]
-- --             =  squash (ma >>=[m] (\a -> return[m] (f a >%= MB)))

-- --                via >>=[m] associates with >%=  (???)

-- --             =  squash (ma >>=[m] (\a -> return[m] (f a))) >%= MB

-- --         =  ra >%= (
-- --              \ma -> squash (ma >>=[m] (\a -> return[m] (f a))) >%=
-- --                (\mb -> squash (mb >>=[m] (return[m].g)))
-- --            )

-- --         =  ra >%= (\ma -> squash (ma >>=[m] (return[m].f)) >%=
-- --                    (\mb -> squash (mb >>=[m] (return[m].g)))))

-- --         =  (ra >%= (\ma -> squash (ma >>=[m] (return[m].f)))) >%=
-- --              (\mb -> squash (mb >>=[m] (return[m].g)))

-- --         =  (ra >%= (\ma -> squash (ma >>=[m] (return[m].f)))) >>=[r] g
-- --         =  (ra >>= f) >>= g


-- ---------------------------------
-- --  A second attempt with an m-r-bind


-- class (Monad m) => RelMonad m r where
--     retRel :: m a -> r a
--     (>%=)  :: r a -> (m a -> m (r b)) -> r b

-- -- B{x} ::= return[m] x | my >>=[m] \y -> B{x}
-- --
-- -- Relative monad laws (just like the standard monad laws)
-- -- 1a retRel x >%= (\y -> return ((f y)) =  f x
-- -- 1b retRel x >%= (\y -> mz y >>=[m] \z -> return (f y)) =  mz x >>=[m] \z -> f x

-- --          and  more generally:
-- -- 1. retRel x >%= (\y -> B{f y}) =  B{f x}                          -- idL   [OLD: where f :: m a -> m (r b) ]

-- -- 2. x >%= return[m].retRel      =  x                            -- idR
-- -- 3. (x >%= return[m].f) >%= g  =  x >%= return[m].(\y => f y >%= g)    -- Assoc

-- -- ????
-- --     retRel x >%= (\ma -> ma >>=[m] return[m].f)
-- --  =  x >>=[m] return[m].f)

-- --------------------------------
-- --   return[m] x >>=[m] f    =  f x                             -- idL
-- --   x >>=[m] return[m]   =  x                                  -- idR
-- --   (x >>=[m] f) >>=[m] g  =  x >>=[m] (\y -> f y >>=[m] g)    -- Assoc

-- -----------------------------------------
-- -----------------------------------------------    
              
-- type family MBase (r :: * -> *) :: * -> *  -- r derives a monad instance from MBase            

-- instance (RelMonad (MBase r) r) => Monad r where
--     ra >>= f  =  ra >%= (\ma -> ma >>= return.f)
--     return    =  retRel . (return :: a -> MBase r a)

-- -- Deriving the monad laws.

-- --    return[r] x >>=[r] f
-- --  = retRel(return[m] x) >>=[r] f
-- --  = retRel(return[m] x) >%= (\ma -> ma >>=[m] \a -> return[m](f a))

-- --  = return[m] x >>=[m] return[m].f
-- --  = return[m](f x)



-- --------- TODO


-- --   ra >>=[r] return[r]
-- --         =  ra >%= (\ma -> squash (ma >>=[m] (return[m].return[r])))
-- --         =  ra >%= (\ma -> retRel ma)
-- --         =  ra

-- --   ra >>= (\a -> f a >>= g)       [where f: a -> r a] 
-- --         =  ra >%= (\ma -> squash (ma >>=[m] (return[m].(\a -> f a >>=[r] g))))
-- --         =  ra >%= (\ma -> squash (ma >>=[m] (\a -> return[m] (f a >>=[r] g))))

-- --         =  ra >%= (\ma -> squash (ma >>=[m] (\a -> return[m] (
-- --                f a >%= (\mb -> squash (mb >>=[m] (return[m].g )))  
-- --            ))))

-- --         Sufficient to show:                          [where MB = (\mb -> squash (mb >>=[m] (return[m].g ))) ]
-- --             =  squash (ma >>=[m] (\a -> return[m] (f a >%= MB)))

-- --                via >>=[m] associates with >%=  (???)

-- --             =  squash (ma >>=[m] (\a -> return[m] (f a))) >%= MB

-- --         =  ra >%= (
-- --              \ma -> squash (ma >>=[m] (\a -> return[m] (f a))) >%=
-- --                (\mb -> squash (mb >>=[m] (return[m].g)))
-- --            )

-- --         =  ra >%= (\ma -> squash (ma >>=[m] (return[m].f)) >%=
-- --                    (\mb -> squash (mb >>=[m] (return[m].g)))))

-- --         =  (ra >%= (\ma -> squash (ma >>=[m] (return[m].f)))) >%=
-- --              (\mb -> squash (mb >>=[m] (return[m].g)))

-- --         =  (ra >%= (\ma -> squash (ma >>=[m] (return[m].f)))) >>=[r] g
-- --         =  (ra >>= f) >>= g




                 
-----------------------------------------------             
-- Implement functions particular to  'RelMonad Maybe _'

-- Compose with a possibly failing operation f 
andThen :: (RelMonad Maybe r) => forall a b. r a -> (a -> Maybe b) -> r b
andThen ra f = ra >%= (\ma -> (return::r b -> Maybe (r b)) (retRel (ma >>= f)))
               
-- In general, here's a bunch more functions e.g., rMap, or, ensures...
-- ...

--------------------------------------------------
-- Implement retRel and >%= for a particular r, instantiating andThen, etc.

type Conf = [(String, String)]
data ReadMay a = ReadMay (Conf -> Maybe a)

type instance MBase ReadMay = Maybe

instance RelMonad Maybe ReadMay where
    retRel x               = ReadMay (const x)
                             
    (ReadMay reader) >%= f = ReadMay (\conf -> case f(reader conf) of
                                                 Nothing -> Nothing
                                                 Just (ReadMay res) -> res conf)
                                                                   
-- -- With JoinR
-- instance RelMonad Maybe ReadMay where
--     retRel x                = ReadMay (const x)
--     (ReadMay reader) >%= f = ReadMay (\conf -> case f(reader conf) of
--                                                   (ReadMay res) -> res conf)

-- instance JoinR ReadMay where                                                                    
--     joinR readMay = readMay >%= fMaybe
--         where fMaybe Nothing = ReadMay (\_ -> Nothing)
--               fMaybe (Just rdMay) = rdMay


--              fMaybe (Just (ReadMay reader)) = ReadMay (\conf -> case f(reader conf) of 
--                                                                   (ReadMay res) -> res conf)

--                                       Nothing = ReadMay (\_ -> Nothing)
--    joinR (Just rdMay) = rdMay
    
--instance RelMonad Maybe ReadMay                                                                         

-----------------------------------------------             
-- Implement functions particular to 'RelMonad [] _'

-- Concatenate
-- (++%) :: (RelMonad [] r) => r a -> r a -> r a
-- rxs ++% rys =
--     rxs >%=! \(xs::[a]) ->
--         ([rys >%=! \(ys::[a]) -> ([retRel (xs ++ ys)] :: [r a])] :: [r a])

--     where (>%=!) = ((>%=) :: r a -> ([a] -> [r a]) -> r a)
                             
--        ( [(retRel :: [a] -> r a) (xs ++ ys)] :: [r a])
                   
               
-- In general, here's a bunch more functions e.g., rmap, rfold, empty, ... 
-- ...


--------------------------------------------------
-- Implement retRel and >%= for a particular r, instantiating the above.

-- data ReadChoice a = ReadChoice (Conf -> [a])
-- instance Show a => Show (ReadChoice a) where
--     show (ReadChoice reader) = (reader []) >>= show

-- type instance MBase ReadChoice = []
                               
-- instance RelMonad0 [] ReadChoice where
--     retRel x                   = ReadChoice (const x)
--     (ReadChoice reader) >%= f = ReadChoice (\conf -> case f(reader conf) of
--                                                       (ReadChoice res) -> res conf)
-- instance Squash ReadChoice where
--     squash xs = ReadChoice (\conf -> concat (map (\(ReadChoice x) -> x conf) xs))
                   
-- instance RelMonad [] ReadChoice                                                                         

------------------------------------------------

-- Monad transformer version
--import Control.Monad.Trans.Reader

-- type ReaderTMaybe = ReaderT Int Maybe 

-- type instance MBase ReaderTMaybe = Maybe    
-- instance RelMonad ReaderTMaybe where
--   retRel x = ReaderT $ \_ -> x
--   reader >%= f = ReaderT (\r -> case f (runReaderT reader r) of
--                                   Just inner -> runReaderT inner r
--                                   Nothing    -> Nothing
--                           )
-----------------------------------------------

-- Specific to (MBase r)=Execution

-- ??

---------

-- data MyBe = MyBe Maybe 
-- type instance MBase MyBe = Execution
-- instance RelMonad MyBe where
--     retRel 
