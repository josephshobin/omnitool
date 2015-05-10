class (Functor f, Functor g) => Adjoint f g where
     counit :: f (g a) -> a
     unit   :: a -> g (f a)
-- Note that the "a" in counit is really the identity functor in C, and the "a" in unit is really the identity functor in D.

-- We can also recover the hom-set adjunction definition from the counit/unit definition.

phiLeft :: Adjoint f g => (f a -> b) -> (a -> g b)
phiLeft f = fmap f . unit            

phiRight :: Adjoint f g => (a -> g b) -> (f a -> b)
phiRight f = counit . fmap f

(>>==) :: Adjoint f g => a -> (f a -> b) -> g b
x >>== ff = phiLeft ff

(==>>) :: Adjoint f g => f a -> (a -> g b) -> b
y ==>> gg = phiRight gg x


--In any case, we can now define a Monad from our unit/counit adjunction like so:

instance Adjoint f g => Monad (Compose g f) where
    return x  =  x >>== id
    (x :: g(f(a))) >>= (gf :: a -> g(f b))  =  fmap g (\fa -> fa ==>> gf)                   
    -- return x = Compose $ unit x
    --x >>= f  = Compose . fmap counit . getCompose $ fmap (getCompose . f) x

-- Now we can implement the classic adjunction between (a,) and (a ->):

instance Adjoint ((,) a) ((->) a) where
    -- counit :: (a,a -> b) -> b
    counit (x, f) = f x
    -- unit :: b -> (a -> (a,b))
    unit x = \y -> (y, x)

-- And now a type synonym

type State s = Compose ((->) s) ((,) s)
