{-# LANGUAGE Rank2Types #-}

module Lens6 where
data Lens a b = Lens { view :: a -> b
                     , overF :: forall t. Functor t => (b -> t b) -> (a -> t a)
                     }

newtype I x = I { unI :: x }

instance Functor I where
  fmap f  = I . f . unI

over :: Lens a b -> (b -> b) -> (a -> a)
over l f = unI . overF l (I . f)
-- over l f a = unI $ overF l f' a where f' b = I (f b)

set :: Lens a b -> b -> a -> a
set l x = over l (const x)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = Lens view overF
  where overF f a = (\b' -> set b' a) <$> f (view a)


comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1)
                  (overF l1 . overF l2)
