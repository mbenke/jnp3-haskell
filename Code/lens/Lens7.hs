{-# LANGUAGE Rank2Types #-}

module Lens7 where
type Lens a b = forall t. Functor t => (b -> t b) -> (a -> t a)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = overF
  where overF f a = (\b' -> set b' a) <$> f (view a)

comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = l1 . l2

newtype I x = I { unI :: x }

instance Functor I where
  fmap f  = I . f . unI

newtype K b x = K { unK :: b }

instance Functor (K b) where
  fmap f (K b) = K b

overF l = l

over :: Lens a b -> (b -> b) -> (a -> a)
over l f = unI . l (I . f)
-- over l f = unI . overF l (I . f)

set :: Lens a b -> b -> a -> a
set l x = over l (const x)

view :: Lens a b -> a -> b
view l a = unK $ overF l K a


