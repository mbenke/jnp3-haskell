{-# LANGUAGE Rank2Types #-}

module Lens5Functor where

data Lens a b = Lens { view :: a -> b
                     , over :: (b -> b) -> (a -> a)
                     , overF :: forall t. Functor t => (b -> t b) -> (a -> t a)
                     }

set :: Lens a b -> b -> a -> a
set l x = over l (const x)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = Lens view over overF
  where over f a = set (f (view a)) a
        overF f a = (\b' -> set b' a) <$> f (view a)


comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1)
                  (over l1 . over l2)
                  (overF l1 . overF l2)
