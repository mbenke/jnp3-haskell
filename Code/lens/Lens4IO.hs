module Lens4IO where

data Lens a b = Lens { view :: a -> b
                     , over :: (b -> b) -> (a -> a)
                     , overIO :: (b -> IO b) -> (a -> IO a)
                     }

set :: Lens a b -> b -> a -> a
set l x = over l (const x)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = Lens view over overIO
  where over f a = set (f (view a)) a
        overIO f a = do
          b' <- f (view a)
          return $ set b' a


comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1)
                  (over l1 . over l2)
                  (overIO l1 . overIO l2)

overIO1 :: (a -> b) -> (b -> a -> a) -> (b -> IO b) -> (a -> IO a)
overIO1 view set f a = do
          b' <- f (view a)
          return $ set b' a

-- m >>= (\x -> return $ g x)  równoważne  g <$> m

overIO2 :: (a -> b) -> (b -> a -> a) -> (b -> IO b) -> (a -> IO a)
overIO2 view set f a = (\b' -> set b' a) <$> f (view a)
