module Identity where

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return a = Identity a     -- return = id
  (Identity x) >>= f = f x  -- x >>= f = f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
