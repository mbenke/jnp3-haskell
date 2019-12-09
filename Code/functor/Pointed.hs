module Pointed where

class Functor f => Pointed f where
  ppure :: a -> f a

instance Pointed [] where
  ppure = (:[])
