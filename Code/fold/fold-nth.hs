{-# LANGUAGE ScopedTypeVariables #-}

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- Counts from 0 for compatibility with (!!)
safe_nth :: [a] -> Int -> Maybe a -- b ~ (Int -> Maybe a)
safe_nth = foldr f z where
  z :: Int -> Maybe a
  z _ = Nothing

  f :: a -> (Int -> Maybe a) -> (Int -> Maybe a)
  f a b 0 = Just a
  f a b n = b (n-1)

nth :: [a] -> Int -> a
nth xs = fromJust . safe_nth xs

fromJust (Just a) = a

-- >>> nth [1,2,3] 1
-- 2

unsafe_nth :: [a] -> Int -> a -- b ~ (Int -> Maybe a)
unsafe_nth = foldr f z where
  z :: Int -> a
  z _ = error "Index out of range"

  f :: a -> (Int -> a) -> (Int -> a)
  f a b 0 = a
  f a b n = b (n-1)
