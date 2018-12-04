{-# LANGUAGE ScopedTypeVariables #-}

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

safe_nth :: [a] -> Int -> Maybe a -- b ~ (Int -> MAybe a)
safe_nth = foldr f z where
  z :: Int -> Maybe a
  z _ = Nothing

  f :: a -> (Int -> Maybe a) -> (Int -> Maybe a)
  f a b 0 = Just a
  f a b n = b (n-1)

nth :: [a] -> Int -> a
nth xs = fromJust . safe_nth xs

-- >>> nth [1,2,3] 1
2

fromJust (Just a) = a
