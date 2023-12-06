# Zadanie 6 - podsumowanie

W pliku `SupplySkeleton.hs` znajdziesz szkielet rozwiązania. Skopiuj go do pliku `Supply.hs` i uzupełnij brakujące elementy zgodnie z poniższym opisem.

## Strumienie

Typ list pozwala definiować zarówno listy skończone jak i nieskończone.
Rozważmy typ zawierający wyłącznie strumienie nieskończone:

``` haskell
infixr 5 :>
data Stream a = a :> (Stream a)
```

* Napisz funkcję zamieniającą `Stream` na zwykłą (nieskończoną) listę

``` haskell
streamToList :: Stream a -> [a]
```

* Napisz instancję klasy `Show`, pokazującą pierwsze 20 elementów strumiena

``` haskell
instance Show a => Show (Stream  a) where
...
```

* Napisz funkcję

``` haskell
rep :: a -> Stream a
-- | Example:
--
-- >>> rep 0
-- 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...
```

taką, że `rep a` daje strumień złozony z samych `a`

* Zdefiniuj strumień wszystkich liczb naturalnych

``` haskell
nats :: Stream Integer
-- | Example:
--
-- >>> nats
-- 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,...
```

* Zdefiniuj instancje `Functor` i `Applicative` dla `Stream`. Upewnij się że `show (pure (*2) <*> nats) == show (fmap (*2) nats)`

``` haskell
-- >>> fmap (+1) nats
-- 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,...
-- >>> pure (+1) <*> nats
-- 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,...
--- >>> show (pure (*2) <*> nats) == show (fmap (*2) nats)
-- True
```

* Bonus: zdefiniuj funkcję `zipStreamsWith :: (a->b->c) -> Stream a -> Stream b -> Stream c`. Czy potrafisz zdefiniować `(<*>)` w terminach tej funkcji?

``` haskell
-- >>> zipStreamsWith (+) nats nats
-- 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,...
```

## Supply

W wielu zastosowaniach potrzebujemy zasobu dostarczającego swoich kolejnych elementów, np. świeże identyfikatory, liczby pseudolosowe itp.

Zdefiniujmy typ

``` haskell
newtype Supply s a = S { runSupply :: Stream s -> (a, Stream s) }
```

typ ten modeluje obliczenie, które używa elementów strumienia `s` i daje wynik obliczenia `a` oraz niewykorzystaną resztę strumienia

Będziemy potrzebowali wyłuskać wynik obliczenia korzystającego z zasobu; zdefiniujmy

``` haskell
evalSupply :: Supply s a -> Stream s -> a
evalSupply p s = fst $ runSupply p s
```

Zaimplementuj funkcje

``` haskell
get :: Supply s s
-- | Example:
--
-- >>> evalSupply get nats
-- 0
```

(obliczenie, które wyjmuje element ze strumienia i daje go w wyniku)

``` haskell
pureSupply :: a -> Supply s a
-- | Example:
--
-- >>> evalSupply (pure 42) nats
-- 42
```

(obliczenie czyste, które nie korzysta ze strumienia)

Dla zdefiniowania instancji

``` haskell
instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  (>>=) = bindSupply
```

potrzebujemy funkcji

``` haskell
mapSupply :: (a->b) -> Supply s a -> Supply s b
-- >>> evalSupply (mapSupply (+1) get) nats
-- 1
mapSupply2 :: (a->b->c) -> Supply s a -> Supply s b -> Supply s c
bindSupply :: Supply s a -> (a->Supply s b) -> Supply s b
```

Przy czym `fmap f x` używa tych samych zasobów co `x`, 
natomiast `x <*> y` i  `x >> y` używa takich zasobów jak kolejno `x` i `y`.

```
-- >>> evalSupply (get >> get >> get) nats
-- 2
-- >>> evalSupply (do { x <- get; y <- get; return (x,y)}) nats
-- (0,1)
```

Użyj tak zdefiniowanej monady `Supply` do ponumerowania lisci drzewa od lewej do prawej:

``` haskell
data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving (Eq, Show)

labelTree :: Tree a -> Tree Integer
labelTree t = evalSupply (go t) nats
  where
    go :: Tree a -> Supply s (Tree s)
    go = undefined

size :: Tree a -> Int
size (Leaf _) = 1
size (Branch l r) = size l + size r

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Branch l r) = toList l ++ toList r
```

## Testy

Funkcja `labelTree` musi spełniac własności

``` haskell
prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = size (labelTree t) == size t

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = toList (labelTree t) == [0..n]
    where n = fromIntegral $ size t - 1

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = labelTree (labelTree t) == labelTree t
```
