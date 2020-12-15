# Zadanie 6 - podsumowanie

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
```

taką, że `rep a` daje strumień złozony z samych `a`

* Zdefiniuj strumień wszystkich liczb naturalnych

``` haskell
nats :: Stream Integer
```

* Zdefiniuj instancje `Functor` i `Applicative` dla `Stream`

## Supply

W wielu zastosowaniach potrzebujemy zasobu dostarczającego swoich kolejnych elementów, np. świeże identyfikatory, liczby pseudolosowe itp.

Zdefiniujmy typ

``` haskell
newtype Supply s a = S { runSupply :: Stream s -> (a, Stream s) }
```

typ ten modeluje obliczenie, które używa elementów strumienia `s` i daje wynik obliczenia `a` oraz niewykorzystaną resztę strumienia

Zaimplementuj funkcje

``` haskell
get :: Supply s s
```

(obliczenie, które wyjmuje element ze strumienia i daje go w wyniku)

``` haskell
pureSupply :: a -> Supply s a
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
mapSupply2 :: (a->b->c) -> Supply s a -> Supply s b -> Supply s c
bindSupply :: Supply s a -> (a->Supply s b) -> Supply s b
```

Wreszcie, będziemy potrzebowali wyłuskać wynik obliczenia korzystającego z zasobu; zdefiniuj


``` haskell
evalSupply :: Supply s a -> Stream s -> a
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
