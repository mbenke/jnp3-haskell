# I/O - co jest pod maską?

Rozwiązanie problemu I/O jest oparte na typach i klasach. Musimy powiedzieć o nich coś więcej, do I/O wrócimy za chwilę.

## Typy algebraiczne

```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a)

mapTree :: (a->b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Branch l r) = Branch (m l) (m r) where
m = mapTree f
```
**Leaf** jest 1-argumentowym konstruktorem,
**Branch** — 2-argumentowym. Per analogiam mówimy, że **Tree** jest jednoargumentowym *konstruktorem typu*:

-   jeśli **x** jest wartością, to **Leaf x** jest wartością;
-   jesli **a** jest typem, to **Tree a** jest typem.

### Typy Maybe i Either

Dwa przydatne typy (predefiniowane w Prelude):

```haskell
    data Maybe a = Nothing | Just a
    data Either a b = Left a | Right b
    -- Prelude> head []
    -- *** Exception: Prelude.head: empty list

    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    -- *Main> safeHead []
    -- Nothing

    safeHead2 :: [a] -> Either String a
    safeHead2 [] = Left "Empty list"
    safeHead2 (x:xs) = Right x
```

<!--
### Synonimy

Czasem przydatne jest wprowadzenie własnej nazwy (synonimu) dla jakiegoś typu.

```haskell
    type Name = String
    type Possibly = Either Name

    safeHead3 :: [a] -> Possibly a
    safeHead3 [] = Left "Empty list"
    safeHead3 (x:xs) = Right x
```

Synonim **nie jest** konstruktorem typu; jest identyczny z nazywanym typem.
-->

### Opakowywanie typów: **newtype**

Jeśli chcemy opakować istniejacy typ w nowy konstruktor typu, mozemy uzyć konstrukcji **newtype**:

```haskell
    newtype Identity a = Identity { runIdentity :: a }
      deriving (Eq, Show)
```
**newtype** działa niemal identycznie jak **data** z jednym konstruktorem(ale efektywniej;
pakowanie/odpakowywanie odbywa się w czasie kompilacji a nie wykonania).

przypomnienie: `runIdentity` jest etykietą pola - automatycznie definiuje funkcję wyłuskująca to pole:

```
runIdentity :: Identity a -> a
runIdentity (Identity a) = a
```

Spróbujmy w `ghci`:
```
    *Newtype> Identity "Ala"
    Identity {runIdentity = "Ala"}
    *Newtype> runIdentity it
    "Ala"
```
### Klasy konstruktorowe

Typy polimorficzne jak **\[a\]** czy **Tree a** mogą być instancjami klas (przeważnie pod warunkiem, ze **a** jest też instancją odpowiedniej klasy)…

```haskell
    data Tree a = Leaf a | Branch (Tree a) (Tree a)
      deriving Show

    instance Eq a => Eq (Tree a) where
      Leaf x == Leaf y = x == y
      Branch l r == Branch l' r' = (l==l')&&(r==r')
      _ == _ = False
```

…ale są też klasy, których instancjami są nie typy, a *konstruktory typów*. Na przykład funkcję **map** możemy uogólnić na inne pojemniki:

```haskell
    -- Klasa Functor jest zdefiniowana w Prelude
    -- class  Functor t  where
    --    fmap :: (a -> b) -> t a -> t b

    (<$>) :: Functor f => (a -> b) -> f a -> f b
    (<$>) = fmap

    instance Functor Tree where
     fmap f (Leaf a) = Leaf $ f a
     fmap f (Branch l r) = Branch (fmap f l)(fmap f r)
```

```
    *Tree> negate <$> Leaf 6
    Leaf (-6)
```


-   Typy takie jak **Tree** czy listy są pojemnikami przechowującymi obiekty

-   Instancja **Eq(Tree a)** mówi o własnościach pojemnika z zawartością

-   Instancja **Functor Tree** mówi o własnościach samego pojemnika, *niezależnie od zawartości*


```haskell
    import Prelude hiding(Functor(..))

    class Functor f where
      fmap :: (a -> b) -> (f a -> f b)

    -- [] poniżej oznacza konstruktor *typu* dla list
    instance Functor [] where
      fmap = map

    instance Functor Maybe where
      fmap f (Just x) = Just (f x)
      fmap f Nothing = Nothing
```

### Applicative

Chcemy dodać dwie liczby opakowane w **Maybe**

    *Applicative> :t (+1) <$> Just 5
    (+1) <$> Just 5 :: Num b => Maybe b
    *Applicative> :t (+) <$> Just 5
    (+) <$> Just 5 :: Num a => Maybe (a -> a)

czyli nie możemy napisać `(+) <$> Just 2 <$> Just 3`

W ogólności Functor nie wystarczy, potrzeba

```haskell
    class (Functor f) => Applicative f where
        pure  :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
```

teraz:

    *Applicative> (+) <$> Just 2 <*> Just 3
    Just 5

Definicja `Applicative` dla `Maybe` jest dość naturalna:

``` haskell
instance Applicative Maybe where
  pure = Just
  Just f <*> Just x = Just (f x)
  _ <*> _ = Nothing
```

### Ćwiczenia

:pencil: Rozważmy trochę iny typ drzew:

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```
Stwórz dla niego instancje klas `Eq`, `Show`, `Functor` (bez używania `deriving`).

Klasa `Show` zawiera metody służące do reprezentacji wartości jako `String`.
W uproszczeniu mozemy ja traktować jakby wyglądała tak:

```haskell
class Show a where
    show :: a -> String
```

:pencil:  Zdefiniuj klasę Pointed (funkcyjnych pojemników z singletonem)
```haskell
class Functor f => Pointed f where
 singleton :: a -> f a
```

i jej instancje dla list, `Maybe`, `Tree`:

```haskell
instance Pointed Maybe where
instance Pointed [] where ...
instance Pointed Tree where
```

### Obliczenia funkcyjne

Możemy zdefiniować typ, którego wartościami będą obliczenia,
z operacjami:

-   Obliczenie czyste (daje jakąś wartość, nie ma efektów ubocznych)

-   Sekwencjonowanie obliczeń:

    `obliczenie -> (wynik -> obliczenie) -> obliczenie`

-   Operacje pierwotne (np. wczytaj, wypisz, etc.)

Mechanizm wykonywania obliczeń (“interpreter”, maszyna wirtualna)

### Klasa Monad czyli programowalny średnik

```haskell
class Monad obliczenie where
  return :: a -> obliczenie a
  (>>=) :: obliczenie a -> (a -> obliczenie b) -> obliczenie b
```

-   Klasa **Monad** jest klasą konstruktorową (jak **Functor**).

-   Gdy **m** jest instancja **Monad**, to **m a** jest typem obliczeń o
    wyniku typu **a**.

-   **return x** jest czystym obliczeniem dającym wynik **x** (alternatywnie możemy używać **pure x**)

-   Operator `(>>=)` (zwany “bind”) sekwencjonuje obliczenia
    (“programowalny średnik”)

*Monads: just a fancy name for scripting your semicolons
(via @TacticalGrace, inspired by @donsbot)*

![image](monad.jpg)

Jeżeli kolejne obliczenie nie korzysta z wyniku (a tylko z efektu)
poprzedniego, możemy użyć operatora `(>>)`

    o1 >> o2 = o1 >>= \_ -> o2

    print A >> print B

Ponadto czasami wygodniej jest zapisywać złożenie obliczeń w kolejności
analogicznej do złożenia funkcji:

    f =<< o = o >>= f

### Najprostszy efekt: brak efektu

Najprostsza monadą jest **Identity**
(moduł **Control.Monad.Identity** w bibliotece standardowej)

```haskell
newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return a = Identity a     -- return = id
  (Identity x) >>= f = f x  -- x >>= f = f x
```

### `Monad` jest podklasą `Applicative` (w nowszych wersjach)

```haskell
class Applicative m => Monad m where ...
```

Dlatego żeby stworzyć instancję `Monad` wymagana jest instancja `Applicative` (a zatem także Functor). Dlatego możemy zobaczyć błąd

```
Code/monad/Identity.hs:5:10: error:
    • No instance for (Applicative Identity)
        arising from the superclasses of an instance declaration
    • In the instance declaration for ‘Monad Identity’
```

Zauważmy jednak, że `<*>` możemy odtworzyć przy pomocy metod klasy `Monad` 

```haskell
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = do { f <- mf; a <- ma; return (f a) }
```
:pencil: Napisz instancje `Functor`, `Applicative` dla Identity.


## Prosty efekt: obliczenia zawodne

**Cel:** chcemy modelować obliczenia, które czasem czasem zawodzą

**Środek:** monada `Maybe`

```haskell
instance Monad Maybe where
  return = Just
  Nothing >>= k = Nothing
  Just x >>= k = k x
```

-   Obliczenie, które nie daje wyniku: `Nothing`

-   Obliczenie, które daje wynik x: `Just x`

-   Jeśli pierwsze obliczenie zawodzi, to cała sekwencja zawodzi

```
> import Text.Read
> :t readMaybe
readMaybe :: Read a => String -> Maybe a
> :t readEither
readEither :: Read a => String -> Either String a

> do {n <- readMaybe “ala” ; return (n+1) }
Nothing
> do {n <- readMaybe “41” ; return (n+1) }
Just 42
```

Możemy oczywiście korzystać z **Maybe** bez mechanizmu monad:

```haskell
case obliczenie1 of
  Nothing -> Nothing
  Just x -> case obliczenie2 of
    Nothing -> Nothing
    Just y -> obliczenie3
```

Monada pozwala nam to zapisać zgrabniej:

```haskell
obliczenie1 >>= (\x -> obliczenie2 >>= (\y -> obliczenie3))
```

albo

```haskell
do { x <- obliczenie1; y <- obliczenie2; obliczenie3 }
```

:pencil: Zdefiniuj funkcję `addMaybe :: Maybe Int -> Maybe Int -> Maybe Int`, dodającą dwie liczby opakowane w `Maybe`
* przy pomocy `Applicative`
* przy pomocy `Monad`

# Zaszłości

<!--
* `isGraphClosed` - https://gist.github.com/mbenke/eb7b2e515633540ec0f60de218b21e1c
* `nth` przy użyciu `foldr` - https://github.com/mbenke/jnp3-haskell/blob/master/Code/fold/fold-nth.hs
-->

## `foldl` i `foldr`
Są dwa naturalne sposoby implementacji `fold` na listach:

``` haskell
foldr f z (x:xs) = f x (foldr f z xs)
foldl f z (x:xs) = foldl f (f z x) xs
```

W ML czesto preferowany jest `foldl` (jako ogonowy); w Haskellu raczej używamy `foldr`. 

**Ciekawostka:** ten ostatni jest też ogólniejszy: można zdefiniować `foldl` przy pomocy `foldr`:

``` haskell
foldLeft :: (b->a->b) -> b -> [a] -> b
foldLeft f v xs = fold (\x g -> (\a -> g (f a x) ) ) id xs v
```

...ale nie na odwrót.

Więcej: [https://wiki.haskell.org/Foldr_Foldl_Foldl'](https://wiki.haskell.org/Foldr_Foldl_Foldl') (ale to raczej po zajęciach o leniwej ewaluacji).

## `nth`

``` haskell
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
```

# Wskazówka do zadania - listy różnicowe

Reprezentacja list pozwalająca na efektywniejszą konkatenację: listę `xs :: [T]` reprezentujemy jako funkcję `f :: [T] -> [T]`
taką, że `f ys = xs ++ ys`; patrz np. `Data.DList` w pakiecie `dlist`:

``` haskell
newtype DList a = DL { unDL :: [a] -> [a] }

-- | Convert a list to a dlist
fromList    :: [a] -> DList a
fromList    = DL . (++)

-- | Convert a dlist to a list
toList      :: DList a -> [a]
toList      = ($[]) . unDL

empty :: DList
empty = DL id
```
:pencil: napisz funkcje `cons` i `snoc`  (doklejenie elementu na początku i końcu) oraz `append`

``` haskell
cons :: a -> DList a -> DList a
snoc :: DList a -> a -> DList a
append :: DList a -> DList a -> DList a
```

Ponieważ przy rysowaniu często uzywamy składania obrazów, podobną szuczkę możemy zastosować w zadaniu:

``` haskell
type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun
blank = id
(&) = (.)
```
