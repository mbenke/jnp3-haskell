> Costate Comonad Coalgebra is equivalent of Java&#39;s member variable update technology for Haskell &mdash; [@PLT_Borat](https://twitter.com/PLT_Borat/status/228009057670291456)

# Soczewki (lenses)

Soczewki są mechanizmem ułatwiającym skupienie się na wybranym fragmencie danych.

Rozważmy typy

``` haskell
data Atom = Atom { _element :: String, _point :: Point }
data Point = Point { _x :: Double, _y :: Double }
```

### Odczyt

odczytanie współrzednej atomu jest proste:

``` haskell
getAtomX :: Atom -> Double
getAtomX = _x . _point
-- getAtomX atom = _x(_point atom)
```

### Zapis
natomiast ustawienie wartości jest  bardziej skomplikowana:

``` haskell
setPoint :: Point -> Atom -> Atom
setPoint p atom = atom { _point = p }

setElement :: String -> Atom -> Atom
setElement e atom = atom { _element = e }

setX, setY:: Double -> Point -> Point
setX x p = p { _x = x }
setY y p = p { _y = y }

setAtomX :: Double -> Atom -> Atom
setAtomX x a = setPoint (setX x (_point a)) a
```

Zauważmy, że `setAtomX` oprócz setterów, korzysta także z gettera `_point`;
oba rodzaje funkcji są powiązane, jeśli zatem chcemy stworzyć ogólny
mechanizm, warto zająć się nimi wspólnie.

W pierwszym przyblizeniu możemy zdefiniować typ soczewek

``` haskell
data Lens a b = Lens { view :: a -> b
                     , set :: b -> a -> a
                     }
```

oraz konkretne soczewki dla naszych typów:

``` haskell
point :: Lens Atom Point
point = Lens _point setPoint

element :: Lens Atom String
element = Lens _element setElement

x, y :: Lens Point Double
x = Lens _x setX
y = Lens _y setY
```

Nasze soczewki dają się składać, choć trochę niewygodnie:

``` haskell
comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1)
                  (\c a -> set l1 (set l2 c (view l1 a)) a)

setAtomX :: Double -> Atom -> Atom
setAtomX = set (point `comp` x)
```

### Modyfikacja

Powtarzającym się wzorcem jest modyfikacja wybranego pola przy pomocy funkcji.
Narzucającym się sposobem jest użycie gettera i settera:

``` haskell
over :: Lens a b -> (b -> b) -> (a -> a)
over l f a = set l (f (view l a)) a
```

aby przesunąć atom, możemy napisać

``` haskell
moveAtom :: Atom -> Atom
moveAtom = over (point `comp` x) (+1)
```

Zauważmy, że przy uzyciu `over` możemy także wyrazić `comp`:

``` haskell
comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1)
                  (\c -> over l1 (set l2 c))
```

## Wydajność

Podejście zaprezentowane powyżej nie jest w pełni zadowalające -
zauważmy, że `over` używa soczewki dwukrotnie: raz do odczytania wartości
i drugi raz do jej ustawienia. Z kolei, ponieważ `over` jest używane przez `comp`,
większe zagnieżdzenie szybko doprowadzi do wyraźnych nieefektywności.

Cóż zatem począć?  Na przykład uczynić `over` funkcją podstawową:

``` haskell
data Lens a b = Lens { view :: a -> b
                     , set :: b -> a -> a
                     , over :: (b -> b) -> (a -> a)
                     }
```

Do budowania soczewek przyda się funkcja pomocnicza, która zaoszczędzi nam
pisania `over` za każdym razem na nowo:

``` haskell
mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = Lens view set over
  where over f a = set (f (view a)) a

point :: Lens Atom Point
point = mkLens _point setPoint

element :: Lens Atom String
element = mkLens _element setElement

x, y :: Lens Point Double
x = mkLens _x setX
y = mkLens _y setY
```

Teraz możemy wyrazić złożenie soczewek tak, aby każdej z nich używało raz:

``` haskell
comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens { view = (view l2 . view l1)
                  , set  = (\c -> over l1 (set l2 c))
                  , over = (over l1 . over l2)
                  }
```

Zauważmy, że gdy przyjmiemy `over` jako pierwotne, `set` da się przez nie wyrazić:

``` haskell
set' :: Lens a b -> b -> a -> a
set' l x = over l (const x)
```

## Soczewki van Laarhovena

Teraz mamy jeszcze jeden problem - trudność używania soczewek z monadami;
na przykład poniższy kod się nie typuje:

``` haskell
askX :: Atom -> IO Atom
askX a = over (point `comp` x) askUser a
  where
    askUser :: Double -> IO Double
    askUser x = do
	putStrLn $ "Current X position is " ++ show x ++ ". New Position?"
	answer <- getLine
	return (read answer)
```

Dostaniemy komunikat o błędzie
```
    • Couldn't match type ‘IO Double’ with ‘Double’
      Expected type: Double -> Double
        Actual type: Double -> IO Double
    • In the second argument of ‘over’, namely ‘askUser’
      In the expression: over (point `comp` x) askUser a
```

Moglibyśmy próbować to naprawić, przez użycie `view` przed wykonaniem IO,
a `set` po nim, ale to znowu oznacza dwukrotne zagłębianie się w strukturę.

Zamiast tego możemy uzyć wariantu `over` pozwalającego na IO:

``` haskell
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
```

### Uogólnienie: `Functor`

Oczywiście może się okazać, że będziemy chcieli wykonywać podobne manipulacje
dla innych monad, albo wręcz konstruktorów, które monadami nie są.

Jeśli przyjrzymy się bliżej `overIO`, możemy zauważyć,
że występuje w nim schemat `m >>= (\x -> return $ g x)`,
który możemy zastąpić przez równoważne (a ogólniejsze) `g <$> m` (czyli `fmap g m`),
które nie wymaga monady a tylko funktora:

``` haskell
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
```

NB aby kompilator zaakceptował powyższą definicję `Lens`,
musimy użyć rozszerzenia

``` haskell
{-# LANGUAGE Rank2Types #-}
```

Jest to istotnie pewna komplikacja, zwłaszcza pojęciowa,
jednak pozwala nam na dalsze uproszczenia.

Skoro `overF` jest uogólnieniem `over`, możemy to drugie wyrazić przez
to pierwsze przy użyciu funktora identycznościowego:

``` haskell
newtype I x = I { unI :: x }

instance Functor I where
  fmap f  = I . f . unI

over :: Lens a b -> (b -> b) -> (a -> a)
over l f = unI . overF l (I . f)
-- over l f a = unI $ overF l f' a where f' b = I (f b)

set :: Lens a b -> b -> a -> a
set l x = over l (const x)

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = Lens view overF
  where overF f a = (\b' -> set b' a) <$> f (view a)


comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1)
                  (overF l1 . overF l2)
```

Z kolei przy pomocy funktora stałego

``` haskell
newtype K b x = K { unK :: b }

instance Functor (K b) where
  fmap f (K b) = K b
```

mozemy wyrazić `view`:

``` haskell
view :: Lens a b -> a -> b
view l a = unK $ overF l K a
```

Po wyeliminowaniu z `Lens` składowych `over` i `view` pozostała tam
tylko jedna składowa: `overF`.
Zatem typ `Lens a b` jest izomorficzny z typem `overF`, to jest

``` haskell
forall t. Functor t => (b -> t b) -> (a -> t a)
```

Mozemy zatem zdefiniować

``` haskell
type Lens a b = forall t . Functor t => (b -> t b) -> (a -> t a)
```

Co ciekawe, teraz soczewki są funkcjami i złożenie soczewek jest po prostu złożeniem funkcji:

``` haskell
comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = l1 . l2
```

Przy odpowiednich (prostych) definicjach operatorów infiksowych
możemy teraz pisać na przykład

``` haskell
atom2 = atom0 & point . x %~ (+1)
newx = atom2 ^. point . x
```

:pencil: Wypróbuj opisane tu definicje soczewek na typach `Atom` i `Point` (albo innych, np. typach stanu z Sokobana).

## Costate Comonad Coalgebra
Alternatywna definicja monady:

```haskell
return   :: a -> m a
join     :: m (m a) -> m a
```

`Comonad` to pojęcie dualne:

```haskell
extract   :: w a -> a
duplicate :: w a -> w (w a)
```

Przypomnijmy sobie monadę `State`:

```haskell
newtype State s a = State a -> (a, s)
```

`Store` jest (prawie) dualną komonadą:

```haskell
-- State ~ (a -> (a ,  s))
-- Store ~ (a ,  (a -> s))
-- Store ~ Costate
data Store a s = Store a (a -> s)

instance Comonad (Store a) where
  extract (Store a f) = f a
  duplicate (Store a f) = Store a (\b -> Store b f)
```

Koalgebra

```haskell
type Coalg f a = a -> f a

-- Coalg (Store a) s ~ s -> Store a s ~ Lens s a
```
