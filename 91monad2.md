### Trzy prawa monadyki

Każda monada musi spełniać następujące prawa:

       1. (return x) >>= f == f x
       2. m >>= return == m
       3. (f >=> g) >=> h == f >=> (g >=> h)
    gdzie f >=> g = (\x -> (f x >>= g))

Pierwsze dwa prawa mówią, że *return* nie ma efektów.

Trzecie prawo mówi, że sekwencjonowanie obliczeń jest łączne,
czyli w pewnym sensie, że

     (o1;o2);o3 === o1;(o2;o3)

…i możemy je traktować jako sekwencję `o1;o2;o3`

Podobnie jak zapis `a+b+c` jest jednoznaczny dzięki łączności dodawania.

![image](monadlaws.jpg)


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

### Obsługa błędów

Przeważnie w wypadku błedu chcemy mieć więcej informacji niż tylko, że
obliczenie zawiodło: komunikat o błedzie.

Możemy do tego wykorzystać typ **Either**:

```haskell
instance Monad (Either error) where
  return = Right
  (Left e)  >>= _ = Left e
  (Right x) >>= k = k x
```

```
> Left “error” >> return 3 Left “error”
> do [n <- readEither “41” ; return (n+1) ]{} Right 42
```

## Użyteczne kombinatory monadyczne

w module `Control.Monad`:

```haskell
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
sequence :: Monad m => [m a] -> m [a]
sequence_ :: Monad m => [m a] -> m ()
liftM :: Monad m => (a1->r) -> m a1 -> m r -- fmap
liftM2 :: Monad m => (a1->a2->r)
                  -> m a1 -> m a2 -> m r
```

na przykład:

```
void(forM [1..7] print)
forM_ ['1'..'7'] putChar >> putStrLn ""
liftM2 (+) (readMaybe "40") (readMaybe "2")
```

:pencil: Napisz własną implementację funkcji

```haskell
sequence :: Monad m => [m a] -> m [a]
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
forM :: Monad m => [a] -> (a -> m b) -> m [b]
```
