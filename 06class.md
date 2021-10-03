# Don't Repeat Yourself

https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Która funkcja jest czytelniejsza?

```haskell
handleEvent :: Event -> State -> State
handleEvent (KeyPress e) state@(coord, direction, boxes) =
     if isWinning state then state
     else
      (newCord, Just dir, adjacentBoxes boxes newCord dir)
   where
     newCord = adjacentCoordIfAccesible dir coord curMaze
     dir     = textToDir e
     curMaze = addBoxes boxes (removeBoxes myMaze)

handleEvent _ state = state
```

czy

```haskell
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (State dir (C x y) boxList) --(State pict (C x y) _)
      | isWinning (State dir (C x y) boxList) = (State R (C x y) boxList)
      | key == "Right" && (empty levelNow (C (x+1) y)) = (State R (C (x+1) y) boxList)
      | key == "Up" && (empty levelNow (C x (y+1))) = (State U (C x (y+1)) boxList)
      | key == "Left" && (empty levelNow (C (x-1) y))  = (State L (C (x-1) y) boxList)
      | key == "Down" && (empty levelNow (C x (y-1))) = (State D (C x (y-1)) boxList)
      | key == "Right"
        && ((comp (levelNow (C (x+1) y)) Box)
        && (empty levelNow (C (x+2) y))) = (State R (C (x+1) y) (moveTheBox (C (x+1) y) R boxList))
      | key == "Up"
        && ((comp (levelNow (C x (y+1))) Box)
        && (empty levelNow (C x (y+2)))) = (State U  (C x (y+1)) (moveTheBox (C x (y+1)) U boxList))
      | key == "Left"
        && ((comp (levelNow (C (x-1) y)) Box)
        && (empty levelNow (C (x-2) y)))  = (State L (C (x-1) y) (moveTheBox (C (x-1) y) L boxList))
      | key == "Down"
        && ((comp (levelNow (C x (y-1))) Box)
        && (empty levelNow (C x (y-2)))) = (State D (C x (y-1)) (moveTheBox (C x (y-1)) D boxList))
    where
       levelNow :: Coord -> Tile
       levelNow = addBoxes boxList (removeBoxes maze2)
       empty :: (Coord -> Tile) -> Coord -> Bool
       empty lvl c = if ((comp (lvl c) Ground) || ((comp (lvl c) Storage))) then True else False
 ```

# Typy z klasą

Wiele osób w rozwiązaniu ostatniego zadania pisało kod postaci

```haskell
if comp (lvl c) Box then Ground else (lvl c)
```

prawdopodobnie chcieli napisać

```haskell
if  (lvl c) == Box then Ground else (lvl c)
```

ale natknęli się na komunikat

```
No instance for (Eq Tile) arising from a use of ‘==’
```

Otóż równość ma typ

```haskell
(==) :: forall a.Eq a => a -> a -> Bool
```

co nalezy rozumieć jako `a -> a -> Bool` dla wszystkich typów `a` należących do klasy `Eq`.
Warto zauważyć, że nie jest to polimorfizm parametryczny: równość nie działa dla wszystkich typów tak samo (czasami mówi się w tym wypadku o polimorfiźmie *ad hoc*

Klasę należy tu rozumieć jako zbiór typów (dokładniej relację na typach, w tym wypadku jednoargumentową).

## Klasa Eq

```
Prelude> :info Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
	-- Defined in ‘GHC.Classes’
instance Eq Integer
  -- Defined in ‘integer-gmp-1.0.0.0:GHC.Integer.Type’
instance (Eq a, Eq b) => Eq (Either a b)
  -- Defined in ‘Data.Either’
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Eq Word -- Defined in ‘GHC.Classes’
…
```

Klasa `Eq` ma dwie metody: `(==))` i `(/=)`. Aby typ przynależał do tej klasy, należy podać ich implementację.

Ponieważ każdą można łatwo wyrazić przez negację drugiej, wystarczy podać jedną z nich.

Metody dla standardowych typów są zdefiniowane w Prelude (bibliotece standardowej, zawsze domyślnie importowanej).
Czasami te implementacje są warunkowe: np. równość na parach jest definiowana pod warunkiem istnienia równosci na argumentach.


### instance Eq Coord


```haskell
data Coord = C Integer Integer
instance Eq Coord where
  C x y == C x' y' = x == x' && y == y'
```

### Domyślne implementacje

Klasa `Eq` ma dwie metody: `(==))` i `(/=)`. Ponieważ każdą mozna łatwo wyrazić przez negację drugiej, wystarczy podać jedną z nich.

Definiując klasę możemy podać domyślną implementację

```haskell
class  Eq a  where
    (==), (/=) :: a -> a -> Bool
        -- Minimal complete definition:
        --      (==) or (/=)
    x /= y     =  not (x == y)
    x == y     =  not (x /= y)
```

### instance Eq Tile

Wspomniana funkcja `comp` bywała dosyć nudna:

```haskell
comp :: Tile -> Tile -> Bool
comp Wall Wall = True
comp Ground Ground = True
comp Storage Storage = True
comp Box Box = True
comp Blank Blank = True
comp _ _ = False
```

Definicja równości byłaby równie nudna; na szczęście Haskell potrafi wygenerować takie nudne definicje klas standardowych automatycznie:

```haskell
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
```

albo jeśli chcemy więcej niż jedną klasę

```haskell
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)
```

### Równość (nie) dla wszystkich

```haskell
data Activity world = Activity
        world
	(Event -> world -> world)
	(world -> Picture)
    deriving Eq
 ```

Niestety jako, że równość na funkcjach jest w ogólności nierozstrzygalna, nie uda nam się zdefiniować jej np. dla typu `Activity`

```
error:
    • Could not deduce (Eq (world -> Picture))
        arising from the third field of ‘Activity’
          (type ‘world -> Picture’)
      from the context: Eq world
```

Oczywiście możemy zdefiniować funkcję, która nie będzie prawdziwą równoscia, np

```haskell
instance Eq Activity where
  _ == _ = False
```

Niekoniecznie jest to jednak dobry pomysł; zwykle zakładamy, że równość ma pewne własności, np. że jest co najmniej relacją równoważnosci.

## Zalety klas

### Przeciążanie

Zamiast wymyślać osobną nazwę dla każdego typu (`eqInt`, `eqInteger`, `eqTile`, `eqCoord`) możemy wszędzie używać `(==)`

### Algorytmy generyczne

Rozważmy funkcję

```haskell
moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo c1 c2 c | c1 == c   = c2
                   | otherwise = c
```

Nie ma w niej (prawie) nic specyficznego dla `Coord`. Funkcja ta może działać dla dowolnego typu z równością:

```haskell
moveFromTo :: Eq a => a -> a -> a -> a
moveFromTo c1 c2 c | c1 == c   = c2
                   | otherwise = c
```

Ta funkcja nie jest może imponująca, ale mechanizm klas pozwala na znacznie bardziej skomplikowane konstrukcje.

### Rozwiązywanie instancji

Gdy używamy funkcji przeciążonej, odnalezienie właściwej instancji (implementacji metod) jest zadaniem kompilatora.
Jak zobaczymy, w obecności polimorfizmu może to być nietrywialne (wymagać odnalezienia innych instancji i tak dalej).
W sumie kompilator Haskella zawiera w sobie mini-Prolog.

## Przykład: Undo

Powiedzmy, że chcemy dodać do gry możliwość wycofania ruchu (np. przy dojściu z pudłem do ściany).

```haskell
data WithUndo a = WithUndo a [a]

withUndo :: Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []          -> WithUndo s []
    handle' e              (WithUndo s stack)
       = WithUndo (handle e s) (s:stack)
    draw' (WithUndo s _) = draw s
```

Co jest źle z tym kodem?

Wskazówka:

![Events! There's just too many of them](https://i.imgflip.com/1yu2i3.jpg)

Powinniśmy wkładać na stos tylko zdarzenia, które mają efekt:

```haskell
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
```

Teraz jednak potykamy się o
```
No instance for (Eq a) arising from a use of ‘==’
```

nasza funkcja nie działa dla wszystkich typów stanu, ale tylko tych z równością:

```haskell
withUndo :: Eq a => Activity a -> Activity (WithUndo a)
```

teraz mamy inny problem - brak równości dla typu `State`:

```
No instance for (Eq State) arising from a use of ‘withUndo’
```

:pencil: Zdefiniuj wszystkie potrzebne instancje `Eq` (być może przy pomocy deriving) i uruchom kod używający `withUndo`.

## Klasa Show
Klasa `Show` zawiera funkcje dające tekstową reprezentację wartości. W skrócie

``` haskell
class Show a where
  show :: a -> String
  ...
```

Przykład: wyrażenia
``` haskell
  data Exp = N Int | Add Exp Exp | Mul Exp Exp

  f :: Exp -> String
  f (Add e1 e2) = show e1 ++ " + " ++ show e2
```

Konkatenacja kosztuje, ale można to zrobić sprytniej używając kontynuacji
``` haskell
  -- type ShowS = String -> String
  g :: Exp -> ShowS
  g (Add e1 e2) =
    shows e1 . showString " + " . shows e2
  f = g ""
```
Pełniejsza definicja klasy `Show`
``` haskell
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

shows = showsPrec 0
```
Metoda `showsPrec` słuzy do inteligentniejszego wypisywania wyrażeń z priorytetami operatorów, np

``` haskell
infixl 6 :+
infixl 7 :*
-- showParen :: Bool -> ShowS -> ShowS

instance Show Exp where
 showsPrec p (x :+ y) =
   showParen(p>6)(showsPrec 6 x.(" + "++).showsPrec 7 y)
 showsPrec p (x :* y) =
   showParen(p>7)(showsPrec 7 x.(" * "++).showsPrec 8 y)
 showsPrec p (N n) = showParen (p>10) (showsPrec 11 n)

exp1 = (N 2 :+ N 3) :* N 4
exp2 = N 2 :+ N 3 :+ N 4

-- > exp1
-- (2 + 3) * 4
-- > exp2
-- 2 + 3 + 4
```

## Klasa Read

## Klasa IsString

``` haskell
-- | Class for string-like datastructures; used by the overloaded string
--   extension (-XOverloadedStrings in GHC).
class IsString a where
    fromString :: String -> a
```

Przykład użycia:

``` haskell
import CodeWorld
import Data.String

picture :: (Show a) => a -> Picture
picture = lettering . fromString . show
main = drawingOf (picture 42)
```

## Inne ważne klasy

```haskell
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

        -- Minimal complete definition:
        --      (<=) or compare
        -- Using compare can be more efficient for complex types.
    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y
         | x <= y    =  y
         | otherwise =  x
    min x y
         | x <= y    =  x
         | otherwise =  y

-- Enumeration and Bounded classes


class  Enum a  where
    succ, pred       :: a -> a
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

        -- Minimal complete definition:
        --      toEnum, fromEnum
--
-- NOTE: these default methods only make sense for types
-- 	 that map injectively into Int using fromEnum
--	 and toEnum.
    succ             =  toEnum . (+1) . fromEnum
    pred             =  toEnum . (subtract 1) . fromEnum
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]
    enumFromThenTo x y z =
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]


class  Bounded a  where
    minBound         :: a
    maxBound         :: a

-- Numeric classes


class  (Eq a, Show a) => Num a  where
    (+), (-), (*)    :: a -> a -> a
    negate           :: a -> a
    abs, signum      :: a -> a
    fromInteger      :: Integer -> a

        -- Minimal complete definition:
        --      All, except negate or (-)
    x - y            =  x + negate y
    negate x         =  0 - x

    class  (Real a, Enum a) => Integral a  where
    quot, rem        :: a -> a -> a
    div, mod         :: a -> a -> a
    quotRem, divMod  :: a -> a -> (a,a)
    toInteger        :: a -> Integer

        -- Minimal complete definition:
        --      quotRem, toInteger
    n `quot` d       =  q  where (q,r) = quotRem n d
    n `rem` d        =  r  where (q,r) = quotRem n d
    n `div` d        =  q  where (q,r) = divMod n d
    n `mod` d        =  r  where (q,r) = divMod n d
    divMod n d       =  if signum r == - signum d then (q-1, r+d) else qr
                        where qr@(q,r) = quotRem n d
```

# Zadanie: Sokoban 4

https://classroom.github.com/a/TpQtaObY

Termin:
17.11.2021 18:00 UTC+1

## Etap 1

Stwórz kilka (prostych) poziomów. Mozna pomóc sobie np http://sokobano.de/wiki i http://www.sneezingtiger.com/sokoban/levels.html

```haskell
data Maze = Maze Coord (Coord -> Tile)
mazes :: [Maze]
mazes = …
badMazes :: [Maze]
badMazes = …
```

`mazes` powinno zawierać "dobre" poziomy, `badMazes` - nierozwiązywalne (np. miejsce docelowe całkowicie otoczone ścianami)

Aby szybciej uzyskać większą liczbe poziomów, mozesz też wymienić się poziomami z innymi bądź dodać swoje poziomy jako pull request.

## Etap 2 - funkcje polimorficzne

Zdefiniuj kilka funkcji na listach (niektóre być może zostały zdefiniowane już wcześniej).

```
elemList :: Eq a => a -> [a] -> Bool
appendList :: [a] -> [a] -> [a]
listLength :: [a] -> Integer
filterList :: (a -> Bool) -> [a] -> [a]
nth :: [a] -> Integer -> a
mapList :: (a -> b) -> [a] -> [b]
andList :: [Bool] -> Bool
allList :: (a-> Bool) -> [a] -> Bool
foldList :: (a -> b -> b) -> b -> [a] -> b
```
Bonus: wyraź pozostałe funkcje przy użyciu `foldList`

## Etap 3 - wyszukiwanie w grafie

Zaimplementuj funkcję

```haskell
isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = ...
```
gdzie parametry mają następujące znaczenie:

* `initial` - wierzchołek początkowy
* `neighbours` - funkcja dająca listę sąsiadów danego wierzchołka
* `isOk` - predykat mówiący, czy wierzchołek jest dobry (cokolwiek to znaczy).

Funkcja `isGraphClosed` ma dawać wynik `True` wtw wszystkie wierzchołki osiągalne z początkowego są dobre.
Należy pamiętać, ze graf może mieć cykle.

Napisz funkcję
```haskell
reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = ...
```

dającą `True` wtw gdy wierzchołek `v` jest osiągalny z wierzchołka `initial`

Napisz funkcję
```haskell
allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = ...
```

dajacą `True` wtw gdy wszystkie wierzchołki z listy `vs` są osiagalne z `initial`. W tej funkcji nie używaj rekurencji, a tylko innych funkcji zdefiniowanych wcześniej.

## Etap 4 - sprawdzanie poziomów

Korzystając z funkcji z poprzedniego etapu, zaimplementuj funkcje

```haskell
isClosed :: Maze -> Bool
isSane :: Maze -> Bool
```

* `isClosed` - pozycja startowa `Ground` lub `Storage`, żadna osiągalna (z pozycji startowej) nie jest `Blank`
* `isSane` - liczba osiągalnych `Storage` jest niemniejsza od liczby osiągalnych skrzyń.

Sprawdź, które poziomy z list `mazes` oraz `badMazes` sa zamknięte i rozsądne. Do wizualizacji mozna uzyć następującej funkcji

```haskell
pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

main :: IO()
main = drawingOf(pictureOfBools (map even [1..49::Int]))
```

Zdefiniuj `etap4 :: Picture`  jako wizualizację wyników dla wszystkich poziomów. Użyj tej wizualizacji jako ekranu startowego w kolejnym etapie.

## Etap 5 - wielopoziomowy Sokoban

Przerób funkcje wyszukujące skrzynie i `isWinning` z poprzedniego etapu tak aby używały osiągalnych skrzyń.
Odpowiednio przerób funkcję rysującą - w ten sposób będzie mozna rysować poziomy różnych rozmiarów.

Przerób swoją grę z poprzedniego zadania tak aby gra składała się z kolejnych poziomów z listy `mazes`, rozdzielonych ekranami 'Poziom ukończony, liczba ruchów: N'

```haskell
etap5 :: IO()
main = etap5
```

`etap5` powinien używać także  `withUndo`, `withStartScreen` oraz `resettable`.

Dodaj możliwość przejścia do nastepnego poziomu po naciśnięciu `N`


