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

Wiele osób w rozwiazaniu ostatniego zadania pisało kod postaci

```haskell
if comp (lvl c) Box then Ground else  (lvl c)
```

prawdopodobnie chcieli napisać

```haskell
if  (lvl c) == Box then Ground else  (lvl c)
```

ale natknęli się na komunikat

```
No instance for (Eq Tile) arising from a use of ‘==’
```

Otóż równość ma typ

```haskell
(==) :: forall a.Eq a => a -> a -> Bool
```

co nalezy rozumieć jakoa `a -> a -> Bool` dla wszystkich typów `a` należących do klasy `Eq`.
Warto zauważyć, że nie jest to polimorfizm parametryczny: równość nie działa dla wszystkich typów tak samo (czasami mówi się w tym wypadku o polimorfiźmie *ad hoc*

Klasę należy tu rozumiec jako zbiór typów (dokładniej relację na typach, w tym wypadku jednoargumentową).

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

Ponieważ każdą mozna łatwo wyrazić przez negację drugiej, wystarczy podać jedną z nich.

Metody dla standardowych typów są zdefiniowane w Prelude (bibliotece standardowej, zawsze domyślnie importowanej).
Czasami te implementacje są warunkowe: np. równosc na parach jest definiowana pod warunkiem istnienia równosci na argumentach.


### instance Eq Coord


```haskell
data Coord = C Integer Integer
instance Eq Coord where
  C x y == C x' y' = x == x' && y == y'
```

### Domyślne implementacje

Klasa `Eq` ma dwie metody: `(==))` i `(/=)`. Ponieważ każdą mozna łatwo wyrazić przez negację drugiej, wystarczy podać jedną z nich.

Definiując klasę mozemy podac domyślną implementację 

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

Definicja równości byłaby równie nudna; na szczęście Haskell potrafi wygenerowac takie nudne definicje klas standardowych automatycznie:

```haskell
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
```

albo jeśli chcemy więcej niz jedną klasę

```haskell
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)
```

### Równość (nie) dla wszystkich

```haskell
data Interaction world = Interaction
        world
	(Double -> world -> world)
	(Event -> world -> world)
	(world -> Picture)
    deriving Eq
 ```
 
Niestety jako, że równość na funkcjach jest w ogólnosci nierozstrzygalna, nie uda nam się zdefiniować jej np. dla typu `Interaction`

```
error:
    • No instance for (Eq (world -> Picture))
        arising from the fourth field of ‘Interaction’
          (type ‘world -> Picture’)
```

Oczywiście możemy zdefiniować funkcję, która nie będzie prawdziwą równoscia, np

```haskell
instance Eq Interaction where
  _ == _ = False
```

Niekoniecznie jest to jednak dobry pomysł; zwykle zakładamy, że równośc ma pewne własności, np. że jest co najmniej relacją równoważnosci.

## Zalety klas

## Przykład: Undo

Powiedzmy, ze chcemy dodac do gry możliwośc wycofania ruchu (np. przy dojściu z pudłem do ściany).

```haskell
data WithUndo a = WithUndo a (List a)

withUndo :: Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = WithUndo state0 Empty

    step' t (WithUndo s stack) = WithUndo (step t s) stack

    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of Entry s' stack' -> WithUndo s' stack'
                      Empty           -> WithUndo s Empty
    handle' e              (WithUndo s stack)
       = WithUndo (handle e s) (Entry s stack)

    draw' (WithUndo s _) = draw s
```

Co jest źle z tym kodem?

Wskazówka:

![Events! There's just too many of them](https://i.imgflip.com/1yu2i3.jpg)

Powinniśmy wkładać na stos tylko zdarzenia, które mają efekt:

```haskell
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of Entry s' stack' -> WithUndo s' stack'
                      Empty           -> WithUndo s Empty
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (Entry s stack)
      where s' = handle e s
```

Teraz jednak potykamy się o
```
No instance for (Eq a) arising from a use of ‘==’
```

nasza funkcja nie działa dla wszystkich typów stanu, ale tylko tych z równością:

```haskell
withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
```

teraz mamy inny problem - brak równosci dla typu `State`:

```
No instance for (Eq State) arising from a use of ‘withUndo’
```

:pencil: Zdefiniuj wszystkie potrzebne instancje `Eq` (być może przy pomocy deriving) i uruchom kod używający `withUndo`.

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
## Zadanie: Sokoban 4
