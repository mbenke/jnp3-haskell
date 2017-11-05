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

## Inne ważne klasy

## Zadanie: Sokoban 4
