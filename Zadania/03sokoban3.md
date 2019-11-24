# Sokoban 3

**Cel zadania:** pełny Sokoban

# Etap 1: typ stanu

Zdefiniuj typ stanu gry `State`, przechowujący
* pozycję gracza
* kierunek patrzenia
* pozycje wszystkich skrzyń na planszy (jako listę)

# Etap 2: stan początkowy

* wybrana z góry pozycja (jak poprzednio)
* wybrany z góry kierunek
* pozycje skrzyń odczytane z definicji poziomu

Ostatni punkt jest nieco kontrowersyjny, ale możemy założyć, że poziom, jak poprzednio mieści się w zakresie współrzędnych `[-10..10]`.

Obliczoną wartość `initialBoxes` mozna przetestować np. tak:

```haskell
main = drawingOf (pictureOfBoxes initialBoxes)
```

# Etap 3: przesuwanie skrzyń

Dotąd nasza definicja `maze` zawierała skrzynie na początkowych pozycjach. Teraz oddzieliliśmy pozycje skrzyń.

Zdefiniuj funkcję

```haskell
removeBoxes :: (Coord -> Tile) -> Coord -> Tile
```

która poziom  będący argumentem przekształca w ten sposób, że tam gdzie ten dałby `Box`, wynikowy daje `Ground`

Zdefiniuj `addBoxes` dodające skrzynie na podanych pozycjach:

```
type Maze = Coord -> Tile
addBoxes :: [Coord] -> Maze -> Maze
```


**Bonus:** złożenie funkcji jest reprezentowane w Haskellu przez operator `.`:

```haskell
(:) :: (b->c) -> (a->b) -> (a->c)
(f . g) x = f(g x)
```

Uzupełnij

```haskell
removeBoxes maze = f . maze where f = -- ...
```

# Etap 4: rysowanie stanu

Zdefiniuj
```haskell
draw :: State -> Picture
```

# Etap 5: obsługa zdarzeń

Zdefiniuj
```haskell
handleEvent :: Event -> State -> State
```

Reaguj tylko na klawisze strzałek oznaczające ruchy. Ruch moze się powieść lub nie.
Ruch jest udany jeśli w kierunku ruchu jest `Ground`, `Storage` lub `Box` - w tym ostatnim wypadku jesli za skrzynią jest dostępne miejsce. Udany ruch skutkuje odpowiednią zmianą stanu.

Do obsługi listy skrzyń moze być pomocna standardowa funkcja

```haskell
map :: (a->b) -> [a] -> [b]
```

# Etap 6: integracja

Zdefiniuj interakcję typu `Activity State` w oparciu o powyższe funkcje. Opakuj ją w `resettable` i `withStartScreen`

# Etap 7: profit

Ostatni brakujący element to warunki wygranej. Zdefiniuj funkcję

```haskell
isWinning :: State -> Bool
```

dającą `True` gdy wszystkie skrzynie są na polach `Storage`. Może tu się przydać funkcja pomocnicza 
```haskell
allList :: [Bool] -> Bool
```
Użyj tej funkcji w `draw` (komunikat o wygranej) oraz `handleEvent` (inna obsługa zdarzeń po wygranej).
