# Funkcje wyższego rzędu

Zadanie z poprzedniego tygodnia można by rozwiązać np. tak:

```haskell
pictureOfMaze :: Picture
pictureOfMaze = drawRows (-10)

drawRows :: Integer -> Picture
drawRows 11 = blank
drawRows r  = drawCols r (-10) & drawRows (r+1)

drawCols :: Integer -> Integer -> Picture
drawCols _ 11 = blank
drawCols r c = drawTileAt r c & drawCols r (c+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt = …
```

Zauważmy, że `drawRows` i `drawCols ` są podobne: robia 21 razy coś zależnego od licznika. Różnice:

* `coś` jest różne - dla `drawRows` jest to `drawCols`, z kolei dla `drawCols` jest to `drawTileAt`;
* w `drawCols` funkcja `coś` bierze dodatkowy argument.

Spróbujmy uogólnić ten wzorzec:

```
draw21times something = helper something (-10)

helper something 11 = blank
helper something n  = something & helper something (n+1)

pictureOfMaze = draw21times drawRow
drawRow = draw21times drawCol
drawCol = drawTileAt ? ?
```

Mamy tu drobny problem: w definicji `drawCol` musimy znać numer wiersza i kolumny.
Możemy poczuć pokusę aby użyć `n`, ale nie jest ono w zasięgu (jest lokalne w fukcji helper).

Zatem `helper` musi przekazać bieżące `n` do `something`. Podobnie `drawRow` musi poinformować `drawRow`, w którym wierszu ma rysować:

```haskell
draw21times something = helper something (-10)

helper something 11 = blank
helper something n  = something n & helper something (n+1)

pictureOfMaze = draw21times drawRow
drawRow r = draw21times (drawCol r)
drawCol r c = drawTileAt r c
```

Pomocne w zrozumieniu powyższego kodu mogą być sygnatury typów:

```haskell
draw21times :: (Integer -> Picture) -> Picture
helper :: (Integer -> Picture) -> Integer -> Picture
drawRow :: Integer -> Picture
drawCol :: Integer -> Integer -> Picture
```

(NB najbardziej ogólny typ `draw21times` jest bardziej skomplikowany: 
`forall a. (Eq a, Num a) => (a -> Picture) -> Picture` - wrócimy do tego później.)

Zauważmy, że typ `draw21Times` zawiera dwie strzałki, ale finkcja nie bierze dwóch argumentów, ale jeden, będący funkcją.
Jest to przykład tzw. *funkcji wyższego rzędu* - to ważne pojęcie w programowaniu funkcyjnym i jeden z głównych mechanizmów abstrakcji.

:pencil: przerób funkcję `draw21times` na `drawNtimes` tak aby liczba powtórzeń była argumentem i aby odliczać w dół do 0.

## Częściowa aplikacja

Zauważmy, że funkcja `drawCol` potrzebuje dwóch argumentów, ale używamy jej z jednym iprzekazujemy do `draw21times`. 
Typ `drawCol` możemy zapisać jako

```haskell
drawCol :: Integer -> (Integer -> Picture)
```

Czyli w istocie jest to funkcja jednoargumentowa o typie wyniku pasującym do typu argumentu `draw21times` (strzałka wiąze w prawo, więc nawiasy są tylko dla ilustracji).  Mechanizm zastosowania funkcji do niepełnej liczby argumentów nazywamy *cześciową aplikacją*  i jest to również ważny element budowania abstrakcji w programowaniu funkcyjnym (często w połacznieu z funkcjami wyższego rzędu).

# Typy danych

Przypomnijmy sobie funkcję `drawTile :: Integer -> Picture` tworzącą obraz pola na podstawie numeru jego rodzaju.
Kod byłby czytelniejszy i mniej podatny na błędy, gdyby zamiast numerów stosowac nazwy symboliczne.
W innych językach stosujemy konstrukcje takie jak `#define` albo `enum`; w Haskellu zaś konstrukcję `data`:

```
data Tile = Wall | Ground | Storage | Box | Blank
```

W ogólności `data` daje o wiele większe możliwosci, ale w swojej najprostszej postaci pozwala zdeinifować typ poprzez wyliczenie 
konstruktorów jego wartości. Wartościami typu `Tile` są dokładnie te wyliczone konstruktory; nie ma problemu jak funkcja `drawTile ` ma zachowac się np. dla wartości `-1`.

NB nazwy konstruktorów powinny zaczynać się od wielkiej litery (bądź dwukropka dla nazw infiksowych, złożonych z symboli)

Rozpoznawanie konstruktorów odbywa się zwykle przez dopasowanie wzorca, 
np. [(otwórz w CodeWorld)](https://code.world/haskell#P-M5f3eyKkHqrbfW2KObbKQ)

```haskell
drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
 ```
 
 Zauważmy, ze teraz również sygnatury typów stają się bardziej czytelne i pomocne.
 
## Bool

Z jednym wyliczeniowym typem danych juz sie zetknęliśmy: `Bool`. Nie jest on "magiczny", ale zdefiniowany jako

```haskell
data Bool = False | True
```

Podobnie operatory takie jak `(&&)` nie są wbudowane, ale każdy mógłby je zdefiniować (spróbuj!).

## Więcej typów dla gry Sokoban

Naszym celem jest rozszerzenie animacji o interakcję z użytkownikiem. Zacznijmy od potrzebnych typów.

Piewszą rzecza, którą moglibysmy chcieć zrobic jest przesuwanie planszy (np. gdy jest większa niż nasze okno).
Potem moze chcielibysmy przesuwać gracza po planszy. Potrzebujemy typu reprezentującego kierunki:

```haskell
data Direction = R | U | L | D
```

Potrzebujemy też typu reprezentującego pozycję. Tutaj typ wyliczeniowy juz nie wystarczy; 
musimy też przechowywać wartości współrzędnych. Mozemy to osiągnąc przez konstruktory z parametrami, np.

```haskell
data Coord = C Integer Integer
```

(moglibyśmy użyć też pary `(Integer, Integer)`, ale dedykowane typy dają lepsze komunikaty o błędach).

Konstruktor `C` (poza tym, ze może wystapić we wzorcach) zachowuje się jak funkcja typu 
`Integer -> Integer -> Coord`, oto przykład:

```haskell
initialCoord :: Coord
initialCoord = C 0 0
```

Wyłuskiwanie składowych typu `Coord` możemy uzyskać  przy pomocy dopasaowania wzorca.
Na przykład mozemy potrzebowac funkcji przesuwającej obraz o podane współrzędne:

```haskell
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic
```

`translated` bierze argumenty typu `Double`, dlatego musimy uzyć `fromIntegral`.

:pencil:

Napisz funkcję `adjacentCoord :: Direction -> Coord -> Coord` dającą współrzędne przesuniete o 1 w podanym kierunku.

Mozesz ją przetestowac w `ghci`. Aby móc wypisywac elementy swojego typu, warto dodać do jego definicji klauzulę 
`deriving Show`, np.

```haskell
data Coord = C Integer Integer deriving Show
```

Jesli chcesz skłonic CodeWorld aby coś wypisał możesz użyć funkcji `print` w `main`, np.

```haskell
main = print 42
```

albo (jesli zdefiniowałeś `Coord` z klauzulą `deriving Show`)

```haskell
main = print (C 1 2)
```

Innym sposobem przetestowania jest rysowanie naszego poziomu w różnych miejscach, np.

```haskell
someCoord :: Coord
someCoord = adjacentCoord U (adjacentCoord U (adjacentCoord L initialCoord))

main = drawingOf (atCoord someCoord pictureOfMaze)
```

:pencil: napisz funkcję `moveCoords :: [Coord] -> Coord -> Coord` taką aby powyższy przykład dało się zapisać krócej jako

```haskell
someCoord = moveCoords [U, U, L] initialCoord
```
