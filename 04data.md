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

