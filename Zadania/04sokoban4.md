# Sokoban 4

## Etap 1

Stwórz kilka poziomów. Można pomóc sobie http://sokobano.de/wiki

```haskell
data Maze = Maze Coord (Coord -> Tile)
mazes :: [Maze]
mazes = …
badMazes :: [Maze]
badMazes = …
```

`mazes` powinno zawierać "dobre" poziomy, `badMazes` - nierozwiązywalne (np. miejsce docelowe całkowicie otoczone ścianami)

Aby szybciej uzyskać większą liczbę poziomów, możesz też wymienić się poziomami z innymi bądź dodać swoje poziomy jako pull request.

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

dającą `True` wtw gdy wszystkie wierzchołki z listy `vs` są osiągalne z `initial`. W tej funkcji nie używaj rekurencji, a tylko innych funkcji zdefiniowanych wcześniej.

## Etap 4 - sprawdzanie poziomów

Korzystając z funkcji z poprzedniego etapu, zaimplementuj funkcje

```haskell
isClosed :: Maze -> Bool
isSane :: Maze -> Bool
```

* `isClosed` - pozycja startowa `Ground` lub `Storage`, żadna osiągalna (z pozycji startowej) nie jest `Blank`
* `isSane` - liczba osiągalnych `Storage` jest niemniejsza od liczby osiągalnych skrzyń.

Sprawdź, które poziomy z list `mazes` oraz `badMazes` są zamknięte i rozsądne. Do wizualizacji można użyć następującej funkcji

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

## Etap 5 - wieleopoziomowy Sokoban

Przerób funkcje wyszukujące skrzynie i `isWinning` z poprzedniego etapu tak aby używały osiągalnych skrzyń.
Odpowiednio przerób funkcję rysującą - w ten sposób będzie można rysować poziomy różnych rozmiarów.

Przerób swoją grę z poprzedniego zadania tak aby gra składała się z kolejnych poziomów z listy `mazes`, rozdzielonych ekranami 'Poziom ukończony, liczba ruchów: N'

```haskell
etap5 :: IO()
main = etap5
```

`etap5` powinien używać także  `withUndo`, `withStartScreen` oraz `resettable`.


