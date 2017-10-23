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

:pencil: Przerób swój kod tak aby używał nowego typu `Tile` zamiast kodowania typów pól liczbami.

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
## Terminologia

* typ, w którym żaden z konstruktorów nie ma argumentów nazywamy *typem wyliczeniowym* (ang. *enumeration type*);
* typ o jednym konstruktorze nazywamy *typem produktowym* (ang. *product type*) - jest izomorficzny z produktem argumentów konstruktora, np `C Integer Integer ~ (Integer, Integer)`
* typ o więcej niż jednym konstruktorze nazywamy *typem sumarycznym* (ang. *sum type*) jest izomorficzny z sumą rozłączną odpowiednich typów
* typ bez konstruktorów (tak, to możliwe i czasem użyteczne!) nazywamy *typem pustym* (ang. *empty type*, nie mylić z `()`)

# Czysta interakcja

Pora uczynić naszą grę interaktywną. Chcemy aby po uruchomieniu programu, poziom był wysrodkowany,
następnie aby użytkownik mógł przesuwać go przy pomocy klawiszy strzałek.

Jak mozemy modelować interakcję w świecie bez efektów ubocznych? Podobnie jak to uczyniliśmy dla animacji:
animacja jest funkcją z czasu w obraz. Program reagujący na zdarzenia możemy przedstawić jako funkcję,
która mając bieżący stan i zdarzenie, oblicza nowy stan:

```haskell
interactionOf :: world ->
                (Double -> world -> world) ->
                (Event -> world -> world) ->
                (world -> Picture) ->
                IO ()
```

Występujacy w tym typie typ świata `world` jest zmienną typową - mozemy uzyć w jej miejsce dowolnego typu (szerzej powiemy sobie o tym później. Jeżeli chcemy tyliko przesuwać poziom, na początek mozemy użyć `Coord`.

Funkcja `interactionOf` bierze 4 argumenty:

1. Początkowy stan typu `world`.
2. Funkcję opisującą zmiany stanu z upływem czasu, typu `Double -> world -> world`.
3. Funkcję opisującą zmiany stanu w reakcji na zdarzenia, typu `Event -> world -> world`.
4. Funkcję przedstawiającą stan jako obraz.

Takie podejście jest zbliżony do paradygmatu Model-View-Controller, ale nie używa efektów ubocznych, a tylko czystych funkcji.

Prosta próba użycia `interactionOf` moze wyglądać np. tak [(zobacz na CodeWorld)](https://code.world/haskell#PpjfIR2NrgPeBJQKfg_63Kg):

```haskell
main = interactionOf initialCoord handleTime handleEvent drawState

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent e c = adjacentCoord U c

drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze
```

To ...coś robi. Ale gdy tylko najedziemy muszą na obraz, on ucieka ... Dlaczego? Przy każdym zdarzeniu obraz przesuwa się do góry. A ruchy myszy też są zdarzeniami.

## Zdarzenia

Przyjrzyjmy się zatem typowi zdarzeń `Event`. Według [dokumentacji](https://code.world/doc-haskell/CodeWorld.html#t:Event) jest on zdefiniowany  jako `data`, mniej więcej tak:

```haskell
data Event = KeyPress Text
           | KeyRelease Text
           | MousePress MouseButton Point
           | MouseRelease MouseButton Point
           | MouseMovement Point
```

W tym momencie interesują nas zdarzenia `KeyPress`. Spróbujmy je obsłużyć:

```haskell
handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up"    = adjacentCoord U c
    | key == "Left"  = adjacentCoord L c
    | key == "Down"  = adjacentCoord D c
handleEvent _ c      = c
```

:exclamation: **Uwaga**: Aby używać stałych typu `Text` musimy w pierwszej linii programu dodać zaklęcie (pragmę)

```haskell
{-# LANGUAGE OverloadedStrings #-} 
```

Sekwencja `{- ... -}` oznacza komentarz blokowy. Sekwencja  `{-# ... #-}` oznacza pragmę, czyli wskazówkę ddla kompilatora.
W tym wypadku pragma `LANGUAGE OverloadedStrings` oznacza rozszerzenie języka, w którym literały napisowe są przeciążone i (podobnie jak literały liczbowe) dopasowują się do oczekiwanego typu - domyślnie są typu `String`, ale tutaj chcemy ich użyc w typie `Text`.

# :pencil: Sokoban 2

## Etap 1: ruchomy gracz

Stwórz definicję `player1 :: Picture` reprezentującą figurkę gracza.

Zdefiniuj `walk1 :: IO ()` wykorzystujące `interactionOf` aby:
* postać gracza była rysowana na obrazie poziomu 
* początkowa pozycja gracza wypadała na pustym polu (można uzyć ustalonych współrzędnych, nie trzeba szukać pustego pola w programie)
* klawisze strzełek przesuwały obraz gracza (obraz poziomu ma pozostac nieruchomy)
* gracz przesuwał się tylko  na pola `Ground` lub `Storage` (nie wchodzimy na ściany ani pudła).

Zwróć uwagę na kolejnosc elementów w `&` bądź `pictures`:

```haskell
design, square, circ :: Picture
design =  pictures [square, circ]
circ = colored red (solidCircle 1)
square = colored black (solidRectangle 1 1)
```

## Etap 2: gracz skierowany

Chcemy aby postać gracza patrzyła w stronę, w którą się porusza (co najmniej lewo-prawo). Zdefiniuj funkcję `player2 :: Direction -> Picture` dającą figurkę gracza skierowaną w odpowiednią stronę.

Rozszerz kod z Etapu 1, definiując `walk2 :: IO()` tak, aby figurka gracze była wyświetlana odpowiednio do kierunków ruchu.

:point_right: **Wskazówka:** pomyśl najpierw o typach (np. stanu świata), potem o implementacji.

:exclamation: **Uwaga:** upewnij się, że po Twoich modyfikacjach funkcja `walk1` nadal działa.

## Etap3: reset

W trakcie gry przydatna będzie mozliwość rozpoczęcia poziomu od początku. 
Ta funkcjonalność jest w gruncie rzeczy niezależna od gry, zatem zaimplemntujmy ją ogólnie. Napisz funkcję

```haskell
resettableInteractionOf ::
    world ->
    (Double -> world -> world) ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
```

która zasadniczo będzie działać jak `interactionOf`, ale dla zdarzenie odpowiadające naciśnięciu klawisza `Esc` nie jest przekazywane dalej, ale powoduje powrót stanu gry do stanu początkowego.

Zastanów się co powinno się dziać dla zdarzenia odpowiadającego puszczeniu klawisza `Esc` i opisz swój wybór w komentarzu.

Zdefiniuj `walk3 :: IO ()` jako wariant `walk2` używający `resettableInteractionOf`.

Termin: 28.10.2017 06:00 UTC+0200 (0400 UTC)

Oddawanie przez GitHub Classroom: https://classroom.github.com/a/z2X-Jbdu
