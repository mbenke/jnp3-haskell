# Zadanie: Sokoban 5 (ASCII/ANSI)

W ramach tego zadania należy zmodyfikować kod zadania **Sokoban 4** tak, aby zamiast środowiska CodeWorld używało środowiska tekstowego.
Stworzenie takiego srodowiska w oparciu o standardowe funkcje tekstowego I/O jest również przedmiotem zadania.
Frazę `import codeWorld` należy usunąć.

Elementami rozwiązania powinny być co najmniej:

* Zmiana typu `Interaction` na odpowiedni dla wyjscia tekstowego
```haskell
data Interaction world = Interaction
    world
    (Event -> world -> world)
    (world -> Screen)
    
data Event = KeyPress String
type Screen = String
```
Typ `Screen` reprezentuje obraz terminala i powinien zawierać np 23 linie po `<80` znaków, zakończone `\n`

* Przeróbka funkcji `drawState` tak, aby produkowała wyjście tekstowe. Poszczególne elementy należy reprezentowac jako odpowiednie znaki ASCII, 
np. zgodnie z http://www.sokobano.de/wiki/index.php?title=Level_format - w każdym wypadku użyte znaki należy opisać w README.
Wachlarz możliwości jest tu szeroki - można stworzyć wszystko od zera, albo próbować odtworzyć pewien zakres API CodeWorld, np. definiując

```haskell
type Picture = Integer -> Integer -> Maybe Char
```

Inną, potencjalnie wydajniejszą i bliższą ideom programowania funkcyjnego reprezentacją (lepiej punktowaną) jest

```
type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun
blank = id
(&) = (.)
```

NB funkcja `translated` przy takiej reprezentacji będzie wymagała dwukrotnej zmiany współrzędnych (podobnie do sprzężenia w algebrze liniowej).

* Stworzenie funkcji 

```
runInteraction :: Interaction s -> IO ()
```
W pierwszej kolejności należy wyłączyć buforowanie tak, aby informacja o naciśniętych klawiszach trafiała do programu natychmiast 
a nie dopiero po `Enter`:

```haskell
hSetBuffering stdin NoBuffering
```

Czyszczenie ekranu można zrealizować przy pomocy [kodu ANSI](https://en.wikipedia.org/wiki/ANSI_escape_code):
```haskell
putStr "\ESCc"
```

Narysuj bieżący stan, dalej interakcja w rekurencyjnej funkcji `go` (lokalnej dla `runInteraction`):
1. wczytaj znak (używając `getChar`)
2. przekaż do funkcji obsługi zdarzeń (ale patrz uwaga o strzałkach poniżej), która da kolejny stan
3. wyczyść ekran
4. odwzoruj stan (używając funkcji renderującej w `Interaction` oraz `putStr`)
5. wywołaj rekurencyjnie `go`
    
Do obsługi kierunków mozna uzyć klawiszy WASD (prostsze) albo strzałek (wymaga obsługi wieloznakowych sekwencji ANSI, wyżej punktowane).
Strzałka w prawo generuje sekwencję `"\ESC[C"` (trzy znaki). Można to sprawdzić w `ghci` używając `getLine`.
Aby obsługiwać sekwencje wieloznakowe, należy:
1. Wczytać strumień wejściowy przy użyciu `getContents`
2. Sprawdzić przez dopasowanie wzorca jaki jest kolejny znak
3. Jesli to `'\ESC'` przejśc do funkcji analizującej dalszą część sekwencji
4. Uwaga na sekwencje `'\ESC':c:_` z c innym niż `'['`

* Możliwe rozszerzenie: kolory przy użyciu [sekwencji ANSI dla kolorów](https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
