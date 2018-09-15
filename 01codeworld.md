# ELI5 

*Explain Like I'm 5* - czyli wyjaśnij mi to jak pięciolatkowi [1].

Haskell ma opinię języka trudnego i skomplikowanego, dlatego zaczniemy od kursu, nawet jeśli nie dla pięciolatków, 
to dla uczniów szkoły podstawowej (testowane z powodzeniem na 9-latkach).

[1] https://www.reddit.com/r/explainlikeimfive/

# Code World

https://code.world/

CodeWorld jest edukacyjnym środowiskiem programistycznym dostępnym w przeglądarce internetowej.
Przy użyciu prostego modelu matematycznego dla figur i przekształceń, pozwala tworzyć rysunki, animacje, a nawet gry.

# Rysowanie

```haskell
program = drawingOf(codeWorldLogo)
```

## Definicje

Program jest zbiorem (kolejność nie ma znaczenia) definicji.  Na przykład

```haskell
program = drawingOf(wheel)
wheel   = circle(2)
```

NB to jest kompletny program - wypróbuj go!

 Wykonanie programu w środowisku CodeWorld zaczyna się od definicji `program`
 (w "dorosłym" Haskellu wykonanie zaczyna się od definicji `main` ale zasada jest ta sama).

##  Obrazki

:pencil: Wypróbuj, z różnymi wartościami:

```haskell
circle(8)
circle(0.5)
solidCircle(5)
rectangle(4,8)
solidRectangle(8,4)
lettering("W przedszkolu naszym nie jest źle")
```

:exclamation: Zapisuj rozwiązania ćwiczeń w pliku tekstowym - będzie potrzebny później.

## Łączenie figur

Kombinację figur możemy stworzyć przy użyciu operatora `&`:
```haskell
program = drawingOf(design)
design  = solidRectangle(4, 0.4)
          & solidCircle(1.2)
          & circle(2)
```
Czasem warto przy tym nazwać części:

```haskell
program = drawingOf(design)
design  = slot & middle & outside
slot    = solidRectangle(4, 0.4)
middle  = solidCircle(1.2)
outside = circle(2)
```

## Kolory

```haskell
program  = drawingOf(redWheel)
redWheel = colored(wheel, red)
wheel    = solidCircle(4)
```

```haskell
program = drawingOf(tree)
tree    = colored(leaves, green) & colored(trunk, brown)
leaves  = sector(0, 180, 4)
trunk   = solidRectangle(1, 4)
```

Kolory można modyfikować przy pomocy funkcji `dark`, `light`, `translucent`. Wypróbuj je i przeczytaj o nich w dokumentacji.

```haskell
program = drawingOf(overlap)
overlap = colored(square,  translucent(blue))
        & colored(disk, translucent(green))
square  = solidRectangle(5, 5)
disk    = solidCircle(3)
```

## Przekształcenia

### Przesunięcia

`translated(obraz, x, y)` daje obraz przesunięty o `x` w prawo i `y` w górę, np:

```haskell
program = drawingOf(forest)
forest  = translated(tree, -5, 5)
        & translated(tree,  0, 0)
        & translated(tree,  5,-5)
tree    = colored(leaves, green) & colored(trunk, brown)
leaves  = sector(0, 180, 4)
trunk   = solidRectangle(1, 4)
```
:pencil: narysuj sygnalizator drogowy ('światła' - zielone i czerwone kółko wewnątrz prostokąta)

:pencil: narysuj szachownicę (to wymaga pewnego sprytu, za chwilę zobaczymy jak to zrobić sprawniej).

### Obroty

`rotated(obraz, stopni)`

```haskell
program = drawingOf(diamond)
diamond = rotated(square, 45)
square  = solidRectangle(4, 4)
```

### Skalowanie

`scaled(obraz, poziomo, pionowo)`

```
program = drawingOf(oval)
oval    = scaled(base, 2, 0.5)
base    = solidCircle(4)
```

:pencil: Narysuj symbol atomu (koło jako jądro i elipsy jako orbity elektronów).

## Wyrażenia

Z prawej strony definicji (po znaku `=`) umieszczamy *wyrażenie*. Użycie definiowanej nazwy jest równoważne użyciu tego wyrażenia.
Nazywamy to *przejrzystością odwołań* (referential transparency). Wyrażenia mogą być też argumentami funkcji (i obowiązuje tu podobna zasada).

Przykłady wyrażeń:

```
4
2+2
circle(2+2)
colored(lettering("Help"), red)
rectangle(1, 4) & circle(2)
```

Natomiast `x=1` nie jest wyrażeniem ‒ jest definicją.

### Funkcje

Szczególnym rodzajem wyrażeń są *funkcje*. Podstawową operacją którą możemy wykonać przy pomocy funkcji jest zastosowanie jej do argumentów, Na przykład

* `rectangle` jest funkcją. Dostawszy wysokość i szerokość, produkuje obraz (prostokąt)
* `light` jest funkcją. Dostawszy kolor, produkuje (podobny, ale jaśniejszy) kolor. 
* `drawingOf` jest funkcją. Dostawszy  obraz, konstruuje program, który rysuje ten obraz.
* `id` jest funkcją identycznościową

Skoro funkcje są wyrażeniami to czy mogą stać po prawej stronie definicji i być argumentami dla funkcji? Ależ tak:

```haskell
rysuj = drawingOf
id(x) = x
koło = id(circle)
program = rysuj(koło(2))
```

(tak, można używać polskich liter)

:pencil: Narysuj 'gwiazdkę' złożoną z 7 wąskich prostokątów (o wymiarach `(4, 0.2)` lub podobnych).

### Listy

* `[ 1, 2, 3, 4 ]` jest listą liczb,
* `[ circle(2), rectangle(3,5), blank ]` jest listą obrazów.
* `[]` jest listą pustą

Funkcja `picture` buduje obraz złożony ze wszystkich elementów listy podanej jako argument

```haskell
program = drawingOf(allThePictures)
allThePictures = pictures([
    solidRectangle(4, 0.4),
    solidCircle(1.2),
    circle(2)
    ])
```

### Ciągi arytmetyczne

Łatwo domyśleć się, jaką listę oznacza wyrażenie `[1..9]`. Podobnie możemy zapisać inne ciągi arytmetyczne,
na przykład `[1,3..9]`. Trochę więcej myślenia wymaga `[0,2..9]`.


### Wycinanki listowe

```haskell
program = drawingOf(target)
target  = pictures([ circle(r) | r <- [1, 2, 3, 4, 5] ])
```

Wyrażenie `[ circle(r) | r <- [1, 2, 3, 4, 5] ]` nazywamy ‒ nawiązując do aksjomatu wycinania w teorii mnogości ‒ *wycinanką* (list comprehension) ‒ skojarzenie: ![comprehension](https://latex.codecogs.com/gif.latex?%5C%7Bcircle%28r%29%20%5Cmid%20r%5Cin%5C%7B1%2C2%2C3%2C4%2C5%5C%7D%5C%7D).
Wartość tego wyrażenia jest  taka sama jak `[ circle(1), circle(2), circle(3), circle(4), circle(5) ]`.

:question: jak myślisz, co oznacza wyrażenie `[ circle(r) | r <- [1, 2, 3, 4, 5], even r ]`

Możemy również oprzeć wycinankę na kilku listach źródłowych:

```haskell
program = drawingOf(grid)
grid    = pictures([ translated(circle(1/2), x, y)
                     | x <- [-9 .. 9], y <- [-9 .. 9] ])
```
:pencil: Napisz krótszy program rysujący gwiazdkę przy pomocy wycinanki.

## Punkty, linie, łamane, wielokąty

Dla ułatwienia możemy narysować siatkę współrzędnych:

```haskell
program = drawingOf(coordinatePlane)
```

Punkty reprezentowane są jako pary współrzędnych ‒ na przykład `(5,5)`. 
Łamaną mozemy skonstruowac przy pomocy funkcji `path` z lista punktów jako argumentem.


```haskell
program = drawingOf(zigzag)
zigzag  = path([(-2, 0), (-1, 1), (0, -1), (1, 1), (2, 0)])
```

Łamaną zamkniętą możemy uzyskać przy pomocy `polygon`.

:question: Spróbuj bez uruchamiania powiedzieć, co rysuje poniższy kod:

```haskell
program = drawingOf(mystery)
mystery = polygon(
    [(-3, -4), (0, 5), (3, -4), (-4, 2), (4, 2), (-3, -4)])
```

:pencil: Teraz uruchom program. Czy potrafisz narysować to lepiej?

:pencil: Wypróbuj też funkcje `solidPolygon` oraz `thickPolygon`. A co z `thickCircle` i `thickRectangle`?

## Typy

Każda wartość i wyrażenie ma swój typ. Typy pojawiają się przede wsztstkim w dwóch sytuacjach:
* w komunikatach o błędach (spróbuj napisać `program = drawingOf(42)`)
* możemy wskazywać typy wyrażeń i definicji 

### Proste typy

* `Program` jest typem zmiennej `program`,
* `Picture` jest typem obrazów,
* `Number` jest typem liczb (w dorosłym Haskellu używamy trochę dokładniejszych typów, jak `Int` i `Double`),
* `Color` jest typem kolorów.

Generalnie nazwy typów zaczynają sie z wielkiej litery, zmienych ‒ z małej.

Wskazania typu możemy dokonać przy pomocy `::` na przykład

```haskell
wheel :: Picture
wheel = solidCircle(size)

size :: Number
size = 4
```
W większości wypadków deklaracje typów nie są konieczne ‒ kompilator potrafi sam wywnioskować typy. 
Deklaracje mają jednak co najmniej dwie zalety:
* Są cenną dokumentacją kodu (lepszą niz komentarze - bo sprawdzaną przez kompilator).
* Czasem pozwalają na dokładniejsze komunikaty o błędach.

### Typy listowe

Jeśli ktoś spodziewał się, że typem list jest `List`, to jest w błędzie. Wszystkie elementy listy muszą być tego samego typu.
Typ listy  o elementach typu `T` oznaczamy przez `[T]`.

:question: Jakiego typu jest `[]` ?

```
program = drawingOf(circles)
circles = pictures[ circle(r) | r <- sizes ]

sizes :: [Number]
sizes = [ 1, 2, 3, 4 ]
```
:question: Jakiego typu jest zmienna `circles` powyżej?

### Punkty i krotki

A co z punktami? Można powiedzieć, że są typu `Point`:

```haskell
program = drawingOf(path[start, end])

start :: Point
start = (0, 0)

end :: Point
end = (2, -4)
```

Wspomnieliśmy jednak, że punkty są parami liczb. Dokładniej zatem, typem punktu jest `(Number,  Number)`. Typ `Point` jest synonimem tego typu i można go używać zamiennie.

Krotki mogą mieć różne rozmiary (w tym 0, ale nie 1) i różne typy elementów:

```
(4, red) :: (Number, Color)
(3, "train", 10, blue) :: (Number, Text, Number, Color)
() :: ()
```
Oczywiście elementem krotki może też być inna krotka, funkcja, program...

### Typy funkcyjne

Funkcje oczywiście też mają typy, postaci `argument -> wynik`, na przykład

```
circle :: Number -> Picture
rectangle :: (Number, Number) -> Picture
translated :: (Picture, Number, Number) -> Picture
drawingOf :: Picture -> Program
```

## Definiowanie funkcji

Do tej pory definiowaliśmy obiekty prostych typów. Możemy oczywiście definiować też wartości typów funkcyjnych.
Czasem naturalne wydaje się sparametryzowanie definicji

```
program = drawingOf(scene)
scene   = house(red)

house :: Color -> Picture
house(roofColor) = colored(roof, roofColor) & solidRectangle(6, 7)

roof :: Picture
roof = translated(thickArc(45, 135, 6, 1), 0, -2)
```

Parametr funkcji może być dowolnego typu, może to być np. obraz

```haskell
program = drawingOf(ringOf(rectangle(1,1)))
ringOf(p) = pictures([
    rotated(translated(p, 5, 0), a) | a <- [45, 90 .. 360] ])
```

Parametrem albo wynikiem funkcji może też być funkcja albo program, ale to nie dla dzieci :smiling_imp:

### Wyrażenia i definicje warunkowe

`if...then...else`

```haskell
program  = drawingOf(thing(1) & thing(2))
thing(n) = if n > 1 then rectangle(n, n) else circle(n)
```

Możemy też powiedzieć, że definicja obowiązuje tylko "pod warunkiem":

```haskell
program       = drawingOf(thing(1) & thing(2) & thing(3))
thing(n)
  | n > 2     = rectangle(n, 2)
  | n > 1     = rectangle(n, n)
  | otherwise = circle(n)
```

### Dopasowanie
Czasem chcemy zdefiniować osobne zachowania dla listy pustej, jednoelementowej i innych. Na przykład:

```haskell
f :: [Number] -> Number
f([]   ) = 42
f([a]  ) = a + 1
f(other) = sum(other)
```

Uwaga, pułapka:

```haskell
f(pi   ) = 1
f(other) = 2
```

:question: jaka jest wartość `f(0)`?

### Rekurencja

Klasycznym przykładem definicji rekurencyjnej jest silnia:

```haskell
factorial :: Number -> Number
factorial(0) = 1
factorial(n) = n * factorial(n - 1)
```

Suma listy ‒ przykład rekurencyjnej funkcji na listach:

```
program = drawingOf(circle(r))
r = suma([1,2,3])
suma([]) = 0
suma(x:xs) = x + suma(xs)
```

W grafice klasycznym przykładem rekurencji są fraktale:

```
program = drawingOf(fractal(10))

fractal :: Number -> Picture
fractal(0) = stem
fractal(n) = stem
           & translated(part, 0,  5)
           & translated(part, 0, -5)
  where part = rotated(scaled(fractal(n-1), 2/3, 2/3), 90)

stem = path([(0, -10), (0, 10)])
```

:pencil: Narysuj inne fraktale ‒ dywan Sierpińskiego, płatek Kocha, ...

## Animacje

Animacja jest funkcją typu `Number -> Picture` określającą jaki obraz wyświetlić w danej chwili czasu.
Czas jest mierzony w sekundach od uruchomienia programu.

```haskell
program = animationOf(propellor)
propellor :: Number -> Picture
propellor(t) = rotated(solidRectangle(10, 1), 60 * t)
```

Przykład animacji używającej translacji, rotacji i zakresu ‒ zależnych od czasu:

```haskell
program   = animationOf(wheels)
wheels(t) = pictures([
    translated(rotated(tire, -60 * t), t - 10, y)
    | y <- [0, 2 .. t]])
tire      = circle(1) & solidRectangle(0.1, 2)
```

:pencil: Napisz swoją animację ‒ wahadło, odbijająca się piłka,...

:pencil: Napisz animację pokazującą w kilkusekundowych odstępach Twoje rozwiązania poprzednich ćwiczeń (oprócz animacji).

# GitHub

https://github.com

Na tych zajęciach będziemy wykorzystywać GitHub. Jesli jeszcze nia masz konta ‒ załóż.

Materiały są dostepne w repozytorium `https://github.com/mbenke/jnp3-haskell/` (dostęp możliwy bez zakładania konta, ale konto przyda się za chwilę).


W notatkach są błędy, takie jak literówki (niektóre umyślne). Wykonaj [fork](https://help.github.com/articles/fork-a-repo/) tego repo na swoim koncie, popraw jakiś błąd i zgłoś [pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)

# Zastrzeżenia prawne

Przykłady i niektóre opisy pochodzą z dokumentacji CodeWorld: https://code.world/doc.html?help/codeworld.md

CodeWorld jest dostępny na licencji Apache: https://github.com/google/codeworld/blob/master/LICENSE
