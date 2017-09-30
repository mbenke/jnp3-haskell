# ELI5 

*Explain Like I'm 5* - czyli wyjasnij mi to jak pięciolatkowi [1].

Haskell ma opinię języka trudnego i skomplikowanego, dlatego zaczniemy od kursu, nawet jeśli nie dla pięciolatków, 
to dla uczniów szkoły podstawowej.

[1] https://www.reddit.com/r/explainlikeimfive/

# Code World

https://code.world/

CodeWorld jest edukacyjnym srodowiskiem programistycznym dostępnym w przeglądarce internetowej.
Przy uzyciu prostego modelu matematycznego dla figur i przekstałceń, pozwala tworzyć rysunki, animacje, a nawet gry.

# Rysowanie

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

:pencil: Wypróbuj, z róznymi wartościami:

```haskell
circle(8)
circle(0.5)
solidCircle(5)
rectangle(4,8)
solidRectangle(8,4)
text("W przedszkolu naszym nie jest źle")
```

:exclamation: Zapisuj rozwiazania ćwiczeń w pliku tekstowym - będzie potrzebny później.

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

Kolory mozna modyfikować przy pomocy funkcji `dark`, `light`, `translucent`. Wypróbuj je i przeczytaj o nich w dokumentacji.

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

:pencil: Narysuj symbol atomu (koło jako jądro i elipsy jako orbity elektroów)

## Wyrażenia

Z prawej strony definicji (po znaku `=`)   umieszczamy *wyrażenie*. Użycie definiowanej nazwy jest równoważne użyciu tego wyrażenia.
Nazywamy to *przejrzystością odwołań* (referential transparency). Wyrażenia mogą być też argumentami funkcji (i obowiązuje tu podobna zasada).

Przykłady wyrażeń:

```
4
2+2
circle(2+2)
colored(text("Help"), red)
rectangle(1, 4) & circle(2)
```

Natomiast `x=1` nie jest wyrażeniem - jest definicją.

## Funkcje

Szczególnym rodzajem wyrażeń są *funkcje*. Podstawową operacją którą możemy wykonać przy pomocy funkcji jest zastosowanie jej do argumentów, Na przykład

* `rectangle` jest funkcją. Dostawszy wysokość i szerokość, produkuje obraz (prostokąt)
* `light` jest funkcją. Dostawszy kolor, produkuje (podobny, ale jaśniejszy) kolor. 
* `drawingOf` jest funkcją. Dostawszy  obraz, konstruuje program, który rysuje ten obraz.
* `id` jest funkcją identycznościową

Skoro funkcje są wyrażeniami to czy moga stać po prawej stronie definicji i być argumentami dla funkcji? Ależ tak:

```
rysuj = drawingOf
koło = id(circle)
program = rysuj(koło(2))
```

:pencil: Narysuj 'gwiazdkę' złozoną z 7 wąskich prostokatów (o wymiarach `(4, 0.2)` lub podobnych)

# Zastrzeżenia prawne

Przykłady i niektóre opisy pochodzą z dokumentacji CodeWorld: https://code.world/doc.html?help/codeworld.md

CodeWorld jest dostępny na licencji Apache: https://github.com/google/codeworld/blob/master/LICENSE
