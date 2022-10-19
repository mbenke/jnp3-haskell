# CodeWorld/Haskell

https://code.world/haskell

Dokumentacja modułu CodeWorld: https://code.world/doc-haskell/CodeWorld.html

```haskell
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO () -- program wykonuje IO i nie daje wartości 

main :: Program
main = program       -- dorośli używają main jako głównej funkcji

program :: Program
program = drawingOf design

design :: Picture
design = circle 2
```

Pierwsze pięc linii możemy na razie potraktować jako "stały fragment gry".

Wskazywanie typów nie jest obowiązkowe, ale jest dobra praktyką - w tej wersji przy braku  sygnatury otrzymamy ostrzeżenie postaci


```
Line 10, Column 1: warning: [-Wmissing-signatures]
    Top-level binding with no type signature: design :: Picture
```

## Argumenty funkcji i nawiasy

Jak widać na przykład w definicji `design = circle 2` nawiasy wokół argumentu nie są potrzebne (chyba, ze jest on wyrażeniem złożonym).
Nawiasy są potrzebne wokół krotek (są cześcią ich składni), na przykład

```haskell
center :: Point
center = (0,0)
```

Jesli funkcja potrzebuje wielu argumentów, moglibyśmy je przekazać jako krotkę (podobnie jak w "dziecinnym" CodeWorld).
Ale idiomatyczny w Haskellu jest inny sposób - przekazywanie argumentów "jeden po drugim", np

```haskell
design :: Picture
design = rectangle 4 8
```

Funkcja `rectangle` ma typ `Double -> Double -> Picture`, co należy rozumieć jako funkcję o argumencie typu `Double`
i wyniku typu (funkcyjnego) `Double -> Picture`. Funkcje mogą być wynikami (jak i argumentami funkcji).

Zaletą takiego podejścia, jest to, że nie musimy podawac wszystkich argumentów od razu, na przykład

```
-- thickRectangle :: Double -> Double -> Double -> Picture
myRectangle ::  Double -> Double -> Picture
myRectangle = thickRectangle 0.2
```

`myRectangle` jest funkcja oczekującą wymiarów i konstruującą prostokąt o grubości linii `0.2`.

:pencil: Przerób kilka swoich rysunków z pierwszych zajęć na "dorosły" Haskell.

## Programowanie holistyczne

Ralf Hinze:

> “Functional languages excel at wholemeal programming, a term coined by Geraint Jones.
> Wholemeal programming means to think big: work with an entire list, rather than a sequence of elements; 
> develop a solution space, rather than an individual solution; imagine a graph, rather than a single path. 
> The wholemeal approach often offers new insights or provides new perspectives on a given problem.
> It is nicely complemented by the idea of projective programming: first solve a more general problem, 
> then extract the interesting bits and pieces by transforming the general program into more specialised ones."

W językach typu Java/C często skupiamy się na pojedynczych indeksach:

```c
int acc = 0;
for ( int i = 0; i < lst.length; i++ ) {
  acc = acc + lst[i] * lst[i];
}
```

W Haskellu pracujemy raczej na całych strukturach danych i napiszemy po prostu

```haskell
sum (map (\x -> x * x) lst)
```

albo wręcz

```
sum . map square
```

W nowszych bibliotekach do C++ czy Javy znajedziemy podobne mechanizmy, ale wywodzą się one z programowania funkcyjnego.

## Animacje i typy numeryczne

Przypomnijmy sobie rysunek sygnalizatora ulicznego. Możemy zapisac go np. tak:

```haskell
import CodeWorld

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0   1.5  (solidCircle 1))

frame, trafficLight :: Picture

frame = rectangle 2.5 5.5
trafficLight = botCircle green & topCircle red & frame

ourPicture :: Picture
ourPicture = trafficLight

main :: IO ()
main = drawingOf ourPicture
```

Aby światła się zmieniały co jakiś czas, możemy użyć takiej animacji 
[(zobacz na CodeWorld)](https://code.world/haskell#Ph3vruxsOVmcnYG0D2NGG0Q)

```haskell
trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red   & frame

trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 2 == 0 = trafficLight True
  | otherwise                = trafficLight False
                                                  
main :: IO ()
main = animationOf trafficController
```

:pencil: Dodaj do animacji sygnalizatora krótką fazę żółtą.

### Typy numeryczne

Trzy typy numeryczne, które musimy w tym momencie znać to `Int`, `Integer` oraz `Double`:

* `Int` to maszynowe liczby całkowite (gwarantowany zakres co najmniej `2^29`, w praktyce obecnie zwykle `2^63`)
```haskell
i :: Int
i = -42
```
* `Integer` to liczby  całkowite bez ograniczenia zakresu
```haskell
reallyBig :: Integer
reallyBig = 2^2^2^2^2 -- potęgowanie wiąże w prawo: 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)

main = print numDigits
```

* `Double` to 64-bitowe liczby zmiennoprzecinkowe (jest też typ `Float`, rzadko używany).
```haskell
d1, d2, d3 :: Double
d1 = 4.5387
d2 = 6.2831e-4
d3 = pi
```
Później powiemy sobie jak to dokładnie działa, ale w przybliżeniu:

* `(+)` `(-)` `(*)` działają dla dowolnych typów liczbowych
* Potęgowanie (^) działa dla wykładników całkowitych
```
pi^2
9.869604401089358
```

* (/) działa dla `Double` (dokładniej dla typów, w których są ułamki)
* Dzielenie całkowite z resztą to `div` i `mod` (oraz `quot` i `rem`)
* Dla typów zmiennoprzecinkowych działa potęgowanie `(**)` oraz operacje takie jak `sin`, `cos`,`tan`,` sqrt`.

Większość operacji binarnych wymaga aby argumenty były tego samego typu; `i*pi` nie zadziała jesli `i` jest typu `Int`.
Konwersje typów musimy wykonywac explicite:

* `fromIntegral` konwertuje z typów całkowitych do dowolnego typu liczbowego
* `round`, `floor`, `ceiling` konwertują z typów zmiennoprzecinkowych do całkowitych

Wyjątkiem są literały całkowite takie jak `1` czy `42` - mają poniekąd "wbudowane" `fromIntegral`.

Do porównań na wszelkich typach liczbowych (i nie tylko) możemy uzywać `==`, `/=`, `<` `<=`, `>`, `>=` `min`, `max` (oba argumenty muszą być tego samego typu)

:pencil: Napisz funkcję `lights :: Integer -> Picture` taką, że `lights n` narysuje n sygnalizatorów obok siebie.

:pencil: Napisz funkcję `squares :: Double -> Picture`, taką, że `squares d` narysuje obok siebie prostokąty o łacznym polu `d` przy czym wszystkie oprócz być może ostatniego będą kwadratami o boku 1. Na przykład `squares 3.14` da

![obraz](https://user-images.githubusercontent.com/202086/137954791-e3531476-d01c-4223-b97f-9ccbb9a5cbd7.png)


:question: Jak Twoje funkcje zachowują się dla argumentów ujemnych?

# Sokoban :pencil:

W kolejnych tygodniach będziemy implementować grę **Sokoban** (https://en.wikipedia.org/wiki/Sokoban zawiera opis i animację, po polsku 
https://pl.wikipedia.org/wiki/Sokoban):

> Plansza składa się z układu kwadratów, część z nich to ściany przez które nie może przechodzić gracz ani skrzynia.
> Sokoban jest grą, w której dozorca w hurtowni musi przesuwać przedmioty (zwykle paczki, piłki lub skrzynie) 
> na odpowiednie miejsca, przy jak najmniejszej liczbie wykonanych ruchów (lub pchnięć, w zależności od kryteriów punktowania).
> Dozorca może pchać tylko jedną paczkę, nie można ich ciągnąć, ani przez nie przechodzić. 
> Poziomy skomplikowania gry zaczynają się od bardzo łatwych, a kończą na bardzo trudnych.

W tym tygodniu wykonamy kilka czynnosci przygotowawczych, w szczególności potrzebujemy rysunków różnych pól:

1. Ścian (wall)
2. Pustych pól (ground)
3. Pól oznaczonych jako miejsca docelowe składowania skrzyń (storage)
4. Skrzyń (box)

Wynikiem tego ćwiczenia będzie kod niezbędny do narysowania poziomu gry.

Zdefiniuj funkcje `wall, ground, storage, box :: Picture`, tworzące obrazy odpowiednich pól rozmiaru 1 wyśrodkowane na środku obrazu. 

Zdefiniuj funkcję 
`drawTile :: Int -> Picture` taką że `drawTile n` daje obraz pola numeru n według listy powyżej.
Funkcja powinna zachowywać się sensownie również dla argumentów spoza zakresu.

Poziom możemy reprezentować jako funkcję typu `Int -> Int -> Int`,
która otrzymawszy dwie współrzędne daje rodzaj pola, które znajduje się w podanym miejscu.

Przykładowy poziom:

```
maze :: Int -> Int -> Int
maze x y
  | abs x > 4  || abs y > 4  = 0  -- blank
  | abs x == 4 || abs y == 4 = 1  -- wall
  | x ==  2 && y <= 0        = 1  -- wall
  | x ==  3 && y <= 0        = 3  -- storage
  | x >= -2 && y == 0        = 4  -- box
  | otherwise                = 2  -- ground

```

Zdefiniuj obraz `pictureOfMaze :: Picture`, który rysuje powyższy poziom dla współrzędnych x,y z zakresu `[-10..10]`,
wykorzystujac obrazy dane przez funkcję `drawTile` przesunięte w odpowiednie miejsca. Program główny powinien pokazywać ten rysunek.

Oddawanie zadania poprzez GitHub Classroom:  https://classroom.github.com/a/xZFcaCnZ  termin: 27.10 godz. 18:00

**UWAGA:** rozwiązanie należy stworzyć w nowej gałęzi (branch), a po zakończeniu stworzyć pull request i oznaczyć prowadzącego (mbenke) jako jego recenzenta. Taki tryb postepowania znacznie ułatwia ocenianie.
