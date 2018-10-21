# CodeWorld/Haskell

https://code.world/haskell

Dokumentacja modułu CodeWorld: https://code.world/doc-haskell/CodeWorld.html

```haskell
import CodeWorld
main :: Program
main = program       -- dorośli używają main jako głównej funkcji
type Program = IO () -- program wykonuje IO i nie daje wartości 

program :: Program
program = drawingOf design

design :: Picture
design = circle 2
```

Pierwsze cztery linie możemy na razie potraktować jako "program obowiązkowy".

Wskazywanie typów nie jest obowiązkowe, ale jest dobra praktyką - w tej wersji przy braku  sygnatury otrzymamy ostrzeżenie postaci


```
Line 10, Column 1: warning: [-Wmissing-signatures]
    Top-level binding with no type signature: design :: Picture
```

## Argumenty funkcji i nawiasy

Jak widać na przykład w definicji `design = circle 2` nawiasy wokół argumentu nie są potrzebne (chyba, ze jest on wyrażeniem złożonym).
Nawiasy są potrzebne wokół krotek (są częścią ich składni), na przykład

```haskell
center :: Point
center = (0,0)
```

Jeśli funkcja potrzebuje wielu argumentów, możemy je przekazać jako krotkę.
Ale idiomatyczny w Haskellu jest inny sposób - przekazywanie argumentów "jeden po drugim", np

```haskell
design :: Picture
design = rectangle 4 8
```

Funkcja `rectangle` ma typ `Double -> Double -> Picture`, co należy rozumieć jako funkcję o argumencie typu `Double`
i wyniku typu (funkcyjnego) `Double -> Picture`. Funkcje mogą być wynikami (jak i argumentami funkcji).

Zaletą takiego podejścia, jest to, że nie musimy podawać wszystkich argumentów od razu, na przykład

```
-- thickRectangle :: Double -> Double -> Double
myRectangle ::  Double -> Double
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
  acc = acc + 3 * lst[i];
}
```

W Haskellu pracujemy raczej na całych strukturach danych i napiszemy po prostu

```haskell
sum (map (3*) lst)
```

W nowszych bibliotekach do C++ czy Javy znajdziemy podobne mechanizmy, ale wywodzą się one z programowania funkcyjnego.

## Animacje i typy numeryczne

Przypomnijmy sobie rysunek sygnalizatora ulicznego. Możemy zapisać go np. tak:

```haskell
import CodeWorld

botCircleGreen, topCircleRed, frame, trafficLight :: Picture
botCircleGreen = colored green (translated 0 (-1.5) (solidCircle 1))
topCircleRed   = colored red   (translated 0   1.5  (solidCircle 1))
frame = rectangle 2.5 5.5
trafficLight = botCircleGreen & topCircleRed & frame

ourPicture :: Picture
ourPicture = trafficLight

main :: IO ()
main = drawingOf ourPicture
```

Aby światła się zmieniały co jakiś czas, możemy użyć takiej animacji 
[(zobacz na CodeWorld)](https://code.world/haskell#Ph3vruxsOVmcnYG0D2NGG0Q)

```haskell
trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 2 == 0 = trafficLight True
  | otherwise                = trafficLight False
                                                  
main :: IO ()
main = animationOf trafficController
```

Przy okazji zobaczymy groźnie wyglądające ostrzeżenie:

```
Line 18, Column 5: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘Integer’
        (Eq a0) arising from a use of ‘==’ at Line 18, Column 5-28
        (Integral a0) arising from a use of ‘mod’ at Line 18, Column 5-23
        (Num a0) arising from the literal ‘2’ at Line 18, Column 23
    • In the expression: round (t / 3) `mod` 2 == 0
      In a stmt of a pattern guard for
                     an equation for ‘trafficController’:
        round (t / 3) `mod` 2 == 0
      In an equation for ‘trafficController’:
          trafficController t
            | round (t / 3) `mod` 2 == 0 = trafficLight True
            | otherwise = trafficLight False
```

Jeśli mu się dokładniej przyjrzymy, zauważymy jednak, ze precyzyjnie wskazuje ono źródło problemu:

```
...arising from the literal ‘2’ at Line 18, Column 23
```

możemy go uniknąć, wskazując typ literału `2':

```haskell
trafficController t
  | round (t/3) `mod` (2::Integer) == 0 = trafficLight True
  | otherwise                = trafficLight False
```
:pencil: Dodaj do animacji sygnalizatora krótką fazę żółtą.

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

Większość operacji binarnych wymaga aby argumenty były tego samego typu; `i*pi` nie zadziała jeśli i jest typu `Int`.
Konwersje typów musimy wykonywać explicite:

* `fromIntegral` konwertuje z typów całkowitych do dowolnego typu liczbowego
* `round`, `floor`, `ceiling` konwertują z typów zmiennoprzecinkowych do całkowitych

Wyjątkiem są literały całkowite takie jak `1` czy `42` - mają poniekąd "wbudowane" `fromIntegral`.

Do porównań na wszelkich typach liczbowych (i nie tylko) możemy uzywać `==`, `/=`, `<` `<=`, `>`, `>=` `min`, `max` (oba argumenty muszą być tego samego typu)

:pencil: Napisz funkcję `lights :: Integer -> Picture` taką, że `lights n` narysuje n sygnalizatorów obok siebie.

:pencil: Napisz funkcję `squares :: Double -> Picture`, taką, że `squares d` narysuje obok siebie prostokąty o łącznym polu `d` przy czym wszystkie oprócz być może ostatniego będą kwadratami o boku  1.

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

Zdefiniuj funkcje `wall,ground, storage, box :: Picture`, tworzące obrazy odpowiednich pól rozmiaru 1 wyśrodkowane na środku obrazu. 

Zdefiniuj funkcję 
`drawTile :: Integer -> Picture` taką że `drawTile n` daje obraz pola numeru n według listy powyżej.
Funkcja powinna zachowywać się sensownie również dla argumentów spoza zakresu.

Poziom możemy reprezentować jako funkcję typu `Integer -> Integer -> Integer`,
która otrzymawszy dwie współrzędne daje rodzaj pola, które znajduje się w podanym miejscu.

Przykładowy poziom:

```
maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

```

Zdefiniuj obraz `pictureOfMaze :: Picture`, który rysuje powyższy poziom dla współrzędnych x,y z zakresu `[-10..10]`,
wykorzystując obrazy dane przez funkcję `drawTile` przesunięte w odpowiednie miejsca.

Oddawanie zadania poprzez GitHub Classroom: https://classroom.github.com/a/HZF7-yvJ
