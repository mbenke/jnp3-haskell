# CodeWorld/Haskell

https://code.world/haskell

Dokumentacja modułu CodeWorld: https://code.world/doc-haskell/CodeWorld.html

```haskell
import CodeWorld
main :: Program
main = program       -- dorosli używają main jako głównej funkcji
type Program = IO () -- program wykonuje IO ui nie daje wartosci 

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

Jak widać na przykład w definicji `design = circle 2` nawiasy wokół argumentu nie sdą potrzebne (chyba, ze jest on wyrażeniem złożonym).
Nawiasy są potrzebne wokół krotek (są cześcią ich składni), na przykład

```haskell
center :: Point
center = (0,0)
```

Jesli funkcja potrzebuje wielu argumentów, mo żemy je przekazać jako krotkę. Ale idiomatyczny w Haskellu jest inny sposób - przekazywanie argumentów "jeden po drugim", np

```haskell
design :: Picture
design = rectangle 4 8
```

Funkcja `rectangle` ma typ `Double -> Double -> Picture`, co nalezy rozumieć jako funkcję o argumencie typu `Double`
i wyniku typu (funkcyjnego) `Double -> Picture`. Funkcje mogą być wynikami (jak i argumentami funkcji).

Zaletą takiego podejścia, jest to, ze niem usimy podawac wszystkich argumentów od razu, na przykład

```
-- thickRectangle :: Double -> Double -> Double
myRectangle ::  Double -> Double
myRectangle = thickRectangle 0.2
```

`myRectangle` jest funkcja oczekującą wymiarów i konstruującą prostokąt o grubości linii `0.2`.

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

W Haskellu napiszemy po prostu

```haskell
sum (map (3*) lst)
```

W nowszych bibliotekach do C++ czy Javy znajedziemy podobne mechanizmy, ale wywodzą się one cz programowania funkcyjnego.
