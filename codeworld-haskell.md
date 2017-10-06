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
