# Polimorfizm

Polimorfizm (z gr. wielopostaciowość) to w odniesieniu do funkcji możliwość działania na obiektach różnych typów.
W tym miejscu zajmiemy się podstawową formą polimorfizmu - polimorfizmem parametrycznym.

Przypomnijmy funkcję `activityOf`:

```haskell
activityOf :: world ->
	      (Event -> world -> world) ->
	      (world -> Picture) ->
              IO ()
```

Funkcja ta jest polimorficzna: możemy ją zastosować używając w miejscu zmiennej `world` dowolnego typu.

## Parametryczność

Ważne aby pamiętać, że możliwość wyboru typu leży po stronie **wywołującego**. 
Oznacza to, że implementacja funkcji musi działać dla **wszystkich** typów.
Co więcej, musi działać **dla wszystkich typów tak samo**.

Dlaczego parametryczność jest ważna?

1. Umożliwia *wycieranie typów*. Skoro funkcja wywoływana działa dla każdego typu tak samo, nie potrzebuje informacji, 
jakiego typu są jej faktyczne parametry. W związku z tym informacja typowa nie jest potrzebna w trakcie wykonania, a jedynie w trakcie kompilacji.

2. Ograniczenie sposobów działania funkcji polimorficznych daje nam **twierdzenia za darmo** (*theorems for free*, termin ukuty przez Phila Wadlera, a zarazem tytuł jego [słynnej pracy](https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf)).

:pencil: Rozważmy na przykład funkcje o następujących sygnaturach

```haskell
zagadka1 :: a -> a
zagadka2 :: a -> b -> a
```

ile różnych implementacji potrafisz napisać?

:pencil: Trochę trudniejsze, być może do domu: co można powiedzieć o rodzinie funkcji typu

```haskell
(a -> a) -> a -> a
```

## Polimorficzne typy danych

Polimorficzne mogą być nie tylko funkcje, ale i typy danych. W poprzednim tygodniu pisaliśmy wariant funkcji `activityOf`
pozwalający na cofnięcie poziomu do stanu początkowego. 
Spróbujmy teraz rozszerzyć tę funkcję o wyświetlanie ekranu startowego i rozpoczynanie właściwej gry po naciśnięciu spacji.
Na początek możemy stworzyć bardzo prosty ekran startowy:

```haskell
startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")
```

Musimy wiedzieć czy jesteśmy na ekranie startowym czy też gra już się toczy.  Najprościej zapamiętać tę informację w stanie

```haskell
data SSState = StartScreen | Running world
```

Niestety przy takim kodzie dostaniemy komunikat o błędzie `Not in scope: type variable ‘world’`. Istotnie, co to jest `world`? 
Możemy uczynić go *parametrem typu*:

```haskell
data SSState world = StartScreen | Running world
```

Teraz możemy zaimplementować:

```haskell
startScreenActivityOf ::
    (Event -> world -> world) -> 
    (world -> Picture) ->
    IO ()
startScreenActivityOf state0 handle draw
  = activityOf state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
```

:pencil: Dodaj ekran startowy do swojej gry.

## Interakcje całościowe

Chcielibyśmy teraz połączyć funkcjonalność  `startScreenActivityOf` z funkcjonalnością `resettableActivityOf`

```haskell
resettableActivityOf ::
    world ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
```

tak, aby naciśnięcie `ESC` wracało do ekranu startowego. Ale nie możemy - obie te funkcje dają wynik typu `IO()` a nie biorą argumentów takiego typu. Musimy spróbować innego podejścia.

Gdybyśmy mieli typ `Activity`, opisujący interakcje, oraz funkcje

```haskell
resettable :: Activity -> Activity
withStartScreen :: Activity -> Activity
```

moglibyśmy uzyskać pożądany efekt przy pomocy ich złożenia. Potrzebowalibyśmy jeszcze funkcji

```haskell
runActivity :: Activity -> IO ()
```

Jak możemy zdefiniować taki typ `Activity` ? 

Na razie obrazek, żeby zasugerować rozwiązanie, ale jeszcze go nie zdradzać:

![Cat in a box with a cat in a box](https://i.redd.it/k5mjhyewkxdz.jpg)

Musimy opakować argumenty funkcji `activityOf` wewnątrz typu `Activity`:

```haskell
data Activity world = Activity
        world
	(Event -> world -> world)
	(world -> Picture)
```
Zwróćmy uwagę, że dla pełnej ogólności typ świata `world` musi być parametrem typu `Activity`.

Implementacja funkcji `resettable` nie przedstawia większych trudności - musimy po prostu wypakować potrzebne wartości
przy pomocy dopasowania wzorca:

```haskell
resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Interaction state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s
```

Implementacja (a co najmniej zapisanie typu) funkcji `withStartScreen` wymaga chwili namysłu. Zauważmy, że funkcjonalność tę osiągnęliśmy przez rozszerzenie stanu świata:

```haskell
data SSState world = StartScreen | Running world
```

Sygnatura naszej funkcji może wyglądać tak:

```haskell
withStartScreen :: Activity s -> Activity (SSState s)
```

a implementacja np. tak:

```haskell
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
 ```
 
 Do kompletu potrzebujemy jeszcze funkcji `runActivity`.
 
 :pencil: Napisz funkcję `runActivity :: Activity s -> IO ()`
 
 :pencil: Przepisz funkcje `walk2` i `walk3` ze swojego rozwiązania tak aby używały funkcji `runActivity` i `resettable`.
 
 :pencil: Napisz funkcję `walk4 :: IO ()` rozszerzającą `walk3` o ekran startowy.

# :pencil: Zadanie - Sokoban 3

https://github.com/jnp3-haskell-2018/sokoban-3

Oddawanie przez [https://classroom.github.com/a/xVBuFP5_](https://classroom.github.com/a/xVBuFP5_)

Termin: 30.11.2019 06:00 UTC+0100
