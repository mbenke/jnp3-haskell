# Polimorfizm

Polimorfizm (z gr. wielopostaciowość) to w odniesieniu do funkcji możliwość działania na obiektach różnych typów.
W tym miejscu zajmiemy się podstawową formą polimorfizmu - polimorfizmem parametrycznym.

Przypomnijmy funkcję `interactionOf`:

```haskell
interactionOf :: world ->
                 (Double -> world -> world) ->
		 (Event -> world -> world) ->
		 (world -> Picture) ->
		 IO ()
```

Funkcja ta jest polimorficzna: mozemy ją zastosowac używając w miejscu zmiennej `world` dowolnego typu.

## Parametryczność

Ważne aby pamiętać, ze możliwosc wyboru typu lezy po stronie **wywołującego**. 
Oznacza to, że implementacja funkcji musi działać dla **wszystkich** typów.
Co więcej, musi działać **dla wszystkich typów tak samo**.

Dlaczego parametrycznośc jest ważna?

1. Umożliwia *wycieranie typów*. Skoro funkcja wywoływana działa dla każdego typu tak samo, nie potrzebuje informacji 
jakiego typu są jej faktyczne parametry. W związku z tym informacja typowa nie jest potrzebna w trakcie wykonania, a jedynie w trakcie kompilacji.

2. Ograniczając sposoby działania funkcji polimorficznych daje nam **twierdzenia za darmo** (*theorems for free*, termin ukuty przez Phila Wadlera a zarazem tytuł jego [słynnej pracy](https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf)).

:pencil: Rozważmy na przykład funkcje o następujących sygnaturach

```
zagadka1 :: a -> a
zagadka2 :: a -> b -> a
```

ile różnych implementacji potrafisz napisać?

:pencil: Trochę trudniejsze, być może do domu: co można powiedzieć o rodzinie funkcji typu

```
(a -> a) -> a -> a
```

## Polimorficzne typy danych

Polimorficzne, mogą być nie tylko funkcje, ale i typy danych. W poprzednim tygodniu pisaliśmy wariant funkcji `interactionOf`
pozwalający na cofnięcie poziomu do stanu początkowego. Spróbujmy teraz rozszerzyć tę funkcję o wyświetlanie ekranu startowego 
i rozpoczynanie właściwej gry po naciśnięciu spacji. Na początek możemy stworzyć bardzo prostry ekran startowy:

```haskell
startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")
```

Musimy wiedzieć czy jestesmy na ekranie startowym czy też gra już się toczy.  Najprosciej zapamiętać tę informację w stanie

```haskell
data SSState = StartScreen | Running world
```

Niestety przy takim kodzie, dostaniemy komunikat o błędzie `Not in scope: type variable ‘world’`. Istotnie, co to jest `world`? 
Mozemy uczynic go *parametrem typu*:

```haskell
data SSState world = StartScreen | Running world
```

Teraz możemy zaimplementować:

```haskell
startScreenInteractionOf ::
    world -> (Double -> world -> world) ->
    (Event -> world -> world) -> (world -> Picture) ->
    IO ()
startScreenInteractionOf state0 step handle draw
  = interactionOf state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
```

:pencil: Dodaj ekran startowy do swojej gry.
