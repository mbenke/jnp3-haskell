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

Rozważmy na przykład funkcje o następujących sygnaturach

```
zagadka1 :: a -> a
zagadka2 :: a -> b -> a
```

ile róznych implementacji potrafisz napisać?

Trochę trudniejsze, być może do domu: co można powiedzieć o rodzinie funkcji typu

```
(a -> a) -> a -> a
```
