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
