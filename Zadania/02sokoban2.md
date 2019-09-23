# Sokoban 2

## Etap 1: ruchomy gracz

Stwórz definicję `player1 :: Picture` reprezentującą figurkę gracza.

Zdefiniuj `walk1 :: IO ()` wykorzystujące `interactionOf` aby:
* postać gracza była rysowana na obrazie poziomu;
* początkowa pozycja gracza wypadała na pustym polu (można uzyć ustalonych współrzędnych, nie trzeba szukać pustego pola w programie);
* klawisze strzałek przesuwały obraz gracza (obraz poziomu ma pozostać nieruchomy);
* gracz przesuwał się tylko  na pola `Ground` lub `Storage` (nie wchodzimy na ściany ani pudła).

Zwróć uwagę na kolejnosc elementów w `&` bądź `pictures`:

```haskell
design, square, circ :: Picture
design =  pictures [square, circ]
circ = colored red (solidCircle 1)
square = colored black (solidRectangle 1 1)
```

## Etap 2: gracz skierowany

Chcemy aby postać gracza patrzyła w stronę, w którą się porusza (co najmniej lewo-prawo). Zdefiniuj funkcję `player2 :: Direction -> Picture` dającą figurkę gracza skierowaną w odpowiednią stronę.

Rozszerz kod z Etapu 1, definiując `walk2 :: IO()` tak, aby figurka gracze była wyświetlana odpowiednio do kierunków ruchu.

:point_right: **Wskazówka:** pomyśl najpierw o typach (np. stanu świata), potem o implementacji.

:exclamation: **Uwaga:** upewnij się, że po Twoich modyfikacjach funkcja `walk1` nadal działa.

## Etap3: reset

W trakcie gry przydatna będzie mozliwość rozpoczęcia poziomu od początku. 
Ta funkcjonalność jest w gruncie rzeczy niezależna od gry, zatem zaimplemntujmy ją ogólnie. Napisz funkcję

```haskell
resettableInteractionOf ::
    world ->
    (Double -> world -> world) ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
```

która zasadniczo będzie działać jak `interactionOf`, ale dla zdarzenie odpowiadające naciśnięciu klawisza `Esc` nie jest przekazywane dalej, ale powoduje powrót stanu gry do stanu początkowego.

Zastanów się co powinno się dziać dla zdarzenia odpowiadającego puszczeniu klawisza `Esc` i opisz swój wybór w komentarzu.

Zdefiniuj `walk3 :: IO ()` jako wariant `walk2` używający `resettableInteractionOf`.
