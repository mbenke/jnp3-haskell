# Lenistwo

Haskell jest językiem *leniwym* (lazy, non-strict). Co to oznacza?
Przed omówieniem leniwej ewaluacji warto poświęcić chwilę na omówienie jej przeciwieństwa,
jakim jest ewaluacja *gorliwa* (eager, strict).

## Gorliwa ewaluacja
> **Pan Jourdain** — *Jak to? Więc kiedy mówię: Michasiu, podaj mi pantofle i przynieś krymkę — to proza?*

> **Nauczyciel filozofii** — *Tak, panie.*

Większość języków programowania (np. C, Java, ML, etc) używa gorliwej strategii ewaluacji wyrażeń.
Dla większości programistów jest ona niezauważalna, jak dla molierowskiego pana Jourdain — proza.

Otóż gdy w języku gorliwym funkcja

``` haskell
k x y = x
```

użyta zostanie w wyrażeniu

``` haskell
k 42 (fib 1234567)
```

przed wywołaniem funkcji `k`, obliczone zostaną jej argumenty: `42` (już obliczone) i `fib 1234567` (dużo pracy).
Praca ta zostanie wykonana na darmo, albowiem `k` nie uzywa swojego drugiego argumentu.

## Leniwa ewaluacja

Alternatywą dla gorliwej ewaluacji jest *leniwa ewaluacja* - wartości argumentów są obliczane kiedy (i o ile w ogóle są potrzebne).
W Haskellu obowiązuje taki własnie paradygmat. Dlaczego jednak wszystkie języki go nie używają? Przyczyny są dwojakiego rodzaju:

- Implementacja leniwej ewaluacji jest trudniejsza - do funkcji nie przekazujemy wartości, ale domknięcie, które pozwoli ją obliczyć.

- W przypadku gorliwej ewaluacji łatwiej przewidzieć sposób i kolejność realizacji efektów ubocznych; rozważmy np.

``` haskell
main = f (print 1) (print 2)
```

W przypadku gorliwej ewaluacji możemy się spodziewać że przed wywołaniem `f` wypisane zostanie 1 i 2 (chociaż nie każdy język zagwarantuje nam kolejność).
W przypadku leniwej ewaluacji nie wiemy kiedy i czy w ogóle cokolwiek zostanie wypisane.

Wyzwaniem jakie stanęło przed twórcami Haskella było więc wymyślenie sposobu włączenia do języka efektów ubocznych w sposób uporządkowany tak aby programista mógł je kontrolować i nie naruszały one zasadniczej czystości języka. Po kilku iteracjach (strumienie? kontynuacje? monady!) znaleźli rozwiązanie: poznany przez nas niedawno typ IO. Przy okazji okazało się, że monady pozwalają na dokładniejszą kontrolę efektów niż "wszystko albo nic".

## Strumienie

Jedną z ciekawych możliwości, jakie daje leniwa ewaluacja jest programowanie z (potencjalnie) nieskończonymi strukturami danych. Jedną z takich struktur są strumienie, czyli leniwe listy.

Możemy na przykład zdefiniować strumień wszystkich liczb naturalnych:

```
> nats = [0..]
> few = take 5
> few nats
[0,1,2,3,4]
```
Potem możemy wybrać ze strumienia  tylko parzyste:

```
> evens = [x | x <- nats, even x]
> few evens
[0,2,4,6,8]

> odds = map (+1) evens
> few odds
[1,3,5,7,9]
```
:pencil: Zaproponuj inny sposób zdefiniowania strumieni liczb parzystych/nieparzystych

Leniwa ewaluacja oznacza, że argumenty wyliczane są tylko kiedy są potrzebne i tylko w takim stopniu w jakim są potrzebne.
W praktyce, ewaluacja jest sterowana przez dopasowanie wzorca. W naszym przykładzie oznacza to, że lista obliczona zostanie
dopiero gdy dopasowanie wzorca w funkcji `take` sprawdzi czy mamy do czynienia z listą pustą czy niepustą:

```
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs
```

Przy czym sama głowa i ogon listy (`x` i `xs`) nie zostaną obliczone dopóki nie wykonamy na nich dopasowania wzorca.

W interpreterze wypisanie wartości wymaga oczywiście wyliczenie wszystkich jej elementów.

Strumień liczb naturalnych możemy też zdefiniować bezpośrednio przy pomocy

```
> from n = n:from(n+1)
> nats = from 0

> p1 = zip evens odds
> few p1
[(0,1),(2,3),(4,5),(6,7),(8,9)]

add (a:as) (b:bs) = (a+b) : add as bs
> few $ add evens odds
[1,5,9,13,17]

> p3 = add nats (tail nats)
> few p3
???
```

:pencil: W podobny sposób zdefiniuj strumień wszystkich liczb Fibonacciego

``` haskell
take 20 fibs
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
(0.01 secs, 141,984 bytes)
```

:pencil: Zdefniuj funkcję `merge` tak, aby

```
> take 5 (merge evens odds)
[0,1,2,3,4]
```

:pencil:
Zdefiniuj funkcję `union`, która dla dwóch rosnących strumieni da ich sumę teoriomnogościową. Czy zadziała też dla strumieni niemalejących?

## Liczby pierwsze

Krótka definicja strumienia liczb pierwszych

``` haskell
x -/ p = x `mod` p > 0

primes1 :: [Integer]
primes1 = sieve [2..] where
  sieve (p:xs) = p : sieve [x | x<-xs, x -/ p]
```

mozna ją usprawnić...

:pencil: Zmodyfikuj tę definicję biorąc pod uwagę, że wszystkie liczby pierwsze powyżej 2 są nieparzyste

:pencil: Uzupełnij definicję

``` haskell
primes3 :: [Int]
primes3 = 2:[x | x <- xs, isPrime x] where
  xs = ...
  isPrime x = all (x -/) (factorsToTry x)
  factorsToTry x = ...
```

tak by sprawdzać tylko dzielniki `x` nie większe od pierwiastka `x` (ale używamy tylko arytmetyki całkowitej).

## Lenistwo, `reverse`,`foldr` i `foldl`

Z oczywistych względów do strumieni nie możemy stosować `reverse`. Z podobnych powodów, nie możemy stosować `foldl`,
który "moralnie" odwraca listę:

```
> foldl (flip (:)) [] [1..5]
[5,4,3,2,1]
```

Mozemy za to z powodzeniem stosować `foldr` (z leniwą operacją):

```
> take 5 (foldr (:) [] [1..])
[1,2,3,4,5]
```

W ogólności, przy leniwych operacjach zwykle lepiej stosowac `foldl` zaś przy gorliwych - `foldl'`, który jest gorliwą wersją `foldl`.
