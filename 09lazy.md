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

```
k x y = x
```

użyta zostanie w wyrażeniu

```
k 42 (fib 1234567)
```

przed wywołaniem funkcji `k`, obliczone zostaną jej argumenty: `42` (już obliczone) i `fib 1234567` (dużo pracy).
Praca ta zostanie wykonana na darmo, albowiem `k` nie uzywa swojego drugiego argumentu.

Alternatywą dla gorliwej ewaluacji jest *leniwa ewaluacja* - wartości argumentów są obliczane kiedy (i o ile w ogóle są potrzebne). 
W Haskellu obowiązuje taki własnie paradygmat. Dlaczego jednak wszystkie języki go ie uzywaja? Przyczyny są dwojakiego rodzaju:

- Implementacja leniwej ewaluacji jest trudniejsza - do funkcji nie przekazujemy wartości, ale domknięcie, które pozwoli ją obliczyć.

- W przypadku gorliwej ewaluacji łatwiej przewidzieć sposób i kolejność realizacji efektów ubocznych; rozważmy np.

```
main = f (print 1) (print 2)
```

W przypadku gorliwej ewaluacji możemy się spodziewać że przed wywołaniem `f` wypisane zostanie 1 i 2 (chociaż nie każdy język zagwarantuje nam kolejność). 

W przypadku leniwej ewaluacji nie wiemy kiedy i czy w ogóle cokolwiek zostanie wypisane.
