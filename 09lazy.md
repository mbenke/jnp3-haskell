> **Pan Jourdain** — *Jak to? Więc kiedy mówię: Michasiu, podaj mi pantofle i przynieś krymkę — to proza?*

> **Nauczyciel filozofii** — *Tak, panie.*

# Lenistwo

Haskell jest językiem *leniwym* (lazy, non-strict). Co to oznacza? 
Przed omówieniem leniwej ewaluacji warto poświęcić chwilę na omówienie jej przeciwieństwa,
jakim jest ewaluacja *gorliwa* (eager, strict).

## Gorliwa ewaluacja

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
