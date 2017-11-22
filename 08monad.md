# I/O - co jest pod maską?

Rozwiązanie problemu I/O jest oparte na typach i klasach. Musimy powiedzieć o nich coś więcej, do I/O wrócimy za chwilę.

## Typy algebraiczne

    data Tree a = Leaf a | Branch (Tree a) (Tree a)

    mapTree :: (a->b) -> Tree a -> Tree b
    mapTree f (Leaf a) = Leaf (f a)
    mapTree f (Branch l r) = Branch (m l) (m r) where
      m = mapTree f

**Leaf** jest 1-argumentowym konstruktorem,
**Branch** — 2-argumentowym. Per analogiam mówimy, że **Tree** jest jednoargumentowym *konstruktorem typu*:

-   jeśli **x** jest wartością, to **Leaf x** jest wartością;
-   jesli **a** jest typem, to **Tree a** jest typem.

### Typy Maybe i Either

Dwa przydatne typy (predefiniowane w Prelude):

    data Maybe a = Nothing | Just a
    data Either a b = Left a | Right b
    -- Prelude> head []
    -- *** Exception: Prelude.head: empty list

    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    -- *Main> safeHead []
    -- Nothing

    safeHead2 :: [a] -> Either String a
    safeHead2 [] = Left "Empty list"
    safeHead2 (x:xs) = Right x

### Synonimy

Czasem przydatne jest wprowadzenie własnej nazwy (synonimu) dla jakiegoś typu.

    type Name = String
    type Possibly = Either Name

    safeHead3 :: [a] -> Possibly a
    safeHead3 [] = Left "Empty list"
    safeHead3 (x:xs) = Right x

Synonim **nie jest** konstruktorem typu; jest identyczny z nazywanym typem.

### Etykiety pól

Spójrzmy na definicje

    data Point = Pt Float Float
    pointx                  :: Point -> Float
    pointx (Pt x _)         =  x
    pointy ...

Definicja **pointx** jest “oczywista”; możemy krócej:

    data Point = Pt {pointx, pointy :: Float}

W jednej linii definiujemy typ **Point**, konstruktor **Pt**
oraz funkcje **pointx** i **pointy**.

### Opakowywanie typów: **newtype**

Jeśli chcemy opakować istniejacy typ w nowy konstruktor typu, mozemy uzyć konstrukcji **newtype**:

    newtype Identity a = Identity { runIdentity :: a } 
      deriving (Eq, Show)

    *Newtype> Identity "Ala"
    Identity {runIdentity = "Ala"}
    *Newtype> runIdentity it
    "Ala"

**newtype** działa niemal identycznie jak **data** z jednym konstruktorem(ale efektywniej; 
pakowanie/odpakowywanie odbywa się w czasie kompilacji a nie wykonania).
