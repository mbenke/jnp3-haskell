# Testowanie

## QuickCheck
Oprócz podobnych jak w innych językach bibliotek do testów jednostkowych (np. doctest, HUnit), 
Haskell posiada specyficzną, opartą na typach bibliotekę QuickCheck

* Generowanie dużej liczby testów jednostkowych jest żmudne

* Sprawdzenie wszystkich możliwości jest nierealistyczne

* Pomysł: wygenerować odpowiednią losową próbkę danych

### Instalacja

```
cabal install QuickCheck
```

lub

```
stack install QuickCheck
```

## Przykład

``` haskell
import Test.QuickCheck

prop_fadd_comm :: Float -> Float -> Bool
prop_fadd_comm a b = a + b == b + a

prop_fadd_assoc :: Float -> Float -> Float -> Bool
prop_fadd_assoc a b c = (a + b) + c  == a + (b + c)
```


```
λ> quickCheck prop_fadd_com
+++ OK, passed 100 tests.
λ> quickCheckWith stdArgs {maxSuccess = 1000} prop_fadd_comm
+++ OK, passed 1000 tests.
λ> quickCheck prop_fadd_assoc
*** Failed! Falsifiable (after 3 tests and 1 shrink): 
-2.4964767
1.7917264
2.8828287
```

Ogólna zasada: 

* definiujemy własności, które mają być przetestowane - w przybliżeniu: funkcje o typie wyniku `Bool`, 
dokładniej  - typu, który należy do klasy `Testable`;
* QuickCheck losuje pewną liczbę zestawów danych testowych
i sprawdza, czy dla wszystkich własność jest spełniona;
* Istnieją standardowe generatory dla typów wbudowanych, dla własnych typów trzeba je zdefiniować
* Z uwagi na losowanie argumentów, własności powinny być monomorficzne

:pencil: Zdefiniuj i sprawdź kilka własności arytmetyki na liczbach całkowitych oraz funkcji na listach.

## Główne składniki

``` haskell
quickCheck  :: Testable a => a -> IO ()
quickCheck   = check quick


check :: Testable a => Config -> a -> IO ()
quick :: Config

class Testable a where
  property :: a -> Property

instance Testable Bool where...

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

class Arbitrary a where
  arbitrary :: Gen a
  shrink    :: a -> [a]

instance Monad Gen where ...
```

## Dygresja - generacja liczb losowych

``` haskell
import System.Random
  ( StdGen       -- :: *
  , newStdGen    -- :: IO StdGen
  , randomR      -- :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
  , split        -- :: RandomGen g => g -> (g, g)
                 -- rozdziela argument na dwa niezależne generatory
  -- instance RandomGen StdGen
  -- instance Random Int  
  )
  
roll :: StdGen -> Int
roll rnd = fst $ randomR (1,6) rnd
main = do 
  rnd <- newStdGen 
  let (r1,r2) = split rnd
  print (roll r1)
  print (roll r2)
  print (roll r1)
  print (roll r2)
```

```
*Main System.Random> main
4
5
4
5
```

Samo `StdGen` jest czyste i daje za każdym razem ten sam wynik, dlatego zwykle opakowywane jest w odpowiednią monadę.

Nie będziemy w tym momencie wchodzić w szczegóły, ale w przypadku QuickCheck używamy `Gen`.

## Generatory losowych obiektów

``` haskell
choose :: (Int,Int) -> Gen Int
oneof :: [Gen a] -> Gen a

instance Arbitrary Int where
    arbitrary = choose (-100, 100)

data Colour = Red | Green | Blue
instance Arbitrary Colour where
    arbitrary = oneof [return Red, return Green, return Blue]

instance Arbitrary a => Arbitrary [a] where
    arbitrary = oneof [return [], liftM2 (:) arbitrary arbitrary]
    -- NB to nie jest najlepszy generator dla list - jaka jest oczekiwana długość listy?

-- | `sized` tworzy generator z rodziny generatorów indeksowanej rozmiarem
sized :: (Int -> Gen a) -> Gen a

listOf :: Gen a -> Gen [a]
listOf gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf k gen
     
generate :: Gen a -> IO a
sample :: Show a => Gen a -> IO ()
```
```

## Generator drzew

``` haskell
data Tree a = Branch (Tree a) (Tree a) 
            | Leaf a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree'
    where tree' 0 = Leaf <$> arbitrary
          tree' n | n>0 =
                    oneof [Leaf <$> arbitrary,
                           Branch <$> subtree <*> subtree]
                    where subtree = tree' (n `div` 2)
  shrink (Leaf _) = []
  shrink (Branch l r) = [l, r] ++ [Branch l' r' | (l', r') <- shrink (l, r)]
```

```
λ> sample (arbitrary :: Gen (Tree Int))
Leaf 0
Leaf (-2)
Branch (Branch (Leaf 3) (Leaf 1)) (Branch (Branch (Leaf 3) (Leaf (-2))) (Branch (Leaf 0) (Leaf 0)))
Leaf 0
Leaf 5
Branch (Leaf (-9)) (Leaf 7)
Branch (Branch (Branch (Branch (Leaf (-9)) (Leaf (-4))) (Leaf (-4))) (Leaf 0)) (Branch (Branch (Branch (Leaf (-2)) (Leaf (-5))) (Branch (Leaf (-11)) (Leaf 1))) (Branch (Branch (Leaf (-10)) (Leaf (-3))) (Leaf (-4))))
Leaf (-3)
Leaf 0
Branch (Branch (Branch (Leaf 17) (Leaf 14)) (Leaf 4)) (Leaf 3)
Branch (Leaf (-9)) (Branch (Leaf 2) (Leaf (-9)))
```