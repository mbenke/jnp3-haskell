# Sokoban 1

W kolejnych tygodniach będziemy implementować grę **Sokoban** (https://en.wikipedia.org/wiki/Sokoban zawiera opis i animację, po polsku 
https://pl.wikipedia.org/wiki/Sokoban):

> Plansza składa się z układu kwadratów, część z nich to ściany przez które nie może przechodzić gracz ani skrzynia.
> Sokoban jest grą, w której dozorca w hurtowni musi przesuwać przedmioty (zwykle paczki, piłki lub skrzynie) 
> na odpowiednie miejsca, przy jak najmniejszej liczbie wykonanych ruchów (lub pchnięć, w zależności od kryteriów punktowania).
> Dozorca może pchać tylko jedną paczkę, nie można ich ciągnąć, ani przez nie przechodzić. 
> Poziomy skomplikowania gry zaczynają się od bardzo łatwych, a kończą na bardzo trudnych.

W tym tygodniu wykonamy kilka czynnosci przygotowawczych, w szczególności potrzebujemy rysunków różnych pól:

1. Ścian (wall)
2. Pustych pól (ground)
3. Pól oznaczonych jako miejsca docelowe składowania skrzyń (storage)
4. Skrzyń (box)

Wynikiem tego ćwiczenia będzie kod niezbędny do narysowania poziomu gry.

Zdefiniuj funkcje `wall,ground, storage, box :: Picture`, tworzące obrazy odpowiednich pól rozmiaru 1 wyśrodkowane na środku obrazu. 

Zdefiniuj funkcję 
`drawTile :: Integer -> Picture` taką że `drawTile n` daje obraz pola numeru n według listy powyżej.
Funkcja powinna zachowywac się sensownie również dla argumentów spoza zakresu.

Poziom możemy reprezentować jako funkcję typu `Integer -> Integer -> Integer`,
która otrzymawszy dwie współrzędne daje rodzaj pola, które znajduje się w podanym miejscu.
Przykładowy poziom:

```
maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0  -- blank
  | abs x == 4 || abs y == 4 = 1  -- wall
  | x ==  2 && y <= 0        = 1  -- wall
  | x ==  3 && y <= 0        = 3  -- storage
  | x >= -2 && y == 0        = 4  -- box
  | otherwise                = 2  -- ground
```

Zdefiniuj obraz `pictureOfMaze :: Picture`, który rysuje powyższy poziom dla współrzędnych x,y z zakresu [-10..10], wykorzystujac obrazy dane przez funkcję drawTile przesunięte w odpowiednie miejsca. Program główny powinien pokazywać ten rysunek.
