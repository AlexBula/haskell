module Mon where

infixl 5 ><

class Mon m where
  m1 :: m
  (><) :: m -> m -> m


type R = Rational

type R2 = (R,R)

data Vec = V Integer Integer -- wektor 2D
data Point = P Integer Integer  -- punkt 2D

instance Eq Vec where
  V x y == V x1 y1 = x == x1 && y == y1

instance Eq Point where
  P x y == P x1 y1 = x == x1 && y == y1

instance Show Vec where
  show V x y = "Vector (" ++ show x ++ ", " ++ show y ++ ")"

point :: R2 -> Point
point (x, y) = P x y

vec :: R2 -> Vec
vec (x, y) = V x y


instance Mon Vec where
  m1 V x y = V x y
  (><) (V x y) (V x1 y1) = V (x+x1) (y+y1)


data Picture = PP [(Point, Point)]

-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R,R) -> (R,R) -> Picture
line p@(x, y) p1@(x1, y1) = PP [(p, p1)]

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
rectangle :: R -> R -> Picture
rectangle x y = [(p0, p1), (p1, p2), (p2, p3), (p3, p4)] where
  p0 = P 0 0
  p1 = P x 0
  p2 = P x y
  p3 = P 0 y

-- suma (nałożenie) dwóch rysunków
(&) :: Picture -> Picture -> Picture
(&) p p1 = p ++ p1 

type IntLine = ((Int,Int), (Int,Int))
type IntRendering = [IntLine]

-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
renderScaled :: Int -> Picture -> IntRendering
renderScaled c p = foldr f [] p where
  f ((x, y), (x1, y1)) acc = ((round(c*x), round(c*y)), (round(c*x1), round(c*y1))):acc


data Transform = Translate Vec | Rotate R

-- przesunięcie o wektor
translate :: Vec -> Transform
translate v@(x, y) = T f where
  f p@(P x1 y1) = P (x + x1) (y + y1)


-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate r = Rotate r 
  


rot :: R -> (Point -> Point)
rot r = f where 
  f p@(x y) = P (x * cosinus - y * sinus) (x * sinus + y * cosinus) where
    cosinus = coss(r)
    sinus = sinn(r)

fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle = 360


sinn :: R -> R
sinn x = (4 * x (180 - x)) / (40500 - x * (180 - x))

coss :: R -> R
coss x = sinn (90 - x)



instance Mon Transform where 
  m1 f = f
  (><) t t1 = t . t1
  


trpoint :: Transform -> Point -> Point
trpoint t p = t p
trvec :: Transform -> Vec -> Vec

transform :: Transform -> Picture -> Picture