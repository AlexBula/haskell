module Lib where

import Mon

type R = Rational

type R2 = (R,R)

data Vec = V R R -- wektor 2D
data Point = P R R  -- punkt 2D

instance Eq Vec where
  V x y == V x1 y1 = x == x1 && y == y1

instance Eq Point where
  P x y == P x1 y1 = x == x1 && y == y1

instance Show Vec where
  show (V x y) = "Vector (" ++ show x ++ ", " ++ show y ++ ")"

instance Show Point where
  show (P x y) = "Point (" ++ show x ++ ", " ++ show y ++ ")"

point :: R2 -> Point
point (x, y) = P x y

vec :: R2 -> Vec
vec (x, y) = V x y


instance Mon Vec where
  m1 = V 0 0
  (><) (V x y) (V x1 y1) = V (x+x1) (y+y1)


data Picture = PP [(Point, Point)]

-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R,R) -> (R,R) -> Picture
line (x, y) (x1, y1) = PP [(P x y, P x1 y1)]

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
rectangle :: R -> R -> Picture
rectangle x y = PP [(p0, p1), (p1, p2), (p2, p3), (p3, p0)] where
  p0 = (P 0 0)
  p1 = (P x 0)
  p2 = (P x y)
  p3 = (P 0 y)

-- suma (nałożenie) dwóch rysunków
(&) :: Picture -> Picture -> Picture
(&) (PP p) (PP p1) = PP new_list where
  new_list = p ++ p1 

type IntLine = ((Int,Int), (Int,Int))
type IntRendering = [IntLine]

-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
renderScaled :: Int -> Picture -> IntRendering
renderScaled c (PP p) = foldr f [] p where
  f ((P x y), (P x1 y1)) acc = 
    (((round $ (fromIntegral c)*x), (round $ (fromIntegral c)*y)), 
    ((round $ (fromIntegral c)*x1), (round $ (fromIntegral c)*y1))):acc


type TranFun = Point -> Point 

blank :: TranFun
blank = id

data Transform = T TranFun | I blank

-- data Transform = Id | Rotate R Transform | Translate Vec Transform

instance Eq Transform where
  Translate v1 == Translate v2 = v1 == v2
  Rotate r1 == Rotate r2 = r1 == r2
  Translate v1 == Rotate r1 = False
  Compose v1 r1 == Compose v2 r2 = v1 == v2 && r1 == r2
  Compose v1 r1 == Translate v2 = r1 == 0 && v1 == v2
  Compose v1 r1 == Rotate r2 = v1 == (V 0 0) && r1 == r2

instance Show Transform where
  show (Translate v1) = "Translate by " ++ show v1
  show (Rotate r1) = "Rotate by " ++ show r1
  show (Compose v1 r1) = "Translate by " ++ show v1 ++ " and rotate by " ++ show r1

-- przesunięcie o wektor
translate :: Vec -> Transform
translate v = v


-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate r = Rotate r 
  

rot :: R -> Point -> Point
rot r = f where 
  f (P x y) = P (x * cosinus - y * sinus) (x * sinus + y * cosinus) where
    cosinus = coss(r)
    sinus = sinn(r)

fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle = 360


sinn :: R -> R
sinn x
  | x < 0 = -sinn(-x)
  | x >= 0 && x < 180 = (4 * x * (180 - x)) / (40500 - x * (180 - x))
  | x >= 180 && x < 360 = -sinn(x - 180)
  | otherwise = sinn(x - 360)

coss :: R -> R
coss x = sinn(90 + x)


instance Mon Transform where 
  m1 = Translate (V 0 0)
  (><) (Translate (V x y)) (Translate (V x1 y1)) = Translate (V (x + x1) (y + y1))
  (><) (Rotate r) (Rotate r1) = Rotate (r + r1)
  (><) (Translate v) rot@(Rotate r) = Compose v r
  (><) rot@(Rotate r) (Translate v) = Compose v r
  (><) (Translate (V x1 y1)) (Compose (V x y) r) = Compose (V (x + x1) (y + y1)) r
  (><) rot@(Rotate r1) (Compose v r) = Compose (trvec rot v) (r + r1)
  (><) (Compose (V x y) r) (Compose (V x1 y1) r1) = Compose (V (x + x1) (y + y1)) (r + r1)
  (><) x y = (><) y x
  

trpoint :: Transform -> Point -> Point
trpoint (Translate v@(V x y)) p@(P x1 y1) = P (x + x1) (y + y1)
trpoint (Rotate r) p@(P x1 y1) = rot r p
trpoint (Compose v@(V x y) r) p@(P x1 y1) = trpoint (Translate v) (rot r p)


trvec :: Transform -> Vec -> Vec
trvec (Translate _) v = v
trvec (Rotate r) v@(V x1 y1) = new_vector where
  rotated_point@(P x2 y2) = rot r (P x1 y1)
  new_vector = V x2 y2
trvec (Compose _ r) v1@(V x y) = new_vector where
  rotated_point@(P x1 y1) = rot r (P (x) (y))
  new_vector = V x1 y1


transform :: Transform -> Picture -> Picture
transform t (PP x) = PP (foldr f [] x) where
  f (p, p1) acc = (trpoint t p, trpoint t p1):acc
