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

data Transform = T TranFun

-- data Transform = Id | Rotate R Transform | Translate Vec Transform

-- instance Eq Transform where
  -- t1 == t2 

instance Show Transform where
  show t = "Transform a point"

-- przesunięcie o wektor
translate :: Vec -> Transform
translate (V x y) = T f where 
  f (P x1 y1) = P (x + x1) (y + y1) 


-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate r = T f where
  f p = rot r p
  

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
  m1 = T blank
  (><) (T t) (T t1) = T (t . t1)
  

trpoint :: Transform -> Point -> Point
trpoint (T t) p = t p


trvec :: Transform -> Vec -> Vec
trvec (T t) (V x y) = new_vector where
  (P x1 y1) = t (P x y)
  new_vector = V x1 y1


transform :: Transform -> Picture -> Picture
transform t (PP x) = PP (foldr f [] x) where
  f (p, p1) acc = (trpoint t p, trpoint t p1):acc
