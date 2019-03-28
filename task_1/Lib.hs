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


-- type TranFun = Point -> Point 

-- blank :: TranFun
-- blank = id

-- data Transform = T TranFun

data TransformHelper = Rotate R | Translate Vec
data Transform = T [TransformHelper]

instance Eq TransformHelper where
  (Rotate r) == (Rotate r1) = r == r1
  (Translate (V x y)) == (Translate (V x1 y1)) = x == x1 && y == y1
  _ == _ = False

instance Show TransformHelper where
  show (Rotate r) = "Rotate by " ++ show r
  show (Translate (V x y)) = "Translate by " ++ show x ++ " and " ++ show y

instance Eq Transform where
  (T (t:t1)) == (T (tt:t2)) = t == t && (T t1) == (T t2)
  T [] == T [] = True
  T [] == T t = False

instance Show Transform where
  show (T (t:ts)) = show t ++ show (T ts)
  show (T []) = ""

-- przesunięcie o wektor
translate :: Vec -> Transform
translate v = T [Translate v]


-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate r = T [Rotate r]
  

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

checkIfSame :: TransformHelper -> TransformHelper -> Bool
checkIfSame (Rotate r) (Rotate r1) = True
checkIfSame (Translate v) (Translate v1) = True
checkIfSame _ _ = False

sumTranslations :: TransformHelper -> TransformHelper -> TransformHelper
sumTranslations (Rotate r) (Rotate r1) = Rotate (r + r1)
sumTranslations (Translate (V x y)) (Translate (V x1 y1)) = Translate (V (x + x1) (y + y1)) 

combineTransforms :: [TransformHelper] -> [TransformHelper] -> [TransformHelper]
combineTransforms trans@(t:ts) trans1@(t1:t1s) = case (t,t1) of
  (Rotate r, Rotate r1) -> (reverse ts) ++ [Rotate (r1 + r)] ++ t1s
  (Translate (V x y), Translate (V x1 y1)) -> (reverse ts) ++ [Translate (V (x + x1) (y + y1))] ++ t1s
  (_, _) -> (reverse trans) ++ trans1
combineTransforms t t1 = (reverse t) ++ t1

instance Mon Transform where 
  m1 = T []
  (><) (T t) (T t1) = T (combineTransforms (reverse t) t1)
  

trpoint :: Transform -> Point -> Point
trpoint (T t) p = foldr f p t where
  f (Rotate r) p = rot r p
  f (Translate (V x y)) (P x1 y1) = P (x + x1) (y + y1)


trvec :: Transform -> Vec -> Vec
trvec t (V x y) = new_vector where
  (P x1 y1) = trpoint t (P x y)
  new_vector = V x1 y1


transform :: Transform -> Picture -> Picture
transform t (PP x) = PP (foldr f [] x) where
  f (p, p1) acc = (trpoint t p, trpoint t p1):acc
