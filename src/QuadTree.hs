module QuadTree where
import            Particle hiding (position)
import 						Linear (V2 (..), distance)
data Quad a = Quad a a a a
type Rect = (Coord, Coord) -- lower left, upper right
data Quadrant a = Empty | Bucket a Coord Int | Split (Quad (QuadTree a))
data QuadTree a = QuadTree Rect (Quadrant a)

min_dist :: Double
min_dist = 4

-- Adds element into QuadTree
-- lb - left bottom
-- rb - right bottom
-- ru - right upper
-- lu - left upper
addElement :: QuadTree a -> a -> Coord -> QuadTree a
addElement (QuadTree rect Empty) el pt = QuadTree rect (Bucket el pt 1)
addElement (QuadTree rect (Bucket element position cnt)) new_el new_pt
	| distance new_pt position < min_dist = QuadTree rect (Bucket element position (cnt + 1))
	| otherwise                       = addElement (addElement (splitRect rect element) element position) new_el new_pt
addElement (QuadTree (V2 x_lb y_lb, V2 x_ru y_ru) (Split (Quad lb rb ru lu))) new_el new_pt =
		QuadTree (V2 x_lb y_lb, V2 x_ru y_ru) (Split quad)
	where
		quad
			| is_lb     = Quad (g lb) rb ru lu
			| is_rb     = Quad lb (g rb) ru lu
			| is_ru     = Quad lb rb (g ru) lu
			| otherwise = Quad lb rb ru (g lu)
		g qtree = addElement qtree new_el new_pt
		is_lb :: Bool
		is_lb = (x_lb <= x) && (x <= x_half) && (y_lb <= y) && (y <= y_half)

		is_rb :: Bool
		is_rb = (x_half <= x) && (x <= x_ru) && (y_lb <= y) && (y <= y_half)

		is_ru :: Bool
		is_ru = (x_half <= x) && (x <= x_ru) && (y_half <= y) && (y <= y_ru)

		x_half :: Double
		x_half = (x_lb + x_ru) / 2

		y_half :: Double
		y_half = (y_lb + y_ru) / 2
		V2 x y = new_pt

getRects :: Rect -> Quad Rect
getRects (V2 x_lb y_lb, V2 x_ru y_ru) = Quad (V2 x_lb y_lb, center) (V2 x_half y_lb, V2 x_ru y_half)
	(center, V2 x_ru y_ru) (V2 x_lb y_half, V2 x_half y_ru)
		where
			x_half :: Double
			x_half = (x_lb + x_ru) / 2
			y_half :: Double
			y_half = (y_lb + y_ru) / 2
			center:: Coord
			center = V2 x_half y_half

-- placeholder for determining type of QuadTree
splitRect :: Rect -> a -> QuadTree a
splitRect rect _ = QuadTree rect (Split (Quad lb rb ru lu))
	where
		Quad lb_rect rb_rect ru_rect lu_rect = getRects rect
		lb :: QuadTree a
		lb = QuadTree lb_rect Empty
		rb :: QuadTree a
		rb = QuadTree rb_rect Empty
		ru :: QuadTree a
		ru = QuadTree ru_rect Empty
		lu :: QuadTree a
		lu = QuadTree lu_rect Empty


distRectPoint :: Rect -> Coord -> Double
distRectPoint (V2 x_lb y_lb, V2 x_ru y_ru) (V2 x y)
	| (y_lb <= y) && (y <= y_ru) && (x_lb <= x) && (x <= x_ru) = 0
	| (y_lb <= y) && (y <= y_ru) = min (abs (x - x_lb)) (abs (x - x_ru))
	| (x_lb <= x) && (x <= x_ru) = min (abs (y - y_lb)) (abs (y - y_ru))
	| otherwise = minimum [distance (V2 x y) (V2 x_lb y_lb), distance (V2 x y) (V2 x_lb y_ru),
		distance (V2 x y) (V2 x_ru y_lb), distance (V2 x y) (V2 x_ru y_ru)]


getResults :: Double -> Coord -> QuadTree a -> [(a, Int)]
getResults rad pivot qtree
	| distRectPoint rect pivot <= rad = getRange rad pivot qtree
	| otherwise												= []
		where
			QuadTree rect _ = qtree

getRange :: Double -> Coord -> QuadTree a -> [(a, Int)]
getRange _ _ (QuadTree _ Empty) = []
getRange rad pivot (QuadTree _ (Bucket element position cnt))
	| distance pivot position <= rad = [(element, cnt)]
	|	otherwise                     = []
getRange rad pivot (QuadTree _ (Split (Quad lb rb ru lu))) = concat [lb_res, rb_res, ru_res, lu_res]
  where
  	lb_res = f lb
  	rb_res = f rb
  	ru_res = f ru
  	lu_res = f lu
  		-- TODO Azat proveryt signaturu
  		-- Cancelation of qtree
  	f = getResults rad pivot
