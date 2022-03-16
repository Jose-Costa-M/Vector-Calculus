-- DEFINIMOS LAS OPERACIONES BASE
add :: Float -> Float -> Float
add = (\x y -> x + y)

sub :: Float -> Float -> Float
sub = (\x y -> x - y)

mult :: Float -> Float -> Float
mult = (\x y -> x*y)

raiz :: Float -> Float
raiz a = sqrt a 

--- > VECTORES EN R2 < ---

--- > SUMA Y RESTA FUNCIONES DE ORDEN SUPERIOR Y CURRYING <---
vectordos :: ( Float -> Float -> Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
vectordos = (\f (x1,x2) (y1,y2)->(f x1 y1, f x2 y2))
--NOTA: VECTORDOS FUNCIONA PARA LA SUMA, sub

--- > PRODUCTO POR UN ESCALAR FUNCIONES DE ORDEN SUPERIOR Y CURRYING . f = multi < ---
p_escalar_dos :: (Float -> Float -> Float)-> Float -> (Float, Float) -> (Float, Float)
p_escalar_dos = (\f w (x1,x2) ->(f w x1, f w x2))

--- > PRODUCTO PUNTO ENTRE 2 VECTORES R2 F.O.S Y CURRYING f = mult < ---
p_punto_dos :: (Float -> Float -> Float) -> (Float, Float) -> (Float, Float) -> Float
p_punto_dos = (\f (x1,x2)(y1,y2) -> f x1 y1 + f x2 y2 ) 

p_vec_dos :: (Float -> Float -> Float) -> (Float,Float) -> (Float, Float) -> (Float,Float,Float)
p_vec_dos = (\f (x1,x2)(y1,y2) -> (0 ,0 ,f x1 y2 - f x2 y1)) 


magnituddos :: (Float -> Float) -> (Float -> Float -> Float) -> (Float, Float) -> Float
magnituddos = (\f g (x1,x2) -> f( (g x1 x1 + g x2 x2) )) 

arctan :: Float -> Float ->Float
arctan =(\x y ->1/(tan ( (y/x)*(pi/180) ) ) )

direct :: ( Float -> Float -> Float) ->(Float, Float) -> Float
direct = (\f(x1,x2) -> f x1 x2) 


-- VECTORES R3
vector_tres ::( Float -> Float -> Float) -> (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
vector_tres = \f(x1,x2,x3) (y1,y2,y3) ->(f x1 y1, f x2 y2, f x3 y3)  

p_escalar_tres ::(Float -> Float -> Float)-> Float -> (Float, Float,Float) -> (Float, Float, Float)
p_escalar_tres = (\f w(x1,x2,x3) ->(f w x1, f w x2, f w x3))

p_punto_tres :: (Float -> Float -> Float) -> (Float, Float,Float) -> (Float, Float,Float) -> Float
p_punto_tres = (\f (x1,x2,x3)(y1,y2,y3) -> f x1 y1 + f x2 y2 + f x3 y3  )


p_vec_tres :: (Float -> Float -> Float) -> (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
p_vec_tres f (x1,x2,x3) (y1,y2,y3) = ((f x2 y3) - (f x3 y2), -1*((f x1 y3)  - (f x3 y1) ), (f x1 y2) - (f x2 y1))

-- TRIPLE PRODUCTO VECTORIAL DE LA FORMA AX(BXC) f -> mult
tvp ::( Float -> Float -> Float)->(Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
tvp = (\f (x1,x2,x3) (y1,y2,y3) (z1,z2,z3) ->( p_vec_tres f (x1,x2,x3) ( p_vec_tres f (y1,y2,y3) (z1,z2,z3) ) ) )


tpe :: ( Float -> Float -> Float)->(Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float) -> Float
tpe = (\f (x1,x2,x3) (y1,y2,y3) (z1,z2,z3) -> x1*((f y2 z3) - (f y3 z2)) - x2*(f y1 z3 - f y3 z1) + x3*(f y1 z2 - f y2 z1))


magnitudtres :: (Float -> Float ) -> (Float -> Float -> Float) -> (Float, Float, Float) -> Float
magnitudtres = (\f g (x1,x2,x3) -> f(g x1 x1 + g x2 x2 + g x3 x3) )

coseno :: Float -> Float ->Float
coseno = (\x y-> cos (x/y))


--VECTORES RN (RECURSIVIDAD 100% Y ORDEN SUPERIOR)

-- Sumas  resta de RN
vector_rn :: (Float -> Float -> Float) -> [Float] -> [Float] -> [Float]
vector_rn f []  [] = []
vector_rn f (x:xs) (y:ys) = (f x y) : vector_rn  f xs ys

--Multipliación por un escalar
vector_escalar :: (Float -> Float -> Float) -> Float -> [Float] -> [Float]
vector_escalar f w  [] = []
vector_escalar f w (x:xs)  = (f w x) : vector_escalar  f w xs 

--Producto punto
vector_punto :: (Float -> Float -> Float) -> [Float] -> [Float] -> Float
vector_punto f [] []         = 0
vector_punto f (x:xs) []         = 0
vector_punto f  []    (y:ys)     = 0
vector_punto f (x:xs) (y:ys) = f x y + vector_punto  f xs ys


-- Magnitud
add_magnitud ::  [Float] -> Float
add_magnitud    []          = 0
add_magnitud   (x:xs)  = mult x x + add_magnitud    xs 

mag :: ( Float -> Float ) -> [Float] -> Float
-- f = raiz, xs vector con n entradas
mag f xs = f ( add_magnitud xs )




--Para los vectores en RN solo existen estás operaciones ya que no estan definidas el producto vectorial, TPV y TPE