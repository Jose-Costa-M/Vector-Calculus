-- DEFINIMOS LAS OPERACIONES BASE
add :: Float -> Float -> Float
add = (\x y -> x + y)

sub :: Float -> Float -> Float
sub = (\x y -> x - y)

mult :: Float -> Float -> Float
mult = (\x y -> x*y)

raiz :: Float -> Float
raiz a = sqrt a 

--- > INSTRUCCIONES < ---
instructions :: String
instructions = " Bienvenido a la calculadora de vectores  :), para verificar las operaciones escribe funciones_r2, funciones_r3 , funciones_rn o si quieres limpiar la terminal, limpiar"

funciones_r2 :: String
funciones_r2 = "Escribe eleccion_r2 y digita la opcion: 1.- Suma 2.- Resta 3.- Multiplicacion por escalar 4.- Producto punto 5.- Producto Vectorial 6.- Magnitud 7.- Direccion"

eleccion_r2 :: Int -> String
eleccion_r2 x 
      | x == 1 = "Escribe: vectordos add (numero,numero) (numero,numero)"
      | x == 2 = "Escribe: vectordos sub (numero,numero) (numero,numero)"
      | x == 3 = "Escribe: p_escalar_dos mult numero (numero,numero)"
      | x == 4 = "Escribe: p_punto_dos mult (numero,numero) (numero,numero)"
      | x == 5 = "Escribe: p_vec_dos mult (numero,numero) (numero,numero)"
      | x == 6 = "Escribe: magnituddos raiz mult (numero,numero)"
      | x == 7 = "Escribe: direct arctan (numero,numero)"
      |otherwise = "No se encontro la opción"

funciones_r3 :: String
funciones_r3 = "Escribe eleccion_r3 y digita la opcion: 1.- Suma 2.- Resta 3.- Multiplicacion por escalar 4.- Producto punto 5.- Producto Vectorial 6.- TPV 7.- TPE 8.- Magnitud"

eleccion_r3 :: Int -> String
eleccion_r3 x 
      | x == 1 = "Escribe: vector_tres add (numero,numero,numero) (numero,numero,numero)"
      | x == 2 = "Escribe: vector_tres sub (numero,numero,numero) (numero,numero,numero)"
      | x == 3 = "Escribe: p_escalar_tres mult numero (numero,numero,numero)"
      | x == 4 = "Escribe: p_punto_tres mult (numero,numero,numero) (numero,numero,numero)"
      | x == 5 = "Escribe: p_vec_tres mult (numero,numero,numero) (numero,numero,numero)"
      | x == 6 = "Escribe: tvp mult (numero,numero,numero) (numero,numero,numero) (numero,numero,numero)"
      | x == 7 = "Escribe: tpe mult (numero,numero,numero) (numero,numero,numero) (numero,numero,numero)"
      | x == 8 = "Escribe: magnitudtres raiz mult (numero,numero,numero)"
      |otherwise = "No se encontro la opción"

funciones_rn :: String
funciones_rn = "Escribe eleccion_rn y digita la opcion: 1.- Suma 2.- Resta 3.- Multiplicacion por escalar 4.- Producto punto 5.- Magnitud "

eleccion_rn :: Int -> String
eleccion_rn x 
      | x == 1 = "Escribe: vector_rn add [numero...numero] [numero...,numero]"
      | x == 2 = "Escribe: vector_rn sub [numero...numero] [numero...,numero]"
      | x == 3 = "Escribe: vector_escalar mult numero [numero...,numero]"
      | x == 4 = "Escribe: vector_punto mult [numero...numero] [numero...,numero]"
      | x == 5 = "Escribe: mag raiz [numero...,numero]"
      |otherwise = "No se encontro la opción"

limpiar :: String
limpiar = "Para limpiar usa: :! cls "
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
arctan =(\x y ->atan ( y/x ) *180/pi  )

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