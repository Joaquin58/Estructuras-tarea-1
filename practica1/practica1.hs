-- UNIVERSIDAD NACIONAL AUTÓNOMA DE MEXICO
-- PRÁCTICA 01: Naturales, inducción y recursión
-- Ramírez Mendoza Joaquín Rodrigo
-- Villalobos Juárez Gontran Eliut
-- Treviño Puebla Héctor Jerome

data Natural = Cero | S Natural deriving Show

-- Práctica Naturales
-- Definir una funcion ́ a natural que tome un entero (0,1,2,...) y lo lleve a la
-- estructura de dato Natural.
a_natural :: Int -> Natural
a_natural 0 = Cero;
a_natural a = S(a_natural (a-1))

-- Definir una función a_entero que lleve un dato Natural a su vaor entero
a_entero :: Natural -> Int
a_entero Cero = 0
a_entero (S a) = a_entero a + 1

-- Definir una función que realice una suma sobre la estructura de dato Natural
suma_nat :: Natural -> Natural -> Natural
suma_nat Cero b = b
suma_nat (S a) b = S(suma_nat a b)

-- Definir una función que realice la multiplicación de la estructura de un dato Natural
mult_nat :: Natural -> Natural -> Natural
mult_nat b Cero  = Cero
mult_nat b (S a) = suma_nat (b) (mult_nat b a)

--          Práctica recursión
-- Definir una función que tome como un argumento un entero y regrese el número de Fibonacci correspondiente a ese entero.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci a = fibonacci(a-1) + fibonacci(a-2)

-- Definir la funcion de multiplicación entre dos enteros de forma recursiva:
mult_ent :: Int -> Int -> Int
mult_ent a 0 = 0
mult_ent a b = a + (mult_ent a (b - 1))

-- Definir la función de potencia de forma recursiva:
pot_ent :: Int -> Int -> Int
pot_ent a 0 = 1
pot_ent a b = a * (pot_ent a (b-1))

--Definir la función factorial de forma recursiva
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial(x-1)

-- Defiinir el algoritmo de la división.
division :: Integer -> Integer -> (Integer, Integer)
division a b
  | a < b = (0, a)
  | a >= b = let (x, y) = division (a-b) b
             in (x+1, y)


-- Define la función maximo de forma recursiva que regrese el valor mas grande de una lista de enteros:
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) =
  let y = maximo xs
  in if x > y
     then x
     else y



-- Pŕactica Arboles
data ArbolB a = Vacio | Nodo a (ArbolB a) (ArbolB a) deriving (Eq, Show)

-- Definir una funcion que calcule el número de nodos del árbol.  ́


-- Definir una función que calcule la profundidad de un árbol
