-- UNIVERSIDAD NACIONAL AUTÓNOMA DE MEXICO
-- PRÁCTICA 01: Naturales, inducción y recursión
-- Ramírez Mendoza Joaquín Rodrigo
-- Villalobos Juárez Gontran Eliut
-- Treviño Puebla Héctor Jerome

data Natural = Cero | S Natural deriving Show

-- Práctica Naturales
-- Definir una función a natural que tome un entero (0,1,2,...) y lo lleve a la
-- estructura de dato Natural.
-- El dato de entrada es un número entero, definimos el caso base en 0 ya que no es sucesor de nadie
-- y usamos el tipo de dato abstracto para transformarlo en sucesor
-- Ejemplo: a_natural 2 = S (S Cero)
a_natural :: Int -> Natural
a_natural 0 = Cero;
a_natural a = S(a_natural (a-1))

-- Definir una función a_entero que lleve un dato Natural a su valor entero
-- El dato de entrada de tipo de dato natural lo deconstruiremos a su forma de tipo entero
-- Ejemplo: a_entero (S (S Cero)) = 2
a_entero :: Natural -> Int
a_entero Cero = 0
a_entero (S a) = a_entero a + 1

-- Definir una función que realice una suma sobre la estructura de dato Natural
-- Recibimos dos datos en la entrada de tipo natural retornando la suma que resulte de ellos en forma de natural
-- 
--Ejemplo: suma_nat (S (S Cero)) (S Cero) = S (S (S Cero))
suma_nat :: Natural -> Natural -> Natural
suma_nat Cero b = b
suma_nat (S a) b = S(suma_nat a b)

-- Definir una función que realice la multiplicación de la estructura de un dato Natural
-- La forma de multiplicación recursiva recibe dos datos de tipo Natural y retornaremos el resultado de
-- la multiplicación igual en su forma de natural utilizando la función de suma antes definida.
-- Ejemplo: mult_nat (S (S Cer)) (S (SCero)) = S (S (S (S Cero)))
mult_nat :: Natural -> Natural -> Natural
mult_nat b Cero  = Cero
mult_nat b (S a) = suma_nat (b) (mult_nat b a)

--          Práctica recursión
-- Definir una función que tome como un argumento un entero y regrese el número de Fibonacci correspondiente a ese entero.
-- La función fibonacci recibe un valor de tipo entero sobre el cual operaremos.
-- la recursión debe implicar a los casos base para que la función pare y regrese los resultados.
-- fibonacci 4 = 3.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci a = fibonacci(a-1) + fibonacci(a-2)

-- Definir la función de multiplicación entre dos enteros de forma recursiva:
-- La función de multiplicación recibe dos números de tipo entero que es el multiplicador y el multiplicando
-- la función realizará las operaciones hasta llegar al caso base del multiplicando retornando el valor deseado.
-- es como la suma de n veces un número
-- Ejemplo: mult_ent 4 3 => 12
mult_ent :: Int -> Int -> Int
mult_ent a 0 = 0
mult_ent a b = a + (mult_ent a (b - 1))

-- Definir la función de potencia de forma recursiva:
-- La función potencia recibe dos argumentos, la base y el exponente, ambos de tipo entero los cuales nos retornaran
-- otro número de tipo entero dependiendo de los parámetros, su estructura es como el anterior pero, en lugar de
-- sumar, multiplica.
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



-- Práctica Árboles
data ArbolB a = Vacio | Nodo a (ArbolB a) (ArbolB a) deriving (Eq, Show)

-- Definir una funcion que calcule el número de nodos del árbol.
contarNodos :: ArbolB a -> Int
contarNodos Vacio = 0
contarNodos (Nodo _ izq der) = 1 + contarNodos izq + contarNodos der

-- Definir una función que calcule la profundidad de un árbol
profundidad :: ArbolB a -> Int
profundidad Vacio = 0
profundidad (Nodo _ izq der) = 1 + max (profundidad izq) (profundidad der)
