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
--En esta función como nuestro caso base definimos el factorial de 0 como 1, puesto que al multiplicar los números por su antecesor 
--si se multiplicarán por 0 el resultado sería 0.
--Esta función es recursiva puesto que del número dado se resta 1 y se multiplica por este número hasta llegar al factorial de 0
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial(x-1)

-- Defiinir el algoritmo de la división.
-- Esta función evalua los dos enteros dados en dos casos, uno en donde el Divisor es es mayor y otra donde el dividendo es mayor
-- Esta genera como resultado una tupla, el primer valor es el cociente y el otro es el residuo.
-- en el primer caso donde el divisor es mayor al dividendo el cociente es 0 y el residuo es el divisor.
-- Esta función aplica la recursión en el caso en donde el dividendo es mayor, en este caso se le resta el divisor al dividendo y 
-- se vuelve a aplicar el resultado de la resta siendo este el dividendo y con el mismo divisor
-- Se suma 1 al resultado x cada que se aplica este recursión
-- Así hasta que al efectuar la resta el divisor sea mayor que el dividendo y asi se llega al primer caso en donde acaba la operación.
division :: Integer -> Integer -> (Integer, Integer)
division a b
  | a < b = (0, a)
  | a >= b = let (x, y) = division (a-b) b
             in (x+1, y)



-- Define la función maximo de forma recursiva que regrese el valor mas grande de una lista de enteros:
-- Primero definimos el Caso Base en donde la lista es de un elemento, aquí el máximo será este elemento
-- Luego el caso recursivo en donde de una lista de más elementos de evalua el "tope" x y se aplica la función al resto de la lista
-- si x es mayor tomara el valor x y en otro caso se toma el maximo del resto de la lista.
-- Así hasta que se acabe la lista y se hayan evaluado todos los elementos y así se elige el elemento "mayor", el "máximo".
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
-- Cuenta el número total de nodos en un árbol binario.
-- - Si el árbol está vacío (`Vacio`), devuelve `0`.
-- - Si hay un nodo, cuenta `1` por ese nodo y luego suma el número de nodos en los subárboles izquierdo y derecho.


-- Definir una función que calcule la profundidad de un árbol
profundidad :: ArbolB a -> Int
profundidad Vacio = 0
profundidad (Nodo _ izq der) = 1 + max (profundidad izq) (profundidad der)
-- Calcula la profundidad (altura) del árbol binario.
-- - Si el árbol está vacío (`Vacio`), la profundidad es `0`.
-- - Si hay un nodo, suma `1` y luego toma el valor máximo entre la profundidad de los subárboles izquierdo y derecho.
