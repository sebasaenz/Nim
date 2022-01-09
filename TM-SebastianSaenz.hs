type Posicion = [Int]
type Jugada = (Int, Int)

-- Ejercicio 1

-- Funcion auxiliar para "jugar" que lleva la cuenta del indice de las posiciones mediante un segundo parametro i
restarItemsDePila :: Posicion -> Int -> Jugada -> Posicion
restarItemsDePila (x:xs) i (a, b) | i == a = (x - b):xs
                                  | otherwise = x:restarItemsDePila xs (i + 1) (a, b)

-- Eliminar ceros
eliminarCeros :: Posicion -> Posicion
eliminarCeros [] = []
eliminarCeros (x:xs) | x == 0 = eliminarCeros xs
                     | otherwise = x:eliminarCeros xs

-- Devuelve la posicion obtenida al realizar una jugada en una determinada posicion
jugar :: Posicion -> Jugada -> Posicion
jugar p j = eliminarCeros (restarItemsDePila p 1 j)

-------------------------------

-- Ejercicio 2

-- Funcion auxiliar para unir dos listas de jugadas
agregarListaALista :: [Jugada] -> [Jugada] -> [Jugada]
agregarListaALista [] lista = lista
agregarListaALista (x:xs) lista = agregarListaALista xs (x:lista)

-- Devuelve las jugadas posibles en una pila determinada
agregarJugadasEnPila :: Int -> Int -> [Jugada] -> [Jugada]
agregarJugadasEnPila cantidad indice jugadas | cantidad == 0 = jugadas
                                             | otherwise = agregarJugadasEnPila (cantidad - 1) indice ((indice, cantidad):jugadas)

-- Funcion auxiliar con un segundo parametro que lleva la cuenta de las posiciones de las pilas y un tercero que va acumulando las jugadas
posibleJugadaEnPila :: Posicion -> Int -> [Jugada] -> [Jugada]
posibleJugadaEnPila [] _ j = j
posibleJugadaEnPila (x:xs) i j = posibleJugadaEnPila xs (i + 1) (agregarListaALista (agregarJugadasEnPila x i []) j)

-- Devuelve las posibles jugadas en una posicion determinada
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas p = posibleJugadaEnPila p 1 []

-------------------------------

-- Ejercicio 3

-- Funcion auxiliar para chequear si una posicion ya esta incluida en una lista de posiciones
posicionIncluida :: Posicion -> [Posicion] -> Bool
posicionIncluida _ [] = False
posicionIncluida p (x:xs) | p == x = True
                          | otherwise = posicionIncluida p xs

-- Funcion auxiliar que toma como parametros una lista de jugadas y una lista de posiciones (que se inicializara como una lista vacia en la funcion principal)
-- que devuelve la lista de posiciones a partir de las jugadas introducidas como parametro
posiblesPosicionesAuxiliar :: Posicion -> [Jugada] -> [Posicion] -> [Posicion]
posiblesPosicionesAuxiliar p [] posicionesFinales = posicionesFinales
posiblesPosicionesAuxiliar p (x:xs) posicionesFinales | posicionIncluida (jugar p x) posicionesFinales = posiblesPosicionesAuxiliar p xs posicionesFinales
                                                      | otherwise = posiblesPosicionesAuxiliar p xs ((jugar p x):posicionesFinales)

-- Funcion que devuelve las posiciones posibles que pueden surgir a partir de una posicion y las jugadas validas a partir de ella
posiblesPosiciones :: Posicion -> [Posicion]
posiblesPosiciones p = posiblesPosicionesAuxiliar p (posiblesJugadas p) []

-- El parametro jugada de las siguientes funciones utiliza la paridad del numero de jugada para determinar el turno del jugador

-- Funcion que a partir de una lista de posiciones devuelve si TODAS las posiciones son perdedoras o no
sonTodasPosicionesPerdedoras :: [Posicion] -> Int -> Bool
sonTodasPosicionesPerdedoras [x] jugada = esPosicionPerdedora x jugada
sonTodasPosicionesPerdedoras (x:xs) jugada = esPosicionPerdedora x jugada && sonTodasPosicionesPerdedoras xs jugada

-- Funcion que a partir de una lista de posiciones devuelve si ALGUNA de las posiciones es perdedora o no
esAlgunaPosicionPerdedora :: [Posicion] -> Int -> Bool
esAlgunaPosicionPerdedora [x] jugada = esPosicionPerdedora x jugada
esAlgunaPosicionPerdedora (x:xs) jugada = esPosicionPerdedora x jugada || esAlgunaPosicionPerdedora xs jugada

-- Funcion que devuelve si una posicion es perdedora a partir de la posicion y el numero de jugada
esPosicionPerdedora :: Posicion -> Int -> Bool
esPosicionPerdedora [] jugada = odd jugada
esPosicionPerdedora p jugada | odd jugada = sonTodasPosicionesPerdedoras (posiblesPosiciones p) (jugada + 1)
                             | otherwise = esAlgunaPosicionPerdedora (posiblesPosiciones p) (jugada + 1)

-- Funcion que devuelve si una posicion es ganadora
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora [] = False
esPosicionGanadora p = esAlgunaPosicionPerdedora (posiblesPosiciones p) 1

-------------------------------

-- Ejercicio 4

-- Funcion que a partir de una posicion y una lista de jugadas evalua si alguna de esas jugadas genera una posicion perdedora y en ese caso la devuelve
procesarJugadas :: Posicion -> [Jugada] -> Jugada
procesarJugadas p [x] | esPosicionPerdedora (jugar p x) 1 = x
                      | otherwise = (0, 0)
procesarJugadas p (x:xs) | esPosicionPerdedora (jugar p x) 1 = x
                         | otherwise = procesarJugadas p xs

-- Funcion que devuelve una posible jugada ganadora para una determinada posicion
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = procesarJugadas p (posiblesJugadas p)

-------------------------------

-- Ejercicio 5

-- Funcion auxiliar que a partir de una posicion y una lista de jugadas devuelve todas las posiciones que surgen sin filtrar las repetidas (necesario
-- para evaluar correctamente la cantidad de jugadas ganadoras)
posiblesPosicionesAuxiliarSinFiltrarRepetidas :: Posicion -> [Jugada] -> [Posicion]
posiblesPosicionesAuxiliarSinFiltrarRepetidas p [] = []
posiblesPosicionesAuxiliarSinFiltrarRepetidas p (x:xs) =  (jugar p x):(posiblesPosicionesAuxiliarSinFiltrarRepetidas p xs)

-- Funcion que devuelve la lista de posiciones que surgen a partir de realizar las jugadas validas dentro de una posicion determinada
posiblesPosicionesSinFiltrarRepetidas :: Posicion -> [Posicion]
posiblesPosicionesSinFiltrarRepetidas p = posiblesPosicionesAuxiliarSinFiltrarRepetidas p (posiblesJugadas p)

-- Funcion que a partir de una lista de posiciones devuelve cuantas de ellas son perdedoras
cantidadDePosicionesPerdedoras :: [Posicion] -> Int -> Int
cantidadDePosicionesPerdedoras [] cantidad = cantidad
cantidadDePosicionesPerdedoras (x:xs) cantidad | esPosicionPerdedora x 1 = cantidadDePosicionesPerdedoras xs (cantidad + 1)
                                               | otherwise = cantidadDePosicionesPerdedoras xs cantidad

-- Funcion que devuelve la cantidad de jugadas ganadoras evaluando la cantidad de posiciones perdedoras que surgen a partir de esas jugadas
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p = cantidadDePosicionesPerdedoras (posiblesPosicionesSinFiltrarRepetidas p) 0


