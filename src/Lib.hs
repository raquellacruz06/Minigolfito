module Lib where
import Text.Show.Functions

laVerdad = True

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = UnaHabilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (UnaHabilidad 25 60)
todd = UnJugador "Todd" "Ned" (UnaHabilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (UnaHabilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

{-maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b-}

{-1a)Modelar los palos usados en el juego que a partir de una determinada habilidad 
generan un tiro que se compone por velocidad, precisión y altura.
El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.-}

type Palo = Habilidad -> Tiro
elPutter :: Palo
elPutter habilidad = UnTiro 10 (((*2).precisionJugador) habilidad) 0 

--La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión
laMadera :: Palo
laMadera habilidad = UnTiro 100 (((`div` 2).precisionJugador) habilidad) 5 

--Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad 
--igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). 
--Modelarlos de la forma más genérica posible.

losHierros :: Int -> Palo
losHierros n habilidad  = UnTiro ((fuerzaJugador habilidad)*n) ((precisionJugador habilidad)*n) (max 0 (n-3)) 
    --where numero = enumFromTo 1 10 

valores :: [Int]
valores = [1..10]

todosLosHierros :: [Palo]
todosLosHierros = map losHierros valores

--1b)Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.
palos :: [Palo]
palos = [elPutter, laMadera]++ todosLosHierros

--2) Definir la función golpe que dados una persona y un palo, 
--obtiene el tiro resultante de usar ese palo con las habilidades de la persona

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador


{-Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica,
 la precisión pasa a ser 100 y la altura 0. Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, 
 teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.
 {-Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a 
la altura original dividida por el largo de la laguna.
{-Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.-}
-}
-}
----Vieja versión
--type Obstaculo =  Tiro -> Tiro (se cambió por el data)
{-tunelConRampita :: Obstaculo
tunelConRampita tiro 
    | precision tiro > 90 = (cambiarVelocidad (*2). cambiarPrecision (\x-> 100). cambiarAltura (\x-> 0)) tiro
    | otherwise = UnTiro 0 0 0 False

unaLaguna :: Int -> Obstaculo
unaLaguna largoLaguna tiro
    | velocidad tiro > 80 && elem (altura tiro) [1..5] = (cambiarAltura (`div` largoLaguna)) tiro
    | otherwise = UnTiro 0 0 0 False
  unHoyo :: Obstaculo
unHoyo _ = UnTiro 0 0 0 -}


---Funciones que modifican un tiro (para intentar abstraer logica)
cambiarVelocidad :: (Int -> Int) -> Tiro -> Tiro
cambiarVelocidad funcion tiro = tiro {velocidad = (funcion.velocidad) tiro }

cambiarPrecision :: (Int -> Int) -> Tiro -> Tiro
cambiarPrecision funcion tiro = tiro {precision = (funcion.precision) tiro}

cambiarAltura :: (Int -> Int) -> Tiro -> Tiro
cambiarAltura funcion tiro = tiro {altura = (funcion.altura) tiro }

---------EJERCICIO 3 OBSTACULOS
------Abstracción de lógica con Obstaculo
{-superarObstaculos :: (Tiro->Bool)-> (Tiro-> Tiro) -> Tiro -> Tiro
superarObstaculos condicionObstaculo efecto tiro
  |condicionObstaculo tiro = efecto tiro
  | otherwise = tiroDetenido-}

---Escribimos la misma funciópn en terminos del data Obstaculo 
superarObstaculos :: Obstaculo -> Tiro -> Tiro
superarObstaculos obstaculo tiro
 | condicionObstaculo obstaculo tiro = efectoObstaculo obstaculo tiro
 | otherwise = tiroDetenido

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

data Obstaculo = UnObstaculo {
  condicionObstaculo :: (Tiro-> Bool),
  efectoObstaculo :: (Tiro-> Tiro)
}

tunelConRampita :: Obstaculo 
tunelConRampita = UnObstaculo condicionTunelConRampita efectoTunelConRampita
---No es necesario ponerle los tipos porque ya nos lo da el data
condicionTunelConRampita :: (Tiro-> Bool)
condicionTunelConRampita tiro = precision tiro > 90 && rasDelSuelo tiro

rasDelSuelo :: (Tiro-> Bool)
rasDelSuelo tiro = altura tiro == 0

efectoTunelConRampita :: (Tiro -> Tiro)
efectoTunelConRampita tiro = (cambiarVelocidad (*2). cambiarPrecision (\x-> 100). cambiarAltura (\x-> 0)) tiro
------Podríamos devolver directamente UnTiro con las condiciones dadas: Untiro (velocidad*2) 100 0

unaLaguna :: Int-> Obstaculo
unaLaguna largoLaguna = UnObstaculo condicionLaguna (efectoLaguna largoLaguna)
---Largo laguna lo pasamos sólo a efecto laguna porque sólo ahí se usa
condicionLaguna :: (Tiro-> Bool)
condicionLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro
---condicionLaguna tiro = velocidad tiro > 80 && elem (altura tiro) [1..5] Vieja versión, es mejor Between

efectoLaguna :: Int -> (Tiro -> Tiro)
efectoLaguna largoLaguna tiro = (cambiarAltura (`div` largoLaguna)) tiro

unHoyo :: Obstaculo
unHoyo = UnObstaculo condicionHoyo efectoHoyo

condicionHoyo :: (Tiro-> Bool)
condicionHoyo tiro = (between 5 20 . velocidad) tiro && rasDelSuelo tiro && precision tiro > 95

efectoHoyo :: (Tiro->Tiro)
efectoHoyo _ = tiroDetenido

{-4a) Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.-}

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (sirvePalo jugador obstaculo) palos

sirvePalo :: Jugador -> Obstaculo -> Palo ->  Bool
sirvePalo jugador obstaculo = condicionObstaculo obstaculo . golpe jugador 

{-4b) Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.-}

cuantosPuedeSuperar :: [Obstaculo] -> Tiro -> Int
cuantosPuedeSuperar [] _ = 0 --Caso base, si no recibo obstaculos, no los puedo superar
cuantosPuedeSuperar (obstaculo1:obstaculos) tiro  --- recibo lista de obstaculos
  | (condicionObstaculo obstaculo1) tiro = 1 + cuantosPuedeSuperar obstaculos (efectoObstaculo obstaculo1 tiro) ---Uso la condicion y efecto del primer obstaculo de la lista
  | otherwise = 0
--cuantosPuedeSuperar obstaculos hace la recursividad con el efecto que deja el obstaculo1 sobre el tiroDetenido

--4c) Definir paloMasUtil que recibe una persona y una lista de obstáculos
--y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.

-- foldl1 :: (a-> a -> a ) -> [a] -> a 
-- f :: Ord x => (a-> x)
--mayorSegun :: Ord x => (a-> x) -> a -> a -> a
--maximoSegun :: (a-> x) -> [a] -> a
{-maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b-}
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

paloMasUtil :: Jugador -> [Obstaculo]-> Palo
paloMasUtil jugador obstaculos = maximoSegun (cuantosPuedeSuperar obstaculos . golpe jugador) palos 

--todosLostiros :: Jugador -> [Tiro] -- Esto no me serviría porque necesito devolver un Palo
--todosLostiros jugador = map (golpe jugador) palos
--Palos es la constante que definimos más arriba

--Punto 5 Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar 
--el torneo, se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. 
--Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.

padresPerdedores :: [(Jugador, Int)] -> [String]
padresPerdedores  = map (padre.fst) . perdedores 

perdedores :: [(Jugador, Int)] -> [(Jugador, Int)]
perdedores listaJugadores = filter (/= (mejorJugador listaJugadores)) listaJugadores

mejorJugador :: [(Jugador, Int)] -> (Jugador, Int)
mejorJugador listaJugadores = maximoSegun snd listaJugadores

