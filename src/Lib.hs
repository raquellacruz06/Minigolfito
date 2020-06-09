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
  altura :: Int,
  superaObstaculo :: Bool
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

{-Modelar los palos usados en el juego que a partir de una determinada habilidad 
generan un tiro que se compone por velocidad, precisión y altura.
El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.-}

type Palo = Habilidad -> Tiro
elPutter :: Palo
elPutter habilidad = UnTiro 10 (((*2).precisionJugador) habilidad) 0 False

--La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión
laMadera :: Palo
laMadera habilidad = UnTiro 100 (((`div` 2).precisionJugador) habilidad) 5 False

--Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad 
--igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). 
--Modelarlos de la forma más genérica posible.

losHierros :: Int -> Palo
losHierros n habilidad  = UnTiro ((fuerzaJugador habilidad)*n) ((precisionJugador habilidad)*n) (max 0 (n-3)) False
    --where numero = enumFromTo 1 10 

valores :: [Int]
valores = [1..10]

todosLosHierros :: [Palo]
todosLosHierros = map losHierros valores

--Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.
palos :: [Palo]
palos = [elPutter, laMadera]++ todosLosHierros

{-Definir la función golpe que dados una persona y un palo, 
obtiene el tiro resultante de usar ese palo con las habilidades de la persona.-}

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador


{-Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica,
 la precisión pasa a ser 100 y la altura 0. Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, 
 teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.
-}

---Funciones que modifican un tiro si recibieran un tiro y no una habilidad
cambiarVelocidad :: (Int -> Int) -> Tiro -> Tiro
cambiarVelocidad funcion tiro = tiro {velocidad = (funcion.velocidad) tiro }

cambiarPrecision :: (Int -> Int) -> Tiro -> Tiro
cambiarPrecision funcion tiro = tiro {precision = (funcion.precision) tiro}

cambiarAltura :: (Int -> Int) -> Tiro -> Tiro
cambiarAltura funcion tiro = tiro {altura = (funcion.altura) tiro }

type Obstaculo =  Tiro -> Tiro
tunelConRampita :: Obstaculo
tunelConRampita tiro 
    | precision tiro > 90 = (cambiarVelocidad (*2). cambiarPrecision (\x-> 100). cambiarAltura (\x-> 0) . loSupera (\x-> True)) tiro
    | otherwise = UnTiro 0 0 0 False

{-Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a 
la altura original dividida por el largo de la laguna.
-}

loSupera :: (Bool -> Bool) -> Tiro -> Tiro
loSupera funcion tiro = tiro {superaObstaculo = (funcion.superaObstaculo) tiro}

unaLaguna :: Int -> Obstaculo
unaLaguna largoLaguna tiro
    | velocidad tiro > 80 && elem (altura tiro) [1..5] = (cambiarAltura (`div` largoLaguna) . loSupera (\x-> True)) tiro
    | otherwise = UnTiro 0 0 0 False

{-Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.-}

unHoyo :: Obstaculo
unHoyo tiro 
    | elem (velocidad tiro) [5..20] && precision tiro > 95 = UnTiro 0 0 0 True
    | otherwise = UnTiro 0 0 0 False


{-Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.-}

sirvePalo :: Jugador -> Obstaculo ->Palo ->  Bool
sirvePalo jugador obstaculo   = ((==True).superaObstaculo . obstaculo . golpe jugador) 

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (sirvePalo jugador obstaculo) palos

{-Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.-}

--cuantosPuedeSuperar :: [Obstaculo]-> Tiro -> Int
--cuantosPuedeSuperar obstaculos tiro = 







