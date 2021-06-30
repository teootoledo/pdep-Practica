module Library where
import PdePreludat

-- Modelo inicial
data Jugador = UnJugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
    fuerzaJugador :: Number,
    precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
    velocidad :: Number,
    precision :: Number,
    altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]



-- ################################################################################################

-- Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y altura.
-- ✅ El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
-- ✅ La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
-- ✅ Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0).
-- ⚠️ Modelarlos de la forma más genérica posible.

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {
    velocidad = 10,
    precision = 2 * precisionJugador habilidad,
    altura = 0
    }

madera :: Palo
madera habilidad = UnTiro {
    velocidad = 100,
    precision = precisionJugador habilidad / 2,
    altura = 5
    }

hierros :: Number -> Palo
hierros n habilidad = UnTiro {
    velocidad = fuerzaJugador habilidad * n,
    precision = precisionJugador habilidad / n,
    altura = max 0 $ n - 3
    }

-- ✅ Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.

palos :: [Palo]
palos = [putter, madera] ++ map hierros [1..10]

-- ✅ Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
-- Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-- Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. En principio necesitamos representar los siguientes obstáculos:

data Obstaculo = UnObstaculo { 
    puedeSuperar :: Condicion, 
    efectoAlSuperar :: Efecto 
    }
    
type Condicion = Tiro -> Bool
type Efecto = Tiro -> Tiro

obstaculoSuperableSi :: Obstaculo -> Tiro -> Tiro
obstaculoSuperableSi obstaculo tiroOriginal
    | puedeSuperar obstaculo $ tiroOriginal = efectoAlSuperar obstaculo $ tiroOriginal
    | otherwise = tiroDetenido

alRasDelSuelo :: Tiro -> Bool
alRasDelSuelo = (0 ==) . altura

-- ✅ Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

--tunelConRampita :: Obstaculo
--tunelConRampita = obstaculoSuperableSi superaTunelConRampita efectoTunelConRampita

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Condicion
superaTunelConRampita tiro = precision tiro > 90 && alRasDelSuelo tiro

efectoTunelConRampita :: Efecto
efectoTunelConRampita tiroOriginal = tiroOriginal {
    velocidad = 2 * velocidad tiroOriginal,
    precision = 100,
    altura = 0
}

-- ✅ Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.

--laguna :: Number -> Obstaculo
--laguna largo = obstaculoSuperableSi superaLaguna (efectoLaguna largo)

laguna :: Number -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Condicion
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Number -> Efecto
efectoLaguna largo tiroOriginal = tiroOriginal { altura = altura tiroOriginal `div` largo }

-- ✅ Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

--hoyo :: Obstaculo
--hoyo tiro = obstaculoSuperableSi superaHoyo efectoHoyo tiro

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: Condicion
superaHoyo tiro = (between 5 20 . velocidad) tiro && precision tiro > 95 && alRasDelSuelo tiro

efectoHoyo :: Efecto
efectoHoyo _ = tiroDetenido

-- ✅ Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.

tiroDetenido = UnTiro 0 0 0


-- ✅ Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

-- Lambda
--palosUtiles :: Jugador -> Obstaculo -> [Palo]
--palosUtiles jugador obstaculo = filter (\ palo -> ((condicion obstaculo . palo) . habilidad) jugador) palos
-- ? Conviene abstraer a una función "leSirveParaSuperar"? O uso una lambda?

-- Abstraída
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

-- Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.

-- Recursiva
obstaculosSuperados :: Tiro -> [Obstaculo] -> Number
obstaculosSuperados tiro [] = 0
obstaculosSuperados tiro (obstaculo : obstaculosRestantes)
    | puedeSuperar obstaculo tiro = 1 + obstaculosSuperados (efectoAlSuperar obstaculo tiro) obstaculosRestantes
    | otherwise = 0


-- Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.

-- BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.

obstaculosSuperados' tiro [] = 0
obstaculosSuperados' tiro obstaculos = 
    (length . takeWhile (\ (obstaculo, tiroQueLeLlega ) -> puedeSuperar obstaculo tiroQueLeLlega) . zip obstaculos . tirosSucesivos tiro) obstaculos

tirosSucesivos :: Tiro -> [Obstaculo] -> [Tiro]
tirosSucesivos tiroOriginal obstaculos
    = foldl (\ tirosGenerados obstaculo -> tirosGenerados ++ [efectoAlSuperar obstaculo (last tirosGenerados)]) [tiroOriginal] obstaculos
    -- = map (\ obstaculo -> efectoAlSuperar obstaculo tiroOriginal) obstaculos

-- Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos
    = maximoSegun ((flip obstaculosSuperados obstaculos) . golpe jugador) palos

-- Funciones que nos daban

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> (t -> t -> t)
mayorSegun f a b
    | f a > f b = a
    | otherwise = b


-- Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.

jugadorDeTorneo = fst
puntosGanados = snd

pierdeLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdeLaApuesta puntosDeTorneo
    = (map (padre . jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador
    = (all ((< puntosGanados puntosDeUnJugador) . puntosGanados) . filter (/= puntosDeUnJugador)) puntosDeTorneo