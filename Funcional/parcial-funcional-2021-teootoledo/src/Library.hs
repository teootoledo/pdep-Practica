module Library where
import PdePreludat

-- Aquí va la solución del parcial. ¡Éxitos!





-- Punto 1 ✅ ##################################################################################

-- ✅ Modelar las abstracciones necesarias para cumplir con la descripción del dominio y saber que:

type Potencio = Number
type Nombre = String
type Herramienta = (Nombre, Potencio)
type Batallon = [Unidad]

data Unidad = Unidad {
    tipo :: String,
    ataque :: Potencio,
    herramientas :: [Herramienta]
} deriving (Show, Eq)

type Defencio = Number
type Sistema = Ciudad -> Ciudad

data Ciudad = Ciudad {
    nombre :: String,
    defensa :: Defencio,
    batallon :: Batallon,
    sistemasDefensivos :: [Sistema]
} deriving (Show, Eq)

-- ✅ Dada una ciudad necesitamos saber cuáles son los tipos de unidades grosas. Una unidad es grosa si tiene más de 160 potencios de nivel de ataque.

unidadesGrosas :: Ciudad -> Batallon
unidadesGrosas ciudad = filter (ataqueMayorA 160) $ batallon ciudad

ataqueMayorA :: Potencio -> Unidad -> Bool
ataqueMayorA potencios unidad = potencios < ataque unidad

-- ✅ Dada una ciudad queremos saber si tiene un ataque poderoso. Esto ocurre si las primeras 3 unidades de la ciudad tienen más de 100 potencios de nivel de ataque.

tieneAtaquePoderoso :: Ciudad -> Bool
tieneAtaquePoderoso ciudad = all (ataqueMayorA 100) $ (take 3 . batallon) ciudad

-- ✅ Dada una unidad queremos saber el nivel total que le aportan las herramientas. Esto es la sumatoria de nivel de potencios que aporta cada una.

nivelTotal :: Unidad -> Potencio
nivelTotal unidad = foldr ((+) . snd) 0 $ herramientas unidad

-- ✅ Este punto debe resolverse utilizando funciones de orden superior.





-- Punto 2 ✅ ##################################################################################

-- Ahora necesitamos determinar el valor del poderOfensivo de una unidad.

-- ✅ Esto se da con un cálculo que se define en base a una serie de condiciones. Si tiene más de 5 herramientas el poder ofensivo es 100 potencios sumado al nivel de ataque propio más el nivel que aporta cada herramienta.

-- ✅ De lo contrario, si es del tipo "caballería" el cálculo es el doble de la sumatoria del nivel de ataque propio y el nivel que aporta cada herramienta.

-- ✅ En caso contrario solamente es el nivel propio de ataque más el nivel que aporta cada herramienta.

poderOfensivo :: Unidad -> Number
poderOfensivo unidad
    | 5 < (length . herramientas) unidad = 100 + herramientasAplicadas
    | tipo unidad == "caballeria" = (*) 2 $ herramientasAplicadas
    | otherwise = herramientasAplicadas
    where herramientasAplicadas = ataque unidad + nivelTotal unidad

-- ✅ En este punto deben evitar lógica y código repetido.





-- Punto 3 ✅ ##################################################################################

-- Queremos saber si un batallón sobrevive al ataque de otro.

-- ✅ Esto ocurre cuando el batallón rival se queda sin unidades. Si por el contrario el batallón propio es el que se queda sin unidades, entonces no se cumple la condición.

-- Los batallones se deben ir enfrentando en batallas individuales a las unidades en el orden que se encuentran. 

-- Por ejemplo, la primera unidad de ambos bandos se enfrentan comparando su poder ofensivo. Quien supera en valor al contrincante es el sobreviviente y será quien siga para la próxima ronda.

sobrevive :: Batallon -> Batallon -> Bool
sobrevive [] _ = False
sobrevive _ [] = True
sobrevive (primeraUnidad : restoDelBatallon) (primerEnemigo : restoBatallonEnemigo)
    | ataqueMayorA (ataque primeraUnidad) primerEnemigo = sobrevive restoDelBatallon (primerEnemigo : restoBatallonEnemigo)
    | otherwise = sobrevive (primeraUnidad : restoDelBatallon) restoBatallonEnemigo

-- ✅ Resolver el punto utilizando recursividad. No puede utilizar recursividad en ningún otro punto de su solución.





-- Punto 4 ✅ ##################################################################################

-- Como habíamos comentado con anterioridad, las ciudades cuentan con diversos sistemas de defensa que se pueden ir agregando de forma dinámica. Además a futuro podemos implementar nuevos sistemas de defensa para complejizar el juego. Los sistemas de defensa que tenemos son:

-- ✅ La muralla que dada la altura de la misma aumenta el nivel defensivo de la ciudad en el triple de su altura y le agrega el prefijo "La gran ciudad de" al nombre de la ciudad. Ejemplo: A Beijing que tiene 20 "defensios" de nivel defensivo le agregamos una muralla de 4 metros, esto incrementa 12 "defensios" su nivel, de manera que pasa a tener 32 "defensios".

beijing = Ciudad "Beijing" 20 [] [(muralla 4)]

muralla :: Number -> Sistema
muralla altura ciudad = incrementarDefensa ((*) 3 altura) ciudad { nombre = "La gran ciudad de " ++ nombre ciudad }

-- ✅ Las torres de vigilancia que nos permiten preparar a la ciudad ante un posible ataque con antelación. Esto incrementa el nivel de defensa en 40 "defensios".

torresDeVigilancia :: Sistema
torresDeVigilancia ciudad = incrementarDefensa 40 ciudad

-- ✅ El centro de entrenamiento que incrementa un determinado valor el nivel de ataque propio que tiene cada unidad que posee su batallón de defensa. Además sube 10 "defensios" el nivel de defensa de la ciudad.

centroDeEntrenamiento :: Potencio -> Sistema
centroDeEntrenamiento potencios ciudad = incrementarDefensa 10 ciudad { batallon = mejorarBatallon potencios $ batallon ciudad }


-- ✅ Instalar bancos en las plazas. Esto no incrementa el nivel de defensa bajo ningún punto de vista.

bancosEnLasPlazas :: Sistema
bancosEnLasPlazas = id

-- ✅ En este punto deben evitar lógica y código repetido.

incrementarDefensa :: Defencio -> Ciudad -> Ciudad
incrementarDefensa defencios ciudad = ciudad { defensa = defensa ciudad + defencios}

mejorarBatallon :: Potencio -> Batallon -> Batallon
mejorarBatallon potencios batallon = map (incrementarAtaque potencios) batallon

incrementarAtaque :: Potencio -> Unidad -> Unidad
incrementarAtaque potencios unidad = unidad { ataque = ataque unidad + potencios }





-- Punto 5 ✅ ##################################################################################

-- Modelar la función poderDefensivo que nos da el valor del nivel de defensa luego de activar todos los sistemas de defensa en la ciudad. Dar un ejemplo de cómo se invocará a la ciudad Persepolis con un nivel de defensa de 10, sin batallón de defensa pero con una muralla de 5 metros, un centro de entrenamiento de 15 "defensios" adicionales para las unidades del batallón y una torre de vigilancia.

persepolis = Ciudad "Persepolis" 10 [] [(muralla 5), (centroDeEntrenamiento 15), torresDeVigilancia]

poderDefensivo :: Ciudad -> Defencio
poderDefensivo ciudad = (defensa . aplicarSistemasDefensivos) ciudad

aplicarSistemasDefensivos :: Ciudad -> Ciudad
aplicarSistemasDefensivos ciudad = foldr ($) ciudad $ sistemasDefensivos ciudad





-- Punto 6 ✅ ##################################################################################

-- Saber si una ciudad sobrevive al ataque de un batallón. Esto ocurre con dos condiciones: el batallón de defensa sobrevive al ataque del batallón atacante o bien el poder defensivo de la ciudad es superior al poder total de ataque del batallón.

sobreviveAtaque :: Ciudad -> Batallon -> Bool
sobreviveAtaque ciudad enemigo = (sobrevive $ batallon ciudad) enemigo || poderDefensivo ciudad > poderTotalDeAtaque enemigo 

poderTotalDeAtaque :: Batallon -> Potencio
poderTotalDeAtaque batallon = foldr ((+) . poderOfensivo) 0 batallon





-- Punto 7 ✅ ##################################################################################

-- ✅ Si una ciudad tiene un batallón de infinitas unidades, que ocurre en el caso de solicitar los tipos de unidades grosas que posee y saber si tiene un ataque poderoso. Justifique su respuesta en base a los contenidos teóricos vistos en clase.

-- ⚪️ Utilizando la función "unidadesGrosas", esta diverge y rompe. Esto debido a que debe iterar toda la lista infinita de Unidades para saber cuáles son grosas.

-- ⚪️ Utilizando la función "tieneAtaquePoderoso", haskell aprovecha su característica "lazy evaluation" y la función converge. Por lo tanto puede ser evaluada. Solo romperá la consola al momento de mostrar la ciudad.