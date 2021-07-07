module Library where
import PdePreludat

-- Resolución

-- Punto 1 #################################################################
-- Gemas

-- type Gema = String

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show, Eq)


-- Personajes

type Habilidad = String

data Personaje = Personaje {
    edad :: Number,
    energia :: Number,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: Planeta
} deriving (Show, Eq)


-- Universo ⚠️

type Universo = [Personaje]

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso guantelete universoOriginal
    | guanteleteCompleto guantelete && ((== "uru") . material) guantelete = flip take universoOriginal $ length universoOriginal `div` 2

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete =  ((== 6) . length . gemas) guantelete


-- Punto 2 #################################################################

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex universo = any ((< 45) . edad) universo

energiaTotal :: Universo -> Number
energiaTotal universo = foldr ((+) . energia) 0 (filter (tieneMasDeNHabilidades 1) universo)

-- Punto 3 #################################################################

type Gema = Personaje -> Personaje
type Planeta = String

laMente :: Number -> Gema
laMente energiaADebilitar = quitarEnergia energiaADebilitar

elAlma :: Habilidad -> Gema
elAlma habilidad enemigo = enemigo { habilidades = sacar habilidad $ habilidades enemigo }

elEspacio :: Planeta -> Gema
elEspacio planeta enemigo = quitarEnergia 20 enemigo { planeta = planeta }

elPoder :: Gema
elPoder enemigo
    | (not . tieneMasDeNHabilidades 2) enemigo =  (quitarHabilidades . quitarEnergia (energia enemigo)) enemigo
    | otherwise = quitarEnergia (energia enemigo) enemigo

-- Suponemos que no hay menores
elTiempo :: Gema
elTiempo enemigo
    | ((> 36) . edad) enemigo = quitarEnergia 50 enemigo { edad = edad enemigo `div` 2 }
    | otherwise = quitarEnergia 50 enemigo { edad = 18 }

laGemaLoca :: Gema -> Gema
laGemaLoca gema enemigo = gema $ gema enemigo

-- Funciones auxiliares al punto 3

quitarEnergia :: Number -> Personaje -> Personaje
quitarEnergia energiaADebilitar enemigo = enemigo { energia = ((subtract energiaADebilitar) . energia) enemigo }

sacar _ []                 = []
sacar x (y:ys) | x == y    = sacar x ys
                    | otherwise = y : sacar x ys

tieneMasDeNHabilidades :: Number -> Personaje -> Bool
tieneMasDeNHabilidades cantidad = (> cantidad) . length . habilidades

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades personaje = personaje { habilidades = [] }


-- Punto 4 #################################################################

guanteleteDeGoma = Guantelete {
    material = "goma",
    gemas = [elTiempo, elAlma ("usar Mjolnir"), laGemaLoca (elAlma "programacion en Haskell") ]
}

-- Punto 5 #################################################################

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas victima = foldr ($) victima gemas

-- Punto 6 #################################################################
-- Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete enemigo = laQueMenosEnergiaMeDeja (gemas guantelete) enemigo

laQueMenosEnergiaMeDeja :: [Gema] -> Personaje -> Gema
laQueMenosEnergiaMeDeja [gema] _ = gema
laQueMenosEnergiaMeDeja (primeraGema : gemasRestantes) enemigo
    | (energia . primeraGema) enemigo < (energia . gemaGanadoraDelResto) enemigo = primeraGema
    | otherwise = gemaGanadoraDelResto
    where gemaGanadoraDelResto = laQueMenosEnergiaMeDeja gemasRestantes enemigo

laGemaQueNoHaceNada :: Gema
laGemaQueNoHaceNada personaje = personaje

-- Punto 7 #################################################################
-- Dada la función generadora de gemas y un guantelete de locos:

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas elTiempo)

-- Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:

-- -> gemaMasPoderosa guanteleteDeLocos enemigoDePrueba


-- Objetos de prueba #######################################################

enemigoDePrueba = Personaje {
    edad = 45,
    energia = 0,
    habilidades = ["", "", ""],
    nombre = "Enemigo comun",
    planeta = "Tierra"
}

guanteleteDePrueba = Guantelete {
  material = "uru",
  gemas = [(elAlma "Correr"), elTiempo, (laMente 60)]
}