module Library where
import PdePreludat

-- Punto 1
-- Ingrediente tiene calorías

data Ingrediente = Ingrediente {
    calorias :: Number
} deriving (Show)

-- Chocolate
type Gramos = Number
type Porcentaje = Number

data Chocolate = Chocolate {
    nombre :: String,
    porcentajeCacao :: Porcentaje,
    porcentajeAzucar :: Porcentaje,
    gramaje :: Gramos,
    ingredientes :: [Ingrediente]
} deriving (Show)

-- Punto 1
-- Cálculo del precio
precio :: Chocolate -> Number
precio chocolate
--  Gramaje del chocolate multiplicado por el precio premium
    | chocolateAmargo chocolate                     = precioPremium chocolate * gramaje chocolate
--  Si tiene más de 4 ingredientes, su precio es $8 por la cantidad de ingredientes que tiene
    | ((> 4) . cantidadDeIngredientes) chocolate    = 8 * cantidadDeIngredientes chocolate
--  Caso contrario, $1.5 por gramo
    | otherwise                                     = 1.5 * gramaje chocolate

cantidadDeIngredientes :: Chocolate -> Number
cantidadDeIngredientes = length . ingredientes

-- Chocolates con más del 60% de cacao
chocolateAmargo :: Chocolate -> Bool
chocolateAmargo = (> 60) . porcentajeCacao 

-- precioPremium varía si es diabético
precioPremium :: Chocolate -> Number
precioPremium chocolate
    | aptoParaDiabeticos chocolate  = 8 * gramaje chocolate
    | otherwise                     = 5 * gramaje chocolate

-- El chocolate tiene porcentaje 0 de azucar
aptoParaDiabeticos :: Chocolate -> Bool
aptoParaDiabeticos = (== 0) . porcentajeAzucar


-- Punto 2
-- esBombonAsesino si al menos un ingrediente tiene más de 200 calorías
esBombonAsesino :: Chocolate -> Bool
esBombonAsesino = any ((> 200) . calorias) . ingredientes

totalCalorias :: Chocolate -> Number
totalCalorias = sum . map calorias . ingredientes
-- Alternativas con doble interación 👎
-- totalCalorias = foldr ((+) . calorias) 0 . ingredientes
-- totalCalorias = foldr1 (+) . map calorias . ingredientes


type Caja = [Chocolate]

aptoParaNinies :: Caja -> Caja
aptoParaNinies = take 3 . filter (not . esBombonAsesino)



-- Punto 3

type Transformacion = Chocolate -> Chocolate
type UnidadesDeFrute = Number

frutalizado :: Number -> Transformacion
frutalizado unidades chocolate = agregarIngrediente (unidades * 2) chocolate
    
agregarIngrediente :: Number -> Transformacion
agregarIngrediente calorias chocolate = chocolate {
    ingredientes = ingredientes chocolate ++ [Ingrediente calorias]
}

dulceDeLeche :: Transformacion
dulceDeLeche chocolate = agregarIngrediente 220 chocolate {
    nombre = nombre chocolate ++ " tentación"
}

-- Celia crucera es lo mismo que agregarAzucar
celiaCrucera :: Number -> Transformacion
celiaCrucera = agregarAzucar

agregarAzucar :: Number -> Transformacion
agregarAzucar porcentajeAzucarAAgregar chocolate = chocolate {
    porcentajeAzucar = porcentajeAzucar chocolate + porcentajeAzucarAAgregar
}

embriagadora :: Number -> Transformacion
embriagadora gradosDeAlcohol = (agregarIngrediente (min 30 gradosDeAlcohol) . agregarAzucar 20)


-- Punto 4

type Receta = [Transformacion]

receta :: Receta
receta = [ frutalizado 10, dulceDeLeche, embriagadora 32]


-- Punto 5

-- preparacionChocolate :: Receta -> Transformacion
-- preparacionChocolate receta chocolate = foldr ($) chocolate receta

preparacionChocolate :: Chocolate -> Receta -> Chocolate
preparacionChocolate = foldr ($)



-- Punto 6

type CriterioDeAceptacionDeIngrediente = Ingrediente -> Bool

data Persona = Persona {
    nivelDeSaturacion :: Number,
    criterio :: CriterioDeAceptacionDeIngrediente
}

-- hastaAcaLlegue :: Persona -> Caja -> Caja