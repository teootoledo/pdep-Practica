module Library where
import PdePreludat


-- Punto 1 #################################################################################
-- a
type Dolares = Number
type RecursoNatural = String

data Pais = Pais {
    ingresoPerCapita :: Dolares,
    activosPublicos :: Number,
    activosPrivados :: Number,
    recursosNaturales :: [RecursoNatural],
    deudaEnMillones :: Dolares
} deriving (Show, Eq)

-- b
namibia = Pais {
    ingresoPerCapita = 4140,
    activosPublicos = 400000,
    activosPrivados = 650000,
    recursosNaturales = ["Mineria", "Ecoturismo"],
    deudaEnMillones = 50
} 
venezuela = Pais {
    ingresoPerCapita = 12,
    activosPublicos = 100,
    activosPrivados = 10,
    recursosNaturales = ["Petroleo"],
    deudaEnMillones = 12345
} 

-- Punto 2 #################################################################################

type Receta = [Estrategia]
type Estrategia = Pais -> Pais

-- prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)
prestarleAlPais :: Dolares -> Estrategia
prestarleAlPais prestamo pais = pais { deudaEnMillones = (deudaEnMillones pais) + cobroDeIntereses prestamo }

-- Calculo el 150% del prestamo
cobroDeIntereses :: Dolares -> Dolares
cobroDeIntereses cantidad = (*) 1.5 cantidad

-- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario

reducirPuestosDeTrabajo :: Number -> Estrategia
reducirPuestosDeTrabajo empleos pais
    | empleos > 100 = (reducirIngresoPerCapita 0.8 . reducirActivoPublico empleos) pais
    | otherwise = (reducirIngresoPerCapita 0.85 . reducirActivoPublico empleos) pais

reducirActivoPublico :: Number -> Pais -> Pais
reducirActivoPublico activos pais = pais { activosPublicos = (subtract activos . activosPublicos) pais }

reducirIngresoPerCapita :: Number -> Pais -> Pais
reducirIngresoPerCapita porcentaje pais = pais { ingresoPerCapita = ((*) porcentaje . ingresoPerCapita) pais }

-- darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.

darRecursoNatural :: RecursoNatural -> Estrategia
darRecursoNatural recurso pais = reducirDeuda 2 pais { recursosNaturales = (sacar recurso . recursosNaturales) pais }

reducirDeuda :: Dolares -> Pais -> Pais
reducirDeuda descuento pais = pais { deudaEnMillones = (subtract descuento . deudaEnMillones) pais }

sacar _ [] = []
sacar x (y:ys)
    | x == y    = sacar x ys
    | otherwise = y : sacar x ys

sacar' elemento lista = filter (not . (==) elemento) lista

sacar'' elemento lista = filter (/= elemento) lista

-- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.

blindar :: Estrategia
blindar pais = prestarleAlPais ((*) 0.5 $ pBI pais) pais {activosPublicos = subtract 500 $ activosPublicos pais}

pBI :: Pais -> Number
pBI pais = ((*) (totalActivos pais) . ingresoPerCapita) pais

totalActivos :: Pais -> Number
totalActivos pais = ((+) (activosPublicos pais) . activosPrivados) pais

-- Punto 3 #################################################################################

-- Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.
receta :: Receta
receta = [ (prestarleAlPais 200), (darRecursoNatural "Mineria") ]

-- Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.
namibiaEndeudado = foldr ($) namibia receta

-- Punto 4 #################################################################################
listaPaises :: [Pais]
listaPaises = [namibia, venezuela]

-- Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.
zafan :: [Pais] -> [Pais]
zafan = filter (\ pais -> (elem "Petroleo" . recursosNaturales) pais)

-- Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
sumaDeDeudas :: [Pais] -> Number
sumaDeDeudas paises = foldr ((+) . deudaEnMillones) 0 paises

-- Indicar en dónde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver el requerimiento.

-- Punto 5 #################################################################################
-- Debe resolver este punto con recursividad: dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor. Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per cápita por la población activa (privada y pública). 

recetas :: [Receta]
recetas = [receta1, receta2, receta3]

receta1 :: Receta
receta1 = [ (reducirPuestosDeTrabajo 500), (darRecursoNatural "Mineria") ]
receta2 :: Receta
receta2 = [ (prestarleAlPais 500), (darRecursoNatural "Ecoturismo"), blindar ]
receta3 :: Receta
receta3 = [ (prestarleAlPais 100) ]

recetasOrdenadas :: [Receta] -> Pais -> Bool
recetasOrdenadas [receta] _ = True
recetasOrdenadas (recetaActual : recetaSiguiente : recetasRestantes) pais
    = (revisarPBI recetaActual) < (revisarPBI recetaSiguiente) && recetasOrdenadas (recetaSiguiente : recetasRestantes) pais
    where revisarPBI receta = pBI $ aplicarReceta receta pais

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta

{- 
laQueMenosEnergiaMeDeja :: [Gema] -> Personaje -> Gema
laQueMenosEnergiaMeDeja [gema] _ = gema
laQueMenosEnergiaMeDeja (primeraGema : gemasRestantes) enemigo
    | (energia . primeraGema) enemigo < (energia . gemaGanadoraDelResto) enemigo = primeraGema
    | otherwise = gemaGanadoraDelResto
    where gemaGanadoraDelResto = laQueMenosEnergiaMeDeja gemasRestantes enemigo
-}

-- Punto 6 #################################################################################

-- Si un país tiene infinitos recursos naturales, modelado con esta función

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

-- ¿qué sucede evaluamos la función 4a con ese país?
-- Como zafan busca si existe "Petroleo" y este no está en la lista de recursos, la funcion diverge. (SE ROMPE)

-- y con la 4b?
-- Emplea la "lazy evaluation" y puede aplicarse ya que no requiere utilizar ni evaluar la lista infinita.

-- Justifique ambos puntos relacionándolos con algún concepto.
