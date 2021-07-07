module Spec where
import PdePreludat
import Library
import Test.Hspec

unidad1 :: Unidad
unidad1 = Unidad "caballeria" 170 [("dulce de leche", 14)]
unidad2 :: Unidad
unidad2 = Unidad "balconeras" 240 [("dulce de leche", 14), ("aceite caliente", 999)]
unidad3 :: Unidad
unidad3 = Unidad "piqueteros" 110 [("cubiertas prendidas", 123)]
unidad4 :: Unidad
unidad4 = Unidad "la mosca" 90 [("musica", 123)]
unidad5 :: Unidad
unidad5 = Unidad "trapito" 300 [("llave", 123)]

batallon1 :: Batallon
batallon1 = [unidad1, unidad1, unidad5]
batallon2 :: Batallon
batallon2 = [unidad2, unidad4]

ciudadDePrueba :: Ciudad
ciudadDePrueba = Ciudad "Buenos Aires" 20 [unidad1, unidad2, unidad3, unidad4] [bancosEnLasPlazas, (muralla 4)]

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests del Punto 1" $ do
    it "La ciudad tiene unidades grosas" $ do
      length (unidadesGrosas ciudadDePrueba) `shouldBe` 2
    it "La ciudad tiene ataque poderoso" $ do
      ciudadDePrueba `shouldSatisfy` tieneAtaquePoderoso
    it "La sumatoria de los potencios de una lista de herramientas" $ do
      nivelTotal unidad2 `shouldBe` 1013
  describe "Tests del Punto 2" $ do
    it "Poder ofensivo de una Unidad" $ do
      poderOfensivo unidad2 `shouldBe` 1253
  describe "Tests del Punto 3" $ do
    it "Un batallon sobrevive a la pelea con otro" $ do
      sobrevive batallon1 batallon2 `shouldBe` True
    it "Un batallon NO sobrevive a la pelea con otro" $ do
      sobrevive batallon2 batallon1 `shouldBe` False
  describe "Tests del Punto 4" $ do
    it "Ciudad con muralla" $ do
      defensa (muralla 4 beijing) `shouldBe` 32
    it "Ciudad con torres de vigilancia" $ do
      defensa (torresDeVigilancia beijing) `shouldBe` 60
    it "Ciudad con centro de entrenamiento" $ do
      defensa (centroDeEntrenamiento 10 beijing) `shouldBe` 30
    it "Ciudad con bancos en las plazas" $ do
      defensa (bancosEnLasPlazas beijing) `shouldBe` 20
  describe "Tests del Punto 5" $ do
    it "Ciudad aplica todos sus sistemas defensivos" $ do
      poderDefensivo beijing `shouldBe` 32
  describe "Tests del Punto 6" $ do
    it "Ciudad sobrevive al ataque de un batallon" $ do
      sobreviveAtaque ciudadDePrueba batallon2 `shouldBe` True
    it "Ciudad NO sobrevive al ataque de un batallon" $ do
      sobreviveAtaque ciudadDePrueba batallon1 `shouldBe` False
  

