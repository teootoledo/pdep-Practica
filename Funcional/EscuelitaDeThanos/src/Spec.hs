module Spec where
import PdePreludat
import Library
import Test.Hspec

personaje1 = Personaje {
  edad = 44,
    energia = 100,
    habilidades = [""],
    nombre = "ironMan",
    planeta = "La tierra"
}
personaje2 = Personaje {
  edad = 46,
    energia = 100,
    habilidades = ["", ""],
    nombre = "ironMan",
    planeta = "La tierra"
}
personaje3 = Personaje {
  edad = 46,
    energia = 100,
    habilidades = ["", ""],
    nombre = "ironMan",
    planeta = "La tierra"
}
personaje4 = Personaje {
  edad = 50,
    energia = 0,
    habilidades = [],
    nombre = "ironMan",
    planeta = "La tierra"
}
personaje5 = Personaje {
  edad = 46,
    energia = 100,
    habilidades = ["Correr", "Dormir"],
    nombre = "ironMan",
    planeta = "La tierra"
}

guanteleteConGemas = Guantelete {
  material = "uru",
  gemas = [(elAlma "Saber haskell"), elTiempo, (elEspacio "Tierra"), elPoder, (laGemaLoca elTiempo), (laMente 3)]
}

{- enemigoDePrueba = Personaje {
    edad = 45,
    energia = 0,
    habilidades = ["", "", ""],
    nombre = "Enemigo comun",
    planeta = "Tierra"
}

guanteleteDePrueba = Guantelete {
  material = "uru",
  gemas = listaDeGemas2
} -}

listaDeGemas = [(elAlma "Correr"), elTiempo]
listaDeGemas2 = [(elAlma "Correr"), elTiempo, (laMente 10)]

universoDePrueba = [personaje1, personaje2, personaje3, personaje4, personaje5]
universoDePrueba2 = [personaje2, personaje3, personaje4, personaje5]



correrTests :: IO ()
correrTests = hspec $ do
  describe "Punto 1" $ do
    it "Test de la función chasquear universo" $ do
      (length . chasquearUniverso guanteleteConGemas) universoDePrueba `shouldBe` 2
  describe "Punto 2" $ do
    it "Test de la función esAptoParaPendex" $ do
      universoDePrueba `shouldSatisfy` esAptoParaPendex
    it "Test de la función esAptoParaPendex" $ do
      universoDePrueba2 `shouldNotSatisfy` esAptoParaPendex
    it "Test de la función energiaTotal" $ do
      energiaTotal universoDePrueba `shouldBe` 300
  describe "Punto 3" $ do
    it "Test de la gema elAlma" $ do
      (length . chasquearUniverso guanteleteConGemas) universoDePrueba `shouldBe` 2
    it "Test de la gema laMente" $ do
      (length . chasquearUniverso guanteleteConGemas) universoDePrueba `shouldBe` 2
    it "Test de la gema elEspacio" $ do
      (length . chasquearUniverso guanteleteConGemas) universoDePrueba `shouldBe` 2
    it "Test de la gema elPoder" $ do
      (length . chasquearUniverso guanteleteConGemas) universoDePrueba `shouldBe` 2
    it "Test de la gema elTiempo" $ do
      (length . chasquearUniverso guanteleteConGemas) universoDePrueba `shouldBe` 2
    it "Test de laGemaLoca" $ do
      (length . chasquearUniverso guanteleteConGemas) universoDePrueba `shouldBe` 2
  describe "Punto 5" $ do
    it "Test de la función utilizar (con elAlma(Correr))" $ do
      (length . habilidades . utilizar listaDeGemas) personaje5 `shouldBe` 1
    it "Test de la función utilizar (con elTiempo)" $ do
      (edad . utilizar listaDeGemas) personaje5 `shouldBe` 23
{-   describe "Punto 6" $ do
    it "Test de la función utilizar (con elAlma(Correr))" $ do
      energia (gemaMasPoderosa guanteleteDePrueba enemigoDePrueba $ enemigoDePrueba) `shouldBe` (-50) -}