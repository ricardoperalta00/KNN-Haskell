module Knn 
(clasificacion
) where

import Data.Array
import Data.List
import Tipos
import ColaDePrioridadConListas

--------------------------------------------------------------------------------------------
-- Algoritmo KNN
--
-- Este algoritmo, en resumen, calcula la distancia de un punto al resto del dataset
-- escoge los K puntos más cercanos y hace una votación para ver cual es la 'clasificación'
-- que más se repite
---------------------------------------------------------------------------------------------


-- ----------------------------------------------------------------------------------------
-- El flujo que he seguido para el cálculo de este algoritmo es: función para calcular la 
-- distancia euclidea, luego calcula la distancia de la mustra que estudiamos al resto, la
-- ordena y coge los K más cercanos. La función votación te devuelve la etiqueta más repetida
-- y el número de veces que aparece. Finalmente clasificación es la composición de todas las 
-- funciones
----------------------------------------------------------------------------------------------

---------------------------------------------------
--             Distancia euclidea                --
--                sqrt (x1-y1)²                  --
--    calcula la distancia entre dos imágenes    --
---------------------------------------------------

distEuclidea:: Image -> Image -> Double
distEuclidea img1 img2 = 
        (sqrt . (sum . elems)) (array ((0,0),(fi,col)) [((i,j), (img2!(i,j)-img1!(i,j))^2)
                                         | i<-[0..fi], j<-[0..col]])
  where (fi,col) = snd (bounds img1)

---------------------------------------------------------------------------------------
-- Primero tenemos una cola de prioridad que va insertando                           --
-- las distancias y al ser de prioridad se ordenan de menor                          --
-- a mayor. La función colaDistancia nos dará los k primeros                         --
-- elementos de la cola                                                              --
--                                                                                   --
-- entrada:  auxColaDistancia exampleDataset exampleQuery                            --
-- salida:  CP [(2.0,"B"),(3.3166247903554,"B"),(4.0,"C"),(4.0,"D"),(6.0,"C")]       --
--                                                                                   --
-- entrada: let x = auxColaDistancia exampleDataset exampleQuery                     --
--            colaDistancia 3 x                                                      --
-- salida:  [(2.0,"B"),(3.3166247903554,"B"),(4.0,"C")]                              --
--                                                                                   --
---------------------------------------------------------------------------------------

colaDistancia::Int -> CPrioridad (Double, Label) -> [(Double, Label)]
colaDistancia 0 cola = [] 
colaDistancia k cola = [pc] ++ (colaDistancia (k-1) rc)
    where pc = primero cola
          rc = resto cola  

auxColaDistancia :: Dataset -> Imagen -> CPrioridad (Double, Label)
auxColaDistancia [] _ = vacia
auxColaDistancia (dataXs:dataXss) img = inserta (distEuclidea (image img) (image dataXs), Tipos.label dataXs) (auxColaDistancia dataXss img)

---------------------------------------------------------------------------
-- Esta función es auxiliar para poder calcular los que más se repiten   --
-- como salida va a tener una lista de listas de la etiquetas agrupadas  --
--                                                                       --
-- un ejemplo para que se entienda:                                      --
--  entrada: trans [(2.0,"B"),(3.3166247903554,"B"),(4.0,"A"),(4.0,"D")] --
--  salida: [["B","B"],["A"],["D"]]                                      --
--                                                                       --
----------------------------------------------------------------------------
trans::[(Double, Label)] -> [[Label]]
trans x = group (map snd $ x)

---------------------------------------------------------------------------
--  Toma como entrada una lista de etiquetas como la generada en la      --
--  anterior función recorre toda la lista, calculando cuantas etiquetas --
--  de cada hay y se va quedando con la que mayor cantidad hay           --
--  ejemplo:                                                             --
--  entrada : votacion [["B","B"],["A"],["D","D","D"]]                   --
--  salida: ("D",3)                                                      --
---------------------------------------------------------------------------

votacion :: Foldable t => t [Label] -> (Label, Int)
votacion xss = foldl (\(a, b) x -> if length x > b then (x!!0,length x) else (a,b) ) ("?",0) xss

-----------------------------------------------------------------------------
--  Finalmente clasificación es la composición de todas las funciones      --
--  anteriores y además comprueba que las imagenes del dataset y la imagen --
--  a consultar tengan la misma dimensión                                  --
--                                                                         --
-- Si se quiere probar hay cargados datos con nombre exampleDataset y      --
-- exampleQuery                                                            --
-- entrada: clasificacion exampleDataset exampleQuery k                    --
-----------------------------------------------------------------------------

clasificacion :: Dataset -> Imagen -> Int -> Label
clasificacion dataS img k 
    | (mDat, nDat) == (mImg, nImg) = (fst . votacion) (trans (colaDistancia k cola ) ) 
    | otherwise =  error "Las dimensiones entre imágenes no coinciden"
    where (mDat, nDat) = snd ((bounds . image) (dataS!!0))
          (mImg, nImg) = snd (bounds (image img))
          cola = auxColaDistancia dataS img

---------------------------------------
--    Ejemplo de conjunto de datos   --
---------------------------------------
exampleDataset :: Dataset
exampleDataset = [ Imagen { Tipos.label = "D", image = listArray ((0, 0), (1, 1)) [1.0, 2.0, 3.0, 4.0] }
                 , Imagen { Tipos.label = "B", image = listArray ((0, 0), (1, 1)) [2.0, 3.0, 4.0, 5.0] }
                 , Imagen { Tipos.label = "C", image = listArray ((0, 0), (1, 1)) [5.0, 6.0, 7.0, 8.0] }
                 , Imagen { Tipos.label = "C", image = listArray ((0, 0), (1, 1)) [6.0, 7.0, 8.0, 9.0] }
                 , Imagen { Tipos.label = "B", image = listArray ((0, 0), (1, 1)) [3.0, 5.0, 6.0, 9.0] } ]

-- Ejemplo de imagen de prueba
exampleQuery :: Imagen
exampleQuery = Imagen { Tipos.label = "B", image = listArray ((0, 0), (1, 1)) [3.0, 4.0, 5.0, 6.0] }
exampleQuery2 :: Imagen
exampleQuery2 = Imagen { Tipos.label = "B", image = listArray ((0, 0), (1, 1)) [5.0, 6.0, 7.0, 8.0] }
