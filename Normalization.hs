module Normalization 
(
elecNormalizar
) where


-------------------------------------------------------------------------------------
--   En este módulo se implementan los métodos de normalización de matrices        --
-------------------------------------------------------------------------------------

import Data.List
import Data.Array
import Tipos

--- Ejemplo para probar los tipo de escalado

ejemplo1 :: Imagen
ejemplo1 = Imagen { Tipos.label = "B", image = listArray ((0, 0), (2, 2)) [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0] }

ejemplo2 :: Imagen
ejemplo2 = Imagen { Tipos.label = "B", image = listArray ((0, 0), (3, 1)) [0.0,0.0,1.0,0.0,0.0,1.0,1.0,1.0] }


-----------------------------------------------------------------------------------
--  Esta función servirá para en E/S no tener que usar tantos if's               --
--  Basicamente depende del dato de entrada hace un tipo de normalización        --           
----------------------------------------------------------------------------------- 

elecNormalizar:: Char -> Imagen -> Imagen
elecNormalizar '1' img = normMinMax img
elecNormalizar '2' img = standarScaler img
elecNormalizar '3' img = normL1 img
elecNormalizar _ img = error "No hay más Normalizaciones"
                                               



----------------------------------------------------
--  Esta es la normalización lineal, se basa en:  --
--       x = (x-min(x)) / (max(x)-min(x))         --
----------------------------------------------------
normMinMax :: Imagen -> Imagen
normMinMax img = Imagen { label = label img, image = array ((0,0), (fi,col)) [((i,j), (arrImg!(i,j) - minxs) / (maxxs-minxs) ) 
                                                                              | i <- [0..fi], j <- [0..col]]}
  where arrImg = image img
        (fi,col) = snd (bounds (arrImg))          
        lista = elems (arrImg)
        maxxs = maximum lista
        minxs = minimum lista

-------------------------------------------------------
--  Esta es la normalización Manhattan, se basa en:  --
--  x = (x)/ sum (|x|)                               --
-------------------------------------------------------

normL1 :: Imagen -> Imagen
normL1 img = Imagen { label = label img, image = array ((0,0), (fi,col)) [((i,j), arrImg!(i,j) / (suma i) ) 
                                                                              | i <- [0..fi], j <- [0..col]]}
  where arrImg = image img
        (fi,col) = snd (bounds (arrImg))          
        lista i = elems (normL_aux arrImg i)
        suma i = sum (map abs (lista i))

-- Función auxilar que nos ayuda a coger la fila i de la matriz
normL_aux :: Image -> Int -> Image
normL_aux mat i = array ((0,0),(0,col)) [((0,j), mat!(i,j)) | j <- [0..col]]
  where (fi,col) = snd (bounds mat)

----------------------------------------------------------------
--                      Escalado Estandard                    --
--            x = (x - media) / desviacion standard           --
--         desviación standard: Sqrt (sum (x-media)/long)     --
----------------------------------------------------------------

standarScaler :: Imagen -> Imagen
standarScaler img = Imagen { label = label img, image = array ((0,0), (fi,col)) [((i,j), arrImg!(i,j) - (media)/ (desv) ) 
                                                                              | i <- [0..fi], j <- [0..col]]}
  where arrImg = image img
        (fi,col) = snd (bounds (arrImg))          
        lista = elems (arrImg)
        longi = genericLength lista
        media = sum (lista) / longi 
        desv  = sqrt (sum (map (\x -> (x - media)^2) (lista)) / (longi ) )


