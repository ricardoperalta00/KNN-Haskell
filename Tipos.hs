module Tipos where
import Data.Array

----------------------------------------------------------------------------------------------------
-- He elegido este tipo de datos por que las imágenes las voy a tratar como una matriz con datos --
-- aparte, voy a trabajar con imágenes ya etiquetadas                                            --
----------------------------------------------------------------------------------------------------

type Feature = Double
type Image = Array (Int,Int) Feature
type Label = String

data Imagen = Imagen { label :: Label , image :: Image } deriving Show

type Dataset = [Imagen]