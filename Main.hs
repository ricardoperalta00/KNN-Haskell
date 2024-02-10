import System.IO
import Data.Char
import Test.QuickCheck
import Data.Array
import Data.List
import System.Environment 
import System.Directory 
import Normalization
import Tipos
import Knn
import ColaDePrioridadConListas

-------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------
-- Aqui están las funciones para cargar imágenes.                                             --
-- La primera va a leer el fichero y almacenar varias imágenes en el                          --
-- tipo llamado dataset; basicamente lo que hace es leer el fichero                           --
-- quita la primera linea (no contiene informacion, es un cabecero con                        -- 
-- i x j ) separa por comas y finalmente pasa de string -> Imagen con                         --
-- la función parseImagen.                                                                    --
--                                                                                            --
-- "cargarImagen" realiza lo mismo pero como resultado da una imagen en vez                   --
-- de una lista de imágenes                                                                   --
--                                                                                            --
-- Como funciones auxiliares he creado 'splitComa' que recibe una linea del fichero           --
-- y sustituye las comas con espacios para aplicar luego la función words                     --
--                                                                                            --
--  entrada: splitComa "1,2,3,4,5,6,7"                                                        --
--  salida:  ["1","2","3","4","5","6","7"]                                                    --
--                                                                                            --
-- Finalmente la función parseImagen recibe el número de filas, columnas y la linea           --
-- ya separada por comas y lo pasa al tipo Imagen haciendo uso de Read para que transforme    --
-- String a Double                                                                            --
--                                                                                            --
-- entrada: parseImagen 3 3 ["D","1","2","3","4","5","6","7","8","9"]                         --
-- salida: Imagen {label = "D", image = array ((0,0),(2,2))                                   --
--         [((0,0),1.0),((0,1),2.0),((0,2),3.0),                                              --
--          ((1,0),4.0),((1,1),5.0),((1,2),6.0),                                              --
--          ((2,0),7.0),((2,1),8.0),((2,2),9.0)]}                                             --
------------------------------------------------------------------------------------------------

cargarImagenes :: FilePath -> Int -> Int -> IO Dataset
cargarImagenes path n m = do
  foto <- readFile path
  let lineas = tail (lines foto) 
      filas = [splitComa linea | linea <- lineas]  
  return (map (parseImagen n m) filas)

cargarImagen :: FilePath -> Int -> Int -> IO Imagen
cargarImagen path n m = do
  img <- readFile path
  let lineas = tail (lines img)
      fila = splitComa (lineas!!0)
  return (parseImagen n m fila)

--- Función para dividir lineas en comas

splitComa :: String -> [String]
splitComa = words . auxSpliComa

auxSpliComa :: String -> String
auxSpliComa [] = [] 
auxSpliComa (xs:xss) 
  | xs == ',' = [' '] ++ auxSpliComa xss
  | otherwise =  [xs] ++ auxSpliComa xss

-- Función para convertir una fila de palabras en un valor Imagen

parseImagen :: Int -> Int -> [String] -> Imagen
parseImagen n m xs = case xs of [] -> error "Compruebe txt, existen filas vacias"
                                (etiq : caracteristicas) -> Imagen { Tipos.label = etiq, 
                                image = listArray ((0, 0), (n-1, m-1)) $ map read caracteristicas }


----------------------------------------------------------
--  Modulo de entrada y salida con el flujo del         --
--  clasificador                                        -- 
--  Se compone de 3 funciones, main es la pricipal      -- 
--  LoopConsulta te realiza la consulta                 --
--  loopNormalizacion te pegunta si quieres normalizar  --
--  los datos                                           --
----------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Bienvenido al clasificador de imágenes con KNN"
  loopConsulta1
  putStrLn "¿Desea realizar de nuevo una consulta? (S/N)"
  getChar
  deNv <- getChar
  if deNv == 'S' then do
    loopConsulta1
  else
    putStrLn "Hasta Luego!"

----------------------------------------------------------
--  LoopConsulta pregunta los datos de la imagen y el   --
--  path                                                --
----------------------------------------------------------

loopConsulta1 :: IO ()
loopConsulta1 = do
  putStrLn "Introduce el path del dataset:"
  pathDataSet <- getLine
  putStrLn "Introduce el path de la imagen a clasificar:"
  pathImagen <- getLine
  existeDataSet <- doesFileExist pathDataSet
  existeImagen <- doesFileExist pathImagen
  if (existeDataSet && existeImagen) then do
    putStrLn "Introduce la dimensión de la imagen (numFilas y numColumnas) y los vecinos"
    putStrLn "Filas de la imagen: " 
    fiStr <- getLine
    putStrLn "Columnas de la imagen: "
    colStr <- getLine
    putStrLn "vecinos"
    kStr <- getLine
    let fi = read fiStr::Int
        col = read colStr::Int
        k = read kStr::Int
    dataset <- cargarImagenes pathDataSet fi col
    imagenCon <- cargarImagen pathImagen fi col
    loopNormalizacion dataset imagenCon k

  else do
    putStrLn "El archivo del dataset no existe. Por favor, ingrese un path válido."
    loopConsulta1

--------------------------------------------------
-- LoopNormalizacion te pregunta sobre el tipo  --
-- de normalización a realizar                  --                      
--------------------------------------------------
loopNormalizacion :: Dataset -> Imagen -> Int -> IO ()
loopNormalizacion dataset imagenCon k = do
    putStrLn "¿Quiere normalizar la imagen?"
    putStrLn "(0) No"
    putStrLn "(1) Normalización Min-Max"
    putStrLn "(2) Standard Scaler"
    putStrLn "(3) Normalización L1"
    controlNorm <- getChar
    if (controlNorm == '0') then do
      let clasifi = clasificacion dataset imagenCon k 
      putStrLn $ "Resultado de la clasificación: " ++ show clasifi
      putStrLn $ "Clasificación esperada " ++ show (Tipos.label imagenCon)
    else do
      if (elem controlNorm ['1','2','3'] ) then do
        let dataNorm = map (elecNormalizar controlNorm) dataset 
            imgNorm = elecNormalizar controlNorm imagenCon
            clasifiNorm = clasificacion dataNorm imgNorm k
        putStrLn $ "Resultado de la clasificación: " ++ show clasifiNorm
        putStrLn $ "Clasificación esperada " ++ show (Tipos.label imgNorm)
      else do
        putStrLn "No ha seleccionado ninguna opción correcta, vuelva a seleccionar"
        loopNormalizacion dataset imagenCon k




