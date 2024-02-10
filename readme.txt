Para el flujo principal lanzar el fichero Main.hs y ejecutar main.
ahí le pedirá path del dataset e images
dataset: 
ejemplos/RedMNIST.txt

de imágenes tiene varias:

ejemplos/img2.txt
ejemplos/img3.txt
ejemplos/img7.txt
ejemplos/img9.txt

Cuando te pida por número de filas: 28
Cuando te pida por número de columnas: 28
Número de vecinos (K) : Libre elección

También te pedirá si quieres normalizar/estandarizar la matriz: a elección

Si quiere ir viendo funciones dentro de main.hs puede ejecutar lo siguiente (también está
descrito en el código)

splitComa "1,2,3,4,5,6,7"
parseImagen 3 3 ["D","1","2","3","4","5","6","7","8","9"]

para funciones dentro de Knn.hs, lance el archivo y puede usar:

Para medir distancias entre dos imágenes:

distEuclidea (image exampleQuery) (image exampleQuery2)

Conseguir las 3 imagenes más cercanas a una imagen

let x = auxColaDistancia exampleDataset exampleQuery                     
        colaDistancia 3 x

Saber cuál es la etiqueta más repetida:

votacion [["B","B"],["A"],["D","D","D"]]  

Hacer la clasificación con K vecinos (en este ejemplo k<=5):
clasificacion exampleDataset exampleQuery k 

Si quiere ir viendo funciones dentro de Normalization.hs puede ejecutar lo siguiente (también está
descrito en el código)

Para MinMaxScaler:
elecNormalizar '1' ejemplo1

Para StandardScarler:
elecNormalizar '2' ejemplo1

Para Normalizacion L1:
elecNormalizar '3' ejemplo1
