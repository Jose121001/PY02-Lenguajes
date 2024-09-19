-- CargarYMostrarMobiliario.hs
module CargarYMostrarMobiliario (cargarMobiliario, mostrarMobiliario) where

import Data.Array.ST (runSTArray)
import Data.Char (isDigit)
import System.Directory (doesDirectoryExist, doesPathExist)
import System.FilePath (takeDirectory)
import System.IO

-- Funcion para agregar moiliario a un archivo txt
agregarMobiliario :: FilePath -> String -> IO ()
agregarMobiliario ruta nuevoMobiliario = appendFile ruta (nuevoMobiliario ++ "\n")

-- Funcion para cargar mobiliario
cargarMobiliario :: IO ()
cargarMobiliario = do
  putStrLn "\nFuncion para cargar mobiliario de sala\n"
  putStrLn "Ingrese la ubicacion donde desea guardar la informacion (ruta completa, e.g. C:\\ruta\\archivo.txt):\n"
  ruta <- getLine
  let carpeta = takeDirectory ruta
  existe <- doesDirectoryExist carpeta
  if existe
    then do
      putStrLn "\nIngrese la información del mobiliario:\n"

      -- Solicitud de variables individuales
      -- Validamos que no esta vacio cada variable
      codigo <- validarEntrada "Codigo: " (\x -> not (null x)) "Error: El código no puede estar vacío."
      nombre <- validarEntrada "Nombre: " (\x -> not (null x) && all (not . isDigit) x) "Error: El nombre no puede estar vacío ni contener números."
      descripcion <- validarEntrada "Descripción: " (\x -> not (null x)) "Error: La descripción no puede estar vacía."
      tipo <- validarTipo
      -- Moldeamos la info en una cadena
      let contenidoMobiliario = codigo ++ "," ++ nombre ++ "," ++ descripcion ++ "," ++ tipo
      -- Guardamos la informacion del mobiliario con las variables concat
      agregarMobiliario ruta contenidoMobiliario
      putStrLn "Mobiliario agregado exitosamente...."
      -- Mostrar el contenido del archivo despues de agregar el mobiliario
      mostrarMobiliario ruta
    else putStrLn "Error: La carpeta especificada no existe."

-- Funciones auxiliares.

-- Funcion para validar las entradas de las variables brindada por chat
validarEntrada :: String -> (String -> Bool) -> String -> IO String
validarEntrada mensaje condicion mensajeError = do
  putStrLn mensaje
  entrada <- getLine
  if condicion entrada
    then return entrada
    else do
      putStrLn mensajeError
      validarEntrada mensaje condicion mensajeError

-- Funcion para mostrar todos los mobiliarios registrados
mostrarMobiliario :: FilePath -> IO ()
mostrarMobiliario ruta = do
  archivoExiste <- doesPathExist ruta
  if archivoExiste
    then do
      contenido <- readFile ruta -- Lee el contenido del archivo (IO String)
      let lineas = lines contenido -- Procesa el contenido (String -> [String])
      let mobiliarios = map procesarLinea lineas -- Convierte cada línea en una tupla con procesarLinea
      mapM_ (putStrLn . formatearMobiliario) mobiliarios -- Muestra cada mobiliario formateado
    else putStrLn "Error: El archivo especificado no existe."

-- Funcion para validar el tipo de "tipo"
validarTipo :: IO String
validarTipo = do
  putStrLn "Tipo (consumible, electronico o menaje): "
  tipo <- getLine
  case tipo of
    "consumible" -> return tipo
    "electronico" -> return tipo
    "menaje" -> return tipo
    _ -> do
      putStrLn "Error: Tipo invalido. Debe ser (consumible,electronico,menaje)."
      validarTipo

-- Funcion para procesar una línea del archivo en una tupla de mobiliario
procesarLinea :: String -> (String, String, String, String)
procesarLinea linea =
  let [codigo, nombre, descripcion, tipo] = splitBy ',' linea
   in (codigo, nombre, descripcion, tipo)

-- Funcion para formatear la información del mobiliario para mostrarla
formatearMobiliario :: (String, String, String, String) -> String
formatearMobiliario (codigo, nombre, descripcion, tipo) =
  "Código: " ++ codigo ++ ", Nombre: " ++ nombre ++ ", Descripción: " ++ descripcion ++ ", Tipo: " ++ tipo

-- Split simple por comas brindada por chat
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delimiter (c : cs)
  | c == delimiter = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitBy delimiter cs
