-- Imports de archivos

import Data.Char (isDigit) --  isDigit para verifica si un caracter es un digito
import System.Directory (doesDirectoryExist, doesPathExist)
import System.FilePath (takeDirectory)
import System.IO
import Text.Read (Lexeme (String))
import Text.XHtml (menu)

-- Funcion que permite mostrar las opciones del menu de inicio
main :: IO ()
main = do
  putStrLn "\nBienvenido al sistema\n"
  putStrLn "Por favor seleccione la opción deseada:\n"
  putStrLn "1. Menu operacional"
  putStrLn "2. Menu general"
  putStrLn "0. Salir"
  opcion <- getLine
  case opcion of
    "1" -> menuOperacional
    "2" -> menuGeneral
    "0" -> putStrLn "Saliendo del sistema..."
    _ -> do
      putStrLn "Opcion inválida. Intentelo de nuevo."
      main

-- Formatea la informacion del usuario para mostrarla
formatearUsuario :: (String, String, String) -> String
formatearUsuario (id, nombre, puesto) = "ID: " ++ id ++ ", Nombre: " ++ nombre ++ ", Puesto: " ++ puesto

-- Funcion para el menu operacional donde muestra la información del usuario
menuOperacional :: IO ()
menuOperacional = do
  putStrLn "\nIngrese un id valido para acceder a las opciones operacionales: "
  idUsuario <- getLine

  let ruta = "C:\\Users\\joses\\Desktop\\PY02-Lenguajes\\archivosTxt\\usuarios.txt"
  contenido <- leerArchivo ruta
  let usuarios = map procesarLinea (lines contenido)

  case lookup idUsuario (map (\(id, nombre, puesto) -> (id, (nombre, puesto))) usuarios) of -- Brindado por chat
    Just (nombre, puesto) -> do
      putStrLn $ "\nBienvenido " ++ nombre ++ " (" ++ puesto ++ ")"
      putStrLn "\nOpciones del Menu Operacional:\n"
      putStrLn "1. Cargar y Mostrar mobiliario de sala"
      putStrLn "2. Cargar y Mostrar salas de reunión"
      putStrLn "3. Informe de reservas"
      putStrLn "0. Volver"
      opcion <- getLine
      case opcion of
        "1" -> cargarMobiliario
        "2" -> putStrLn "Cargando salas de reunión..."
        "3" -> putStrLn "Informe de reservas..."
        "0" -> main
        _ -> do
          putStrLn "Opción inválida. Intentelo de nuevo."
          menuOperacional
    Nothing -> do
      putStrLn "ID no válido. Por favor, intente nuevamente."
      menuOperacional

-- Funcion para el menú general
menuGeneral :: IO ()
menuGeneral = do
  putStrLn "\nMenu General\n"
  -- Aquí irian las opciones del menú general
  putStrLn "\nVolviendo al menú principal..."
  main

-- Funciones auxiliares

-- Funcion para leer el contenido de un archivo. Brindado por chat
leerArchivo :: FilePath -> IO String
leerArchivo = readFile

-- Funcion para dividir una cadena por un delimitador. Brindado por chat
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delimiter (c : cs)
  | c == delimiter = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitBy delimiter cs

-- Funcion para procesar cada línea del archivo
procesarLinea :: String -> (String, String, String)
procesarLinea linea =
  let [id, nombre, puesto] = splitBy ',' linea
   in (id, nombre, puesto)

-------------------------------------------------Cargar mobiliarios---------------------------------------------------------------

-- Funcion para agregar mobiliario a un archivo txt
agregarMobiliario :: FilePath -> String -> IO ()
agregarMobiliario ruta nuevoMobiliario = appendFile ruta (nuevoMobiliario ++ "\n")

-- Funcion para cargar mobiliario
cargarMobiliario :: IO ()
cargarMobiliario = do
  putStrLn "\nFunción para cargar mobiliario de sala\n"
  putStrLn "Ingrese la ubicacion donde desea guardar la información (ruta completa, e.g. C:\\ruta\\archivo.txt):\n"
  ruta <- getLine
  let carpeta = takeDirectory ruta
  existe <- doesDirectoryExist carpeta
  if existe
    then do
      putStrLn "\nIngrese la informacion del mobiliario:\n"

      -- Solicitud de variables individuales
      -- Validamos que no está vacío cada variable. Brindado por chat
      codigo <- validarEntrada "Código: " (\x -> not (null x)) "Error: El codigo no puede estar vacío."
      nombre <- validarEntrada "Nombre: " (\x -> not (null x) && all (not . isDigit) x) "Error: El nombre no puede estar vacío ni contener números."
      descripcion <- validarEntrada "Descripción: " (\x -> not (null x)) "Error: La descripcion no puede estar vacia."
      tipo <- validarTipo
      -- Moldeamos la informacion en una cadena
      let contenidoMobiliario = codigo ++ "," ++ nombre ++ "," ++ descripcion ++ "," ++ tipo
      -- Guardamos la informacion del mobiliario con las variables concatenadas
      agregarMobiliario ruta contenidoMobiliario
      putStrLn "Mobiliario agregado exitosamente...."
      -- Mostrar el contenido del archivo después de agregar el mobiliario
      mostrarMobiliarioGuardado ruta
      main
    else putStrLn "Error: La carpeta especificada no existe."

-- Funciones auxiliares.

-- Funciin para validar las entradas de las variables brindada por chat
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
mostrarMobiliarioGuardado :: FilePath -> IO ()
mostrarMobiliarioGuardado ruta = do
  archivoExiste <- doesPathExist ruta
  if archivoExiste
    then do
      contenido <- readFile ruta -- Lee el contenido del archivo (IO String)
      let lineas = lines contenido -- Procesa el contenido (String -> [String])
      let mobiliarios = map procesarLineaMobiliario lineas -- Convierte cada línea en una tupla con procesarLinea
      mapM_ (putStrLn . formatearMobiliario) mobiliarios -- Muestra cada mobiliario formateado
    else putStrLn "Error: El archivo especificado no existe."

-- Función para validar el tipo de "tipo"
validarTipo :: IO String
validarTipo = do
  putStrLn "Tipo (consumible, electronico o menaje): "
  tipo <- getLine
  case tipo of
    "consumible" -> return tipo
    "electronico" -> return tipo
    "menaje" -> return tipo
    _ -> do
      putStrLn "Error: Tipo invalido. Debe ser (consumible, electronico, menaje)."
      validarTipo

-- Función para procesar una linea del archivo en una tupla de mobiliario
procesarLineaMobiliario :: String -> (String, String, String, String)
procesarLineaMobiliario linea =
  let [codigo, nombre, descripcion, tipo] = splitBy ',' linea
   in (codigo, nombre, descripcion, tipo)

-- Función para formatear la información del mobiliario para mostrarla
formatearMobiliario :: (String, String, String, String) -> String
formatearMobiliario (codigo, nombre, descripcion, tipo) =
  "Codigo: " ++ codigo ++ ", Nombre: " ++ nombre ++ ", Descripción: " ++ descripcion ++ ", Tipo: " ++ tipo
