-- Imports de archivos

import CargarYMostrarMobiliario
import System.IO
import Text.Read (Lexeme (String))
import Text.XHtml (menu)

-- Funcion que permite mostrar las opciones del menu de inicio
main :: IO ()
main = do
  putStrLn "\nBienvenido al sistema\n"
  putStrLn "Por favor seleccione la opcion deseada:\n"
  putStrLn "1. Menu operacional"
  putStrLn "2. Menu general"
  putStrLn "0. Salir"
  opcion <- getLine
  case opcion of
    "1" -> menuOperacional
    "2" -> menuGeneral
    "0" -> putStrLn "Saliendo del sistema..."
    _ -> do
      putStrLn "Opcion invalida. Inténtelo de nuevo."
      main

-- Formatea la información del usuario para mostrarla
formatearUsuario :: (String, String, String) -> String
formatearUsuario (id, nombre, puesto) = "ID: " ++ id ++ ", Nombre: " ++ nombre ++ ", Puesto: " ++ puesto

-- Función para el menú operacional donde muestra la informacion de usuario
menuOperacional :: IO ()
menuOperacional = do
  putStrLn "\nIngrese un id valido para acceder a las opciones operacionales: "
  idUsuario <- getLine

  let ruta = "C:\\Users\\joses\\Desktop\\PY02-Lenguajes\\archivosTxt\\usuarios.txt"
  contenido <- leerArchivo ruta
  let usuarios = map procesarLinea (lines contenido)

  case lookup idUsuario (map (\(id, nombre, puesto) -> (id, (nombre, puesto))) usuarios) of
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
          putStrLn "Opción inválida. Inténtelo de nuevo."
          menuOperacional
    Nothing -> do
      putStrLn "ID no válido. Por favor, intente nuevamente."
      menuOperacional

-- Función para el menú general
menuGeneral :: IO ()
menuGeneral = do
  putStrLn "\nMenu General\n"
  -- Aquí irian las opciones del menu general
  putStrLn "\nVolviendo al menú principal..."
  main

-- Funciones auxiliares

-- Función para leer el contenido de un archivo codigo brindado por chat
leerArchivo :: FilePath -> IO String
leerArchivo = readFile

-- Función para dividir una cadena por un delimitador
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delimiter (c : cs)
  | c == delimiter = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitBy delimiter cs

-- Función para procesar cada línea del archivo
procesarLinea :: String -> (String, String, String)
procesarLinea linea =
  let [id, nombre, puesto] = splitBy ',' linea
   in (id, nombre, puesto)