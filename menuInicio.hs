-- Imports de archivos
import CargarYMostrarMobiliario
import System.IO
import Text.Read (Lexeme (String))
import Text.XHtml (menu)

-- Funcion que permite mostrar las opciones del menu de inicio
main :: IO () -- Void
main = do
  putStrLn "Bienvenido al sistema de inicio"
  putStrLn "\nPor favor seleccione la opcion deseada:\n "
  putStrLn "1. Informe de usuarios."
  putStrLn "2. Ingresar al menu principal."
  putStrLn "0. Salir."
  opcion <- getLine
  case opcion of
    "1" -> informeUsuarios
    "2" -> menuPrincipal
    "0" -> putStrLn "Saliendo del sistema..."
    _ -> do
      putStrLn "Opcion invalida. intentelo de nuevo."
      main

-- Funcion del informe del usuario el cual me dara la info registrada
informeUsuarios :: IO ()
informeUsuarios = do
  putStrLn "\nInformacion de usuarios:\n "

-- Funcion que me mostrara las opciones dle menu principal
menuPrincipal :: IO ()
menuPrincipal = do
  putStrLn "\nIngrese un id valido: "
  idUsuario <- getLine

  let ruta = "C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\PY02-Lenguajes\\archivosTxt\\usuarios.txt"
  contenido <- leerArchivo ruta
  let ids = lines contenido -- separa el contenido por lineas
  if idValido idUsuario ids
    then do
      putStrLn "\nBienvenido al menu principal.\n"
      putStrLn "Ingrese la opcion deseada: \n"
      putStrLn "1. Menu operacional."
      putStrLn "2. Menu general."
      opcion <- getLine
      case opcion of
        "1" -> menuOperacional
        "2" -> menuGeneral
        "3" -> main
        _ -> do
          putStrLn "Opcion invalida. Por favor, intente nuevamente."
          menuPrincipal
    else do
      putStrLn "ID no valido. Por favor, intente nuevamente."
      menuPrincipal

-- Función para el menú operacional
menuOperacional :: IO ()
menuOperacional = do
  putStrLn "\nMenu Operacional\n"
  putStrLn "1. Cargar y Mostrar mobiliario de sala"
  putStrLn "2. Cargar y Mostrar salas de reunión"
  putStrLn "3. Informe de reservas"
  putStrLn "0. Volver"
  opcion <- getLine
  case opcion of
    "1" -> cargarMobiliario
    "2" -> putStr "Cargando salas...3"
    "3" -> putStr "Informe de reservas..."
    "0" -> menuPrincipal
    _ -> do
      putStrLn "Opcion invalida. Por favor ingrese de nuevo"
      menuOperacional
  -- Aquí irían las opciones del menú operacional
  putStrLn "Volviendo al menú operacional...\n"
  menuOperacional

-- Función para el menú general
menuGeneral :: IO ()
menuGeneral = do
  putStrLn "\nMenu General\n"
  -- Aquí irían las opciones del menú general
  putStrLn "\n2Volviendo al menú principal..."
  menuPrincipal

-- Funciones auxiliares

-- Función para leer el contenido de un archivo codigo brindado por chat
leerArchivo :: FilePath -> IO String
leerArchivo = readFile

-- Función para verificar si un ID es válido. codigo brindado por chat
idValido :: String -> [String] -> Bool
idValido id ids = id `elem` ids