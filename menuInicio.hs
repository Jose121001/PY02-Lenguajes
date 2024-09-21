-- Imports de archivos

import Data.Char (isDigit) --  isDigit para verifica si un caracter es un digito
import Data.IORef
import Data.List (isInfixOf)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Directory (doesDirectoryExist, doesPathExist)
import System.FilePath (takeDirectory)
import System.IO
import Text.XHtml (menu)

-- Funcion que permite mostrar las opciones del menu de inicio
main :: IO ()
main = do
  rutaRef <- newIORef "" -- Inicializamos la referencia vacía
  menuPrincipal rutaRef

-- Funcion del menu principal que ahora recibe la referencia
menuPrincipal :: IORef FilePath -> IO ()
menuPrincipal rutaRef = do
  putStrLn "\nBienvenido al sistema\n"
  putStrLn "Por favor seleccione la opcion deseada:\n"
  putStrLn "1. Menu operacional"
  putStrLn "2. Menu general"
  putStrLn "0. Salir"
  opcion <- getLine
  case opcion of
    "1" -> menuOperacional rutaRef
    "2" -> menuGeneral
    "0" -> putStrLn "Saliendo del sistema..."
    _ -> do
      putStrLn "Opcion inválida. Intentelo de nuevo."
      menuPrincipal rutaRef

-- Funcion para el menu operacional donde muestra la información del usuario
menuOperacional :: IORef FilePath -> IO ()
menuOperacional rutaRef = do
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
        "1" -> cargarMobiliario rutaRef
        "2" -> menuSalasReunion rutaRef
        "3" -> putStrLn "Informe de reservas..."
        "0" -> main
        _ -> do
          putStrLn "Opción inválida. Intentelo de nuevo."
          menuOperacional rutaRef
    Nothing -> do
      putStrLn "ID no válido. Por favor, intente nuevamente."
      menuOperacional rutaRef

-- Funcion para el menu general
menuGeneral :: IO ()
menuGeneral = do
  putStrLn "\nMenu General\n"
  -- Aquí irian las opciones del menu general
  putStrLn "\nVolviendo al menú principal..."
  main

-------------------------------------------------Funciones auxiliares usuario---------------------------------------------------------------
-- Funcion para procesar cada línea del archivo
procesarLinea :: String -> (String, String, String)
procesarLinea linea =
  let [id, nombre, puesto] = splitBy ',' linea
   in (id, nombre, puesto)

-- Formatea la informacion del usuario para mostrarla
formatearUsuario :: (String, String, String) -> String
formatearUsuario (id, nombre, puesto) = "ID: " ++ id ++ ", Nombre: " ++ nombre ++ ", Puesto: " ++ puesto

-------------------------------------------------Fin de funciones auxiliares usuario---------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------Cargar mobiliarios-------------------------------------------------------------------------------

-- Funcion para cargar mobiliario
cargarMobiliario :: IORef FilePath -> IO ()
cargarMobiliario rutaRef = do
  putStrLn "\nFuncion para cargar mobiliario de sala\n"
  putStrLn "Ingrese la ubicacion donde desea guardar la informacion (ruta completa, e.g. C:\\ruta\\archivo.txt):\n"
  ruta <- getLine
  let carpeta = takeDirectory ruta
  existe <- doesDirectoryExist carpeta
  if existe
    then do
      -- Leer mobiliario existente antes de solicitar nuevo código
      archivoExiste <- doesPathExist ruta
      existentes <-
        if archivoExiste
          then do
            -- Guardamos la ruta en la referencia
            writeIORef rutaRef ruta
            contenido <- readFile ruta
            let lineas = lines contenido
            return $ map procesarLineaMobiliario lineas
          else return [] -- Si no existe el archivo, retornamos una lista vacía
      putStrLn "\nIngrese la informacion del mobiliario:\n"
      -- Solicitud de variables individuales
      -- Validamos que no está vacío cada variable. Brindado por chat
      codigo <- validarCodigoUnico "Codigo: " existentes
      nombre <- validarEntrada "Nombre: " (\x -> not (null x) && all (not . isDigit) x) "Error: El nombre no puede estar vacío ni contener números."
      descripcion <- validarEntrada "Descripcion: " (\x -> not (null x)) "Error: La descripcion no puede estar vacia."
      tipo <- validarTipo
      -- Moldeamos la informacion en una cadena
      let contenidoMobiliario = codigo ++ "," ++ nombre ++ "," ++ descripcion ++ "," ++ tipo
      -- Guardamos la informacion del mobiliario con las variables concatenadas
      agregarMobiliario ruta contenidoMobiliario
      putStrLn "Mobiliario agregado exitosamente...."
      -- Mostrar el contenido del archivo después de agregar el mobiliario
      mostrarMobiliarioGuardado rutaRef
      menuPrincipal rutaRef
    else putStrLn "Error: La carpeta especificada no existe."

--------------------------------------Funciones auxiliares para mobiliario.------------------------------------------------------------------------------

-- Funcion para agregar mobiliario a un archivo txt
agregarMobiliario :: FilePath -> String -> IO ()
agregarMobiliario ruta nuevoMobiliario = appendFile ruta (nuevoMobiliario ++ "\n")

-- Funcion para validar que el codigo no este repetido o vacio
validarCodigoUnico :: String -> [(String, String, String, String)] -> IO String
validarCodigoUnico mensaje existentes = do
  putStrLn mensaje
  codigo <- getLine
  if not (null codigo) && all (\(c, _, _, _) -> c /= codigo) existentes
    then return codigo
    else do
      putStrLn "Error: El codigo ya existe o es invalido, ingrese uno nuevo."
      validarCodigoUnico mensaje existentes

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
mostrarMobiliarioGuardado :: IORef FilePath -> IO ()
mostrarMobiliarioGuardado rutaRef = do
  ruta <- readIORef rutaRef -- Leer la ruta desde el IORef
  archivoExiste <- doesPathExist ruta
  if archivoExiste
    then do
      contenido <- readFile ruta
      let lineas = lines contenido
      let mobiliarios = map procesarLineaMobiliario lineas
      mapM_ (putStrLn . formatearMobiliario) mobiliarios
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
      putStrLn "Error: Tipo invalido. Debe ser (consumible, electronico, menaje)."
      validarTipo

-- Funcion para procesar una linea del archivo en una tupla de mobiliario
procesarLineaMobiliario :: String -> (String, String, String, String)
procesarLineaMobiliario linea =
  let [codigo, nombre, descripcion, tipo] = splitBy ',' linea
   in (codigo, nombre, descripcion, tipo)

-- Funcion para formatear la informacion del mobiliario para mostrarla
formatearMobiliario :: (String, String, String, String) -> String
formatearMobiliario (codigo, nombre, descripcion, tipo) =
  "Codigo: " ++ codigo ++ ", Nombre: " ++ nombre ++ ", Descripción: " ++ descripcion ++ ", Tipo: " ++ tipo

------------------------------------Fin de funciones auxiliares de mobiliario--------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------Cargar y mostrar salas--------------------------------------------------------------------------------------------------------------
-- Funcion para cargar mobiliario
menuSalasReunion :: IORef FilePath -> IO ()
menuSalasReunion rutaRef = do
  putStrLn "\nDigite la opcion deseada: \n"
  putStrLn "1. Cargar salas de reunion"
  putStrLn "2. Mostrar salas de reunion"
  putStrLn "0. Volver"
  opcionUsuario <- getLine
  case opcionUsuario of
    "1" -> cargarSalasDeReunion rutaRef
    "2" -> mostrarSalasDeReunion rutaRef
    "0" -> menuPrincipal rutaRef
    _ -> do
      putStrLn "Opción inválida. Intentelo de nuevo."
      menuSalasReunion rutaRef

cargarSalasDeReunion :: IORef FilePath -> IO ()
cargarSalasDeReunion rutaRef = do
  -- Leemos la ruta previamente almacenada en rutaRef
  ruta <- readIORef rutaRef
  putStrLn ("Ruta leida: " ++ ruta)
  ruta <- readIORef rutaRef
  let rutaSala = "archivosTxt\\salas.txt"
  let carpetaSalas = takeDirectory ruta
  existe <- doesDirectoryExist carpetaSalas
  if existe
    then do
      putStrLn "\nIngrese la informacion de la sala:\n"

      -- Solicitud de variables individuales con validacion
      nombre <- validarEntrada "Nombre: " (\x -> not (null x)) "Error: El nombre no puede estar vacio."
      piso <- validarEntrada "Piso: " (\x -> not (null x)) "Error: El piso no puede estar vacio ni contener números."
      ubicacion <- validarEntrada "Ubicacion: " (\x -> not (null x)) "Error: La Ubicacion no puede estar vacia."
      capacidad <- validarEntrada "Capacidad: " (\x -> not (null x)) "Error: La Capacidad no puede estar vacia."

      -- Almacenar los IDs de los mobiliarios seleccionados
      putStrLn "\nMobiliarios deseados:\n"
      -- Cargamos mobiliarios existentes desde el archivo
      existentes <- cargarMobiliariosDesdeArchivo rutaRef
      -- Mostramos el contenido del archivo utilizando la referencia
      mostrarMobiliarioGuardado rutaRef
      -- Llamada a seleccionarMobiliarios con la lista de mobiliarios existentes y una lista vacia para los seleccionados
      idsMobiliarios <- seleccionarMobiliarios existentes []
      -- Genera el código de sala (usamos un contador y lo convertimos a String)
      codigoSala <- generarCodigoUnico

      -- Formateamos la información para guardarla
      -- La lista se agregara en el contenido
      let contenidoSala = codigoSala ++ "," ++ nombre ++ "," ++ piso ++ "," ++ ubicacion ++ "," ++ capacidad ++ "," ++ unwords idsMobiliarios ++ "\n"
      -- Escribir en el archivo utilizando withFile para evitar bloqueo

      -- Guardamos la información en el archivo
      appendFile rutaSala contenidoSala
      putStrLn ("\nCodigo de sala generado: " ++ codigoSala)
      putStrLn "\nSala agregada exitosamente....\n"
      menuSalasReunion rutaRef
    else putStrLn "Error: La carpeta especificada no existe."

-- Funcion que me permite ver la info de una sala segun su codigo de salxa
mostrarSalasDeReunion :: IORef FilePath -> IO ()
mostrarSalasDeReunion rutaRef = do
  let rutaSalas = "archivosTxt\\salas.txt"
  archivoExiste <- doesPathExist rutaSalas
  if archivoExiste
    then do
      putStrLn "\nIngrese el codigo de la sala:\n"
      codigoSala <- getLine
      contenido <- readFile rutaSalas
      let lineas = lines contenido
      let salas = map procesarLineaSala lineas -- Procesar lineas de salas
      let salaEncontrada = filter (\(codigo, _, _, _, _, _) -> codigo == codigoSala) salas
      putStrLn "\nMostrando informacion de la sala:\n"
      case salaEncontrada of
        [(codigo, nombre, piso, ubicacion, capacidad, mobiliarios)] ->
          putStrLn $ formatearSala (codigo, nombre, piso, ubicacion, capacidad, mobiliarios)
        _ -> putStrLn "Error: Sala no encontrada."
      menuSalasReunion rutaRef
    else putStrLn "Error: El archivo de salas especificado no existe."

------------------------------------------Funciones auxiliares Salas-------------------------------------------------------------

-- Función para generar un código unico basado en la fecha y hora
generarCodigoUnico :: IO String
generarCodigoUnico = do
  tiempoActual <- getCurrentTime
  let codigoUnico = formatTime defaultTimeLocale "%Y%m%d%H%M%S" tiempoActual
  return codigoUnico

-- Función para procesar una linea del archivo de salas en una tupla
procesarLineaSala :: String -> (String, String, String, String, String, String)
procesarLineaSala linea =
  let [codigoSala, nombre, piso, ubicacion, capacidad, mobiliarios] = splitBy ',' linea
   in (codigoSala, nombre, piso, ubicacion, capacidad, mobiliarios)

-- Funcion para formatear la informacion de la sala para mostrarla
formatearSala :: (String, String, String, String, String, String) -> String
formatearSala (codigoSala, nombre, piso, ubicacion, capacidad, mobiliarios) =
  "Codigo de la sala: " ++ codigoSala ++ ", Nombre: " ++ nombre ++ ", Piso: " ++ piso ++ ", Ubicacion: " ++ ubicacion ++ ", Capacidad: " ++ capacidad ++ ", Mobiliarios: " ++ mobiliarios

-- Funcion para cargar la lista de mobiliarios desde el archivo
cargarMobiliariosDesdeArchivo :: IORef FilePath -> IO [(String, String, String, String)]
cargarMobiliariosDesdeArchivo rutaRef = do
  ruta <- readIORef rutaRef
  putStrLn ("Ruta leida: " ++ ruta)
  -- Obtenemos la ruta guardada
  ruta <- readIORef rutaRef
  contenido <- readFile ruta
  let lineas = lines contenido
  return $ map procesarLineaMobiliario lineas -- Procesa cada línea en la tupla

-- Funcion para seleccionar mobiliarios.Brindado por chat
seleccionarMobiliarios :: [(String, String, String, String)] -> [String] -> IO [String]
seleccionarMobiliarios existentes seleccionados = do
  putStrLn "Ingrese el ID de los mobiliarios deseados (o '@' para terminar):"
  idMobiliario <- getLine
  if idMobiliario == "@"
    then return seleccionados
    else do
      resultadoValidacion <- validarCodigoExistente idMobiliario existentes
      case resultadoValidacion of
        Just codigo -> do
          let nombreMobiliario = buscarNombrePorID codigo existentes
          if nombreMobiliario `elem` seleccionados
            then do
              putStrLn "Error: El mobiliario ya ha sido seleccionado. Inténtalo de nuevo."
              seleccionarMobiliarios existentes seleccionados -- Volver a pedir el ID
            else do
              -- Agregar el nombre en lugar del ID
              idsRestantes <- seleccionarMobiliarios existentes (nombreMobiliario : seleccionados)
              return (nombreMobiliario : idsRestantes)
        Nothing -> do
          putStrLn "Error: ID no válido. Inténtalo de nuevo."
          seleccionarMobiliarios existentes seleccionados -- Volver a pedir el ID

-- Funcion que me permite obtener el nombre por id.Brindado por chat
buscarNombrePorID :: String -> [(String, String, String, String)] -> String
buscarNombrePorID idMobiliario existentes =
  case filter (\(id, nombre, _, _) -> id == idMobiliario) existentes of
    [(id, nombre, _, _)] -> nombre
    _ -> "Nombre no encontrado" -- O maneja el caso de error según necesites

-- Función para validar que el codigo no este vacio y verificar si existe.Brindado por chat
validarCodigoExistente :: String -> [(String, String, String, String)] -> IO (Maybe String)
validarCodigoExistente codigo existentes = do
  if null codigo
    then do
      putStrLn "Error: El codigo no puede estar vacio. Intenta1lo de nuevo."
      return Nothing -- Retorna Nothing si el código está vacío
    else
      if any (\(c, _, _, _) -> c == codigo) existentes
        then do
          putStrLn $ "El codigo ingresado existe: " ++ codigo
          return (Just codigo) -- Retorna Just el código si existe
        else do
          putStrLn "Error: El codigo no existe. Ingrese uno valido."
          return Nothing -- Retorna Nothing si no existe

---------------------------------------------Fin funciones  auxiliares Salas----------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------Funciones auxiliares generales-----------------------------------------------------------------------------

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

-- Definimos una "variable global" para la ruta de mobiliarios
rutaMobiliariosRef :: IO (IORef FilePath)
rutaMobiliariosRef = newIORef ""