-- Imports de archivos

--  isDigit para verifica si un caracter es un digito

import Data.Char (isAlpha, isDigit)
import Data.IORef
import Data.List (isInfixOf)
import Data.Time (Day, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import System.Directory (doesDirectoryExist, doesPathExist)
import System.FilePath (takeDirectory)
import System.IO
import Text.Read (readMaybe)
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
        "3" -> mostrarReservas rutaRef
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
  putStrLn "1. Gestion de reserva"
  putStrLn "2. Consulta de reserva"
  putStrLn "3. Cancelacion de reserva"
  putStrLn "4. Modificacion de reserva"
  putStrLn "5. Consulta de disponibilidad de sala"
  putStrLn "6. Volver"
  opcion <- getLine
  case opcion of
    "1" -> gestionReserva
    "2" -> consultaReserva
    "3" -> eliminarReserva
    "4" -> modificarReserva
    "5" -> subMenu
    "6" -> main
    _ -> do
      putStrLn "Opción inválida. Intentelo de nuevo."
      menuGeneral

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

-------------------------------------------------SubMenu Disponibilidad---------------------------------------------------------------

-- Paso 1: Solicitar la fecha de inicio y la fecha de fin
solicitarFechas :: IO (Maybe (String, String))
solicitarFechas = do
  putStrLn "Ingrese la fecha de inicio en formato YYYY-MM-DD:"
  fechaInicio <- getLine
  putStrLn "Ingrese la fecha de fin en formato YYYY-MM-DD:"
  fechaFin <- getLine
  if fechaInicio <= fechaFin
    then return $ Just (fechaInicio, fechaFin)
    else do
      putStrLn "La fecha de inicio debe ser menor o igual a la fecha de fin."
      return Nothing -- Retornar Nothing en caso de error

-- Solicitar una única fecha
solicitarFechaUnica :: IO String
solicitarFechaUnica = do
  putStrLn "Ingrese la fecha en formato YYYY-MM-DD:"
  getLine

-- Funcion para generar todas las fechas en el rango
fechasEnRango :: String -> String -> [String]
fechasEnRango fechaInicio fechaFin =
  let dayInicio = parseDate fechaInicio
      dayFin = parseDate fechaFin
   in map (formatTime defaultTimeLocale "%Y-%m-%d") [dayInicio .. dayFin]

-- Funcion para parsear una fecha
parseDate :: String -> Day
parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

-- Paso 2: Leer el archivo de salas y obtener los IDs de sala
obtenerIdsSalas :: FilePath -> IO [String]
obtenerIdsSalas archivoSalas = do
  contenido <- readFile archivoSalas
  let lineas = lines contenido
      idsSalas = map (head . splitBy ',') lineas
  return idsSalas

-- Paso 3 y 4: Asociar las fechas con los IDs de sala y verificar disponibilidad
verificarSalasEnRango :: FilePath -> FilePath -> [String] -> IO ()
verificarSalasEnRango archivoSalas archivoReservas fechas = do
  idsSalas <- obtenerIdsSalas archivoSalas
  mapM_ (verificarSalasEnFecha archivoReservas idsSalas) fechas

verificarSalasEnFecha :: FilePath -> [String] -> String -> IO ()
verificarSalasEnFecha archivoReservas idsSalas fecha = do
  let salasConFecha = map (\idSala -> (idSala, fecha)) idsSalas
  putStrLn $ "Verificando disponibilidad para la fecha: " ++ fecha
  mapM_ (verificarYMostrar archivoReservas) salasConFecha

-- Paso 5: Verificar si una sala con una fecha existe en el archivo de reservas
existeReserva :: FilePath -> (String, String) -> IO Bool
existeReserva archivoReservas (idSala, fecha) = do
  contenido <- readFile archivoReservas
  let lineas = lines contenido
      reservas = map (splitBy ',') lineas
      reservasFiltradas = filter (\[_id, _, idS, _, f] -> idS == idSala && f == fecha) reservas
  return (not (null reservasFiltradas))

-- Paso 6: Mostrar estado de la sala para cada fecha
verificarYMostrar :: FilePath -> (String, String) -> IO ()
verificarYMostrar archivoReservas (idSala, fecha) = do
  reservada <- existeReserva archivoReservas (idSala, fecha)
  if reservada
    then putStrLn $ "La sala " ++ idSala ++ " esta reservada para la fecha " ++ fecha
    else putStrLn $ "La sala " ++ idSala ++ " esta libre para la fecha " ++ fecha

-- Función principal para ejecutar el programa
subMenu :: IO ()
subMenu = do
  let archivoSalas = "archivosTxt\\salas.txt"
  let archivoReservas = "archivosTxt\\reservas.txt"
  putStrLn "\nSub Menu de consultas:"
  putStrLn "1. Verificar disponibilidad para una fecha especifica"
  putStrLn "2. Verificar disponibilidad para un rango de fechas"
  putStrLn "3. Volver"
  putStrLn "Opcion:"
  opcion <- getLine
  case opcion of
    "1" -> do
      fecha <- solicitarFechaUnica
      verificarSalasEnRango archivoSalas archivoReservas [fecha]
      subMenu
    "2" -> do
      fechas <- solicitarFechas
      case fechas of
        Just (fechaInicio, fechaFin) -> do
          let fechasRango = fechasEnRango fechaInicio fechaFin
          verificarSalasEnRango archivoSalas archivoReservas fechasRango
          subMenu
        Nothing -> subMenu -- Volver al submenú si hay un error en las fechas
    "3" -> menuGeneral
    _ -> do
      putStrLn "Opcion no valida. Intente de nuevo."
      subMenu

-------------------------------------------------Consulta de reserva---------------------------------------------------------------
consultaReserva :: IO ()
consultaReserva = do
  putStrLn "\nIngrese el identificador de reserva: "
  codigoReserva <- getLine
  let ruta = "archivosTxt\\reservas.txt"
  contenido <- leerArchivo ruta
  let reservas = map procesarLineaReserva (lines contenido)

  case lookup codigoReserva (map (\(idReserva, idUsuario, codigoSala, capacidad, fecha) -> (codigoReserva, (idUsuario, codigoSala, capacidad, fecha))) reservas) of -- Brindado por chat
    Just (idUsuario, codigoSala, capacidad, fecha) -> do
      putStrLn $ "ID de reserva: " ++ codigoReserva
      putStrLn $ "ID de usuario: " ++ idUsuario
      putStrLn $ "Sala : " ++ codigoSala
      putStrLn $ "Capacidad : " ++ capacidad
      putStrLn $ "Fecha de reserva: " ++ fecha
      menuGeneral
    Nothing -> do
      putStrLn "Esa reserva no existe"
      menuGeneral

-------------------------------------------------Cancelacion de reserva---------------------------------------------------------------

eliminarReserva :: IO ()
eliminarReserva = do
  putStrLn "\nIngrese el identificador de reserva a eliminar: "
  codigoReserva <- getLine
  let ruta = "archivosTxt\\reservas.txt"
  contenido <- leerArchivo ruta
  let reservas = map procesarLineaReserva (lines contenido)

  -- Filtramos las reservas que no tienen el código de reserva indicado
  let nuevasReservas = filter (\(idReserva, _, _, _, _) -> idReserva /= codigoReserva) reservas

  if length reservas == length nuevasReservas
    then do
      putStrLn "Esa reserva no existe."
    else do
      -- Sobrescribimos el archivo con las nuevas reservas
      let nuevoContenido =
            unlines
              ( map
                  ( \(idReserva, idUsuario, codigoSala, capacidad, fecha) ->
                      idReserva ++ "," ++ idUsuario ++ "," ++ codigoSala ++ "," ++ capacidad ++ "," ++ fecha
                  )
                  nuevasReservas
              )
      writeFile ruta nuevoContenido
      putStrLn "La reserva ha sido eliminada correctamente."

  menuGeneral

-------------------------------------------------Modificacion de reserva---------------------------------------------------------------

-- Función para modificar una reserva existente
modificarReserva :: IO ()
modificarReserva = do
  putStrLn "\nIngrese el código de reserva a modificar: "
  codigoReserva <- getLine
  let ruta = "archivosTxt\\reservas.txt"
  contenido <- leerArchivo ruta
  let reservas = map procesarLineaReserva (lines contenido)

  case lookup codigoReserva (map (\(id, user, codigo, cap, fecha) -> (id, (user, codigo, cap, fecha))) reservas) of
    Just (idUsuario, codigoSalaOriginal, capacidadOriginal, fechaOriginal) -> do
      -- Modificación de la sala
      putStrLn $ "Identificador de sala actual: " ++ codigoSalaOriginal
      putStrLn "Ingrese el nuevo identificador de sala (o ingrese 0 para mantener la actual): "
      codigoSala <- getLine
      let nuevoCodigoSala = if codigoSala == "0" then codigoSalaOriginal else codigoSala

      -- Modificación de la fecha
      putStrLn $ "Fecha actual: " ++ fechaOriginal
      putStrLn "Ingrese la nueva fecha en formato yyyy-mm-dd (o ingrese 0 para mantener la actual): "
      fecha <- getLine
      let nuevaFecha = if fecha == "0" then fechaOriginal else fecha

      -- Validamos la nueva fecha
      if nuevaFecha /= "0" && not (validarFecha nuevaFecha)
        then do
          putStrLn "El formato de fecha es incorrecto."
          menuGeneral
        else do
          -- Modificación de la cantidad de personas
          putStrLn $ "Capacidad actual: " ++ capacidadOriginal
          putStrLn "Ingrese la nueva cantidad de personas (o ingrese 0 para mantener la actual): "
          nuevaCapacidad <- getLine
          let nuevaCapacidadFinal = if nuevaCapacidad == "0" then capacidadOriginal else nuevaCapacidad

          case readMaybe nuevaCapacidadFinal :: Maybe Int of
            Just nuevaCap -> do
              -- Validamos la capacidad de la sala
              let rutaSalas = "archivosTxt\\salas.txt"
              salasContenido <- leerArchivo rutaSalas
              let salas = map procesarLineaSala (lines salasContenido)

              case lookup nuevoCodigoSala (map (\(id, nombre, piso, ubicacion, capacidad, mobiliarios) -> (id, (nombre, capacidad))) salas) of
                Just (_, capacidadSala) -> do
                  let capacidadInt = read capacidadSala :: Int
                  if nuevaCap <= capacidadInt
                    then do
                      -- Comprobamos si ya hay una reserva en la nueva sala y fecha
                      case lookup (nuevoCodigoSala, nuevaFecha) (map (\(id, user, codigo, cap, fecha) -> ((codigo, fecha), id)) reservas) of
                        Just _ -> do
                          putStrLn "La sala ya está reservada en esa fecha."
                          menuGeneral
                        Nothing -> do
                          -- Actualizamos la reserva
                          let nuevasReservas =
                                map
                                  ( \(id, user, codigo, cap, fecha) ->
                                      if id == codigoReserva
                                        then (id, user, nuevoCodigoSala, nuevaCapacidadFinal, nuevaFecha)
                                        else (id, user, codigo, cap, fecha)
                                  )
                                  reservas
                          let nuevoContenido =
                                unlines
                                  ( map
                                      ( \(id, user, codigo, cap, fecha) ->
                                          id ++ "," ++ user ++ "," ++ codigo ++ "," ++ cap ++ "," ++ fecha
                                      )
                                      nuevasReservas
                                  )
                          writeFile ruta nuevoContenido
                          putStrLn "La reserva ha sido modificada correctamente."
                          menuGeneral
                    else do
                      putStrLn "La nueva cantidad de personas supera la capacidad de la sala."
                      menuGeneral
                Nothing -> do
                  putStrLn "La sala no existe."
                  menuGeneral
            Nothing -> do
              putStrLn "La cantidad ingresada no es válida."
              menuGeneral
    Nothing -> do
      putStrLn "El código de reserva no existe."
      menuGeneral

-------------------------------------------------Gestion de reserva---------------------------------------------------------------
--- Función para agregar reserva
agregarReserva :: FilePath -> String -> IO ()
agregarReserva direccion contenido = appendFile direccion (contenido ++ "\n")

-- Función para procesar una linea del archivo de reservas en una tupla
procesarLineaReserva :: String -> (String, String, String, String, String)
procesarLineaReserva linea =
  let [idReserva, idUsuario, codigoSala, capacidad, fecha] = splitBy ',' linea
   in (idReserva, idUsuario, codigoSala, capacidad, fecha)

-- Función para validar el formato de la fecha
validarFecha :: String -> Bool
validarFecha fecha = case splitFecha fecha of
  Just (año, mes, día) -> esAñoValido año && esMesValido mes && esDiaValido mes día
  Nothing -> False

-- Función para separar la fecha en (año, mes, día)
splitFecha :: String -> Maybe (Int, Int, Int)
splitFecha fecha =
  let partes = wordsWhen (== '-') fecha
   in if length partes == 3
        then Just (read (partes !! 0), read (partes !! 1), read (partes !! 2))
        else Nothing

-- Verificar si el año es válido
esAñoValido :: Int -> Bool
esAñoValido año = año >= 0

-- Verificar si el mes es válido
esMesValido :: Int -> Bool
esMesValido mes = mes >= 1 && mes <= 12

-- Verificar si el día es válido según el mes
esDiaValido :: Int -> Int -> Bool
esDiaValido mes día
  | mes `elem` [1, 3, 5, 7, 8, 10, 12] = día >= 1 && día <= 31 -- Meses con 31 días
  | mes `elem` [4, 6, 9, 11] = día >= 1 && día <= 30 -- Meses con 30 días
  | mes == 2 = día >= 1 && día <= 28 -- Febrero (no estamos manejando años bisiestos aquí)
  | otherwise = False

-- Función auxiliar para dividir una cadena en partes según un delimitador
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- Funcion que me reserva la fecha
gestionReserva :: IO ()
gestionReserva = do
  putStrLn "\nIngrese el identificador de usuario: "
  codigoUsuario <- getLine
  let ruta = "archivosTxt\\usuarios.txt"
  contenido <- leerArchivo ruta
  let usuarios = map procesarLinea (lines contenido)

  case lookup codigoUsuario (map (\(id, nombre, puesto) -> (id, (nombre, puesto))) usuarios) of -- Brindado por chat
    Just (nombre, puesto) -> do
      putStrLn "\nIngrese el identificador de sala: "
      codigoSala <- getLine
      let ruta = "archivosTxt\\salas.txt"
      contenido <- leerArchivo ruta
      let salas = map procesarLineaSala (lines contenido)

      case lookup codigoSala (map (\(id, nombre, piso, ubicacion, capacidad, mobiliarios) -> (id, (nombre, piso, ubicacion, capacidad, mobiliarios))) salas) of -- Brindado por chat
        Just (nombre, piso, ubicacion, capacidad, mobiliarios) -> do
          let entero = read capacidad :: Int
          putStrLn "Ingrese la cantidad de personas: "
          input <- getLine
          case readMaybe input :: Maybe Int of
            Just num ->
              if num <= entero
                then do
                  putStrLn "Ingrese la fecha en la que desea reservar en formato yyyy-mm-dd"
                  fecha <- getLine
                  if validarFecha fecha
                    then do
                      let ruta = "archivosTxt\\reservas.txt"
                      contenido <- leerArchivo ruta
                      let reservas = map procesarLineaReserva (lines contenido)
                      case lookup (codigoSala, fecha) (map (\(id, user, codigo, cap, fecha) -> ((codigo, fecha), (id, user, cap))) reservas) of
                        Just (idReserva, idUsuario, capacidad) -> do
                          putStrLn "La sala ya se encuentra reservada para esa fecha"
                          menuGeneral
                        Nothing -> do
                          let identificador = length reservas + 1
                          putStrLn $ "El identificador de su reserva es " ++ show identificador
                          let resultado = show identificador ++ "," ++ codigoUsuario ++ "," ++ codigoSala ++ "," ++ input ++ "," ++ fecha
                          agregarReserva ruta resultado
                          menuGeneral
                    else do
                      putStrLn "El formato de fecha es incorrecto"
                      menuGeneral
                else do
                  putStrLn "La cantidad de personas ingresadas supera la capacidad de la sala"
                  menuGeneral
            Nothing -> do
              putStrLn "Lo ingresado no es un número entero válido"
              menuGeneral
        Nothing -> do
          putStrLn "La sala no existe."
          menuGeneral
    Nothing -> do
      putStrLn "El usuario no existe."
      menuGeneral

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
      capacidad <- validarEntradaCapacidad "Capacidad: "

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
------------------------------------------------Informe de reservas----------------------------------------------------------------------------------------------------------

-- Mostrar la información de la reserva y la sala
mostrarReservas :: IORef FilePath -> IO ()
mostrarReservas rutaRef = do
  let rutaReservas = "archivosTxt\\reservas.txt"
  archivoExiste <- doesPathExist rutaReservas

  if archivoExiste
    then do
      contenido <- readFile rutaReservas
      let lineas = lines contenido
      let reservas = map procesarLineaReserva lineas

      -- Mostrar todas las reservas
      putStrLn "\nDatos de las reservas:\n"
      mapM_ mostrarReserva reservas
    else putStrLn "El archivo de reservas no existe."

-- Función para mostrar una reserva y obtener información de la sala
mostrarReserva :: (String, String, String, String, String) -> IO ()
mostrarReserva (idReserva, idUsuario, codigoSala, capacidad, fecha) = do
  putStrLn $ "IdReserva: " ++ idReserva
  putStrLn $ "IdUsuario: " ++ idUsuario
  putStrLn $ "CodigoSala: " ++ codigoSala
  putStrLn $ "Capacidad: " ++ capacidad
  putStrLn $ "Fecha: " ++ fecha

  -- Obtener información de la sala
  obtenerInfoSala codigoSala
  putStrLn "" -- Línea en blanco para separación
  menuGeneral

-- Función para obtener la información completa de la sala
obtenerInfoSala :: String -> IO ()
obtenerInfoSala codigoSala = do
  let rutaSalas = "archivosTxt\\salas.txt"
  archivoExiste <- doesPathExist rutaSalas

  if archivoExiste
    then do
      contenido <- readFile rutaSalas
      let lineas = lines contenido
      let salas = map procesarLineaSala lineas
      let salaEncontrada = filter (\(codigo, _, _, _, _, _) -> codigo == codigoSala) salas

      case salaEncontrada of
        [(codigo, nombre, ubicacion, piso, capacidadSala, equipamiento)] -> do
          putStrLn $ "NombreSala: " ++ nombre
          putStrLn $ "UbicacionSala: " ++ ubicacion
          putStrLn $ "Piso: " ++ piso
          putStrLn $ "CapacidadSala: " ++ capacidadSala
          putStrLn $ "Equipamiento: " ++ equipamiento
        _ -> putStrLn "No se encontró la información de la sala."
    else putStrLn "El archivo de salas no existe."

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

-- Valida entrada solo letras
validarEntradaCapacidad :: String -> IO String
validarEntradaCapacidad mensaje = do
  putStr mensaje
  entrada <- getLine
  if null entrada || any (not . isDigit) entrada
    then do
      putStrLn "Error: La capacidad debe ser un número entero sin caracteres ni espacios."
      validarEntradaCapacidad mensaje
    else return entrada