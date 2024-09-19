-- CargarYMostrarMobiliario.hs
module CargarYMostrarMobiliario (cargarMobiliario, mostrarMobiliario) where

-- Funcion para agregar moiliario a un archivo txt

cargarMobiliario :: IO ()
cargarMobiliario = do
  putStrLn "\nFunción para cargar mobiliario de sala\n"

mostrarMobiliario :: IO ()
mostrarMobiliario = do
  putStrLn "\nFunción para mostrar mobiliario de sala\n"
