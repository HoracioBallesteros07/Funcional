module TP_Funcional where
--Declaracion de type
type Programa = Robot -> Robot
type Academia = [Robot]

--Modelado Robot
data Robot = Robot {
    nombre :: String,
    experiencia :: Int,
    energia :: Int,
    programas :: [Programa]
}

instance Show Robot where
  show robot = "Robot { nombre = " ++ show (nombre robot)
             ++ ", experiencia = " ++ show (experiencia robot)
             ++ ", energia = " ++ show (energia robot)
             ++ ", programas = <funciones> }"

-- ROBOTS 

-- Robot sin programas
robotBasico :: Robot
robotBasico = Robot {
    nombre = "R1",
    experiencia = 2,
    energia = 50,
    programas = []
}

-- Robot ATLAS
robotATLAS :: Robot
robotATLAS = Robot {
    nombre = "Atlas",
    experiencia = 10,
    energia = 100,
    programas = []
}

-- Robot con un programa que le recarga energía en 20
robotRecargado :: Robot
robotRecargado = Robot {
    nombre = "R2",
    experiencia = 5,
    energia = 40,
    programas = [recargarBateria 20]
}

-- Robot con múltiples programas
robotComplejo :: Robot
robotComplejo = Robot {
    nombre = "R3",
    experiencia = 3,
    energia = 30,
    programas = [descargaElectrica, recargarBateria 15, olvidarProgramas 1]
}

--Robot con menos de 10 de energia (8)
robotEnergia8 :: Robot
robotEnergia8 = Robot {
    nombre = "R4",
    experiencia = 3,
    energia = 8,
    programas = [descargaElectrica, recargarBateria 15, olvidarProgramas 1]
}

--PUNTOS

--recargarBateria: Este programa recibe un robot y lo recarga, aumentando su energía en una cantidad variable.
recargarBateria :: Int -> Programa
recargarBateria cantidad robot  =  robot { energia = energia robot + cantidad}  

--descargaElectrica: Este programa causa una reducción de energía al robot objetivo: si su energía es mayor a 10, le quita 10 puntos; en caso contrario, reduce su energía a la mitad.
descargaElectrica :: Programa
descargaElectrica robot
    | energia robot > 10 = robot { energia = energia robot - 10 }
    | otherwise = robot { energia = energia robot `div` 2 }
