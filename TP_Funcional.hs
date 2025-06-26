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

--olvidarProgramas: Hace que el robot que lo recibe olvide los primeros N programas que conoce.
olvidarProgramas :: Int -> Programa
olvidarProgramas n robot = robot { programas = drop n (programas robot) }

--autoAtaque: El robot objetivo se ataca a sí mismo usando su primer programa registrado. Lanzar error si no tiene ningún programa.
autoAtaque :: Programa
autoAtaque robot =
    case programas robot of
        [] -> error "El robot no tiene programas para autoatacarse."
        (p:_) -> p robot

--poder: Calcula la fuerza de un robot sumando su energía más el producto de su nivel de experiencia por la cantidad de programas que tiene.
poder :: Robot -> Int
poder r = energia r + experiencia r + length (programas r)

--danio: Calcula cuánta energía se pierde o gana al aplicar un programa a un robot. La ganancia se indica con un número negativo. La función retorna 0 si no hay cambio.
danio :: Robot -> Programa -> Int
danio r p =
    let energiaAntes = energia r
        energiaDespues = energia (p r)
    in energiaAntes - energiaDespues

--diferenciaDePoder: La diferencia absoluta en poder entre dos robots
diferenciaDePoder :: Robot -> Robot -> Int
diferenciaDePoder r1 r2 = abs(poder r1 - poder r2)
