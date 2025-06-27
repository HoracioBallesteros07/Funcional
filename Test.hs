import TP_Funcional
import Control.Exception (catch, evaluate, SomeException)
import System.IO.Unsafe (unsafePerformIO)

test :: IO ()
test = do
    putStrLn "== Test 1: recargarBateria =="
    print $ energia (recargarBateria 25 robotBasico) == 75

    putStrLn "== Test 2: descargaElectrica =="
    print $ energia (descargaElectrica robotBasico) == 40
    print $ energia (descargaElectrica robotEnergia8) == 4

    putStrLn "== Test 3: olvidarProgramas =="
    print $ length (programas (olvidarProgramas 1 robotComplejo)) == 2

    putStrLn "== Test 4: autoAtaque (sin programas) =="
    print (catchAutoAtaque robotBasico)

    putStrLn "== Test 5: poder =="
    print $ poder robotBasico == 52
    print $ poder robotComplejo == 36

    putStrLn "== Test 6: danio =="
    print $ danio robotBasico (recargarBateria 10) == -10
    print $ danio robotComplejo descargaElectrica == 10

    putStrLn "== Test 7: diferenciaDePoder =="
    print $ diferenciaDePoder robotBasico robotComplejo == 16

    putStrLn "== Test 8: existeAtlasSinProgramas =="
    let robotATLAS = Robot "Atlas" 5 60 []
    print $ existeAtlasSinProgramas [robotBasico, robotComplejo] == False
    print $ existeAtlasSinProgramas [robotATLAS] == True

    putStrLn "== Test 9: sonViejosObstinados =="
    let viejoBueno = Robot "Old1" 17 50 (replicate 52 descargaElectrica)
    let viejoMalo  = Robot "Old2" 17 50 [descargaElectrica]
    print $ sonViejosObstinados [viejoBueno] == True
    print $ sonViejosObstinados [viejoMalo] == False
    print $ sonViejosObstinados [robotBasico] == True

    putStrLn "== Test 10: mejorProgramaContra =="
    let prog = mejorProgramaContra robotBasico robotComplejo
    print $ energia (prog robotBasico) == 40

    putStrLn "== Test 11: mejorOponente =="
    let academia1 = [robotBasico, robotRecargado, robotComplejo]
    print $ nombre (mejorOponente robotBasico academia1) == "R3"


    -- Función auxiliar para capturar el error del autoAtaque
catchAutoAtaque :: Robot -> Bool
catchAutoAtaque r = 
    unsafePerformIO (catch (evaluate (autoAtaque r `seq` False)) handler)
  where
    handler :: SomeException -> IO Bool
    handler _ = return True
