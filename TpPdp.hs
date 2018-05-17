<<<<<<< HEAD
import Text.Show.Functions
=======
type Instruccion = Microprocesador -> Microprocesador
data Microprocesador = Microprocesador{posiciones :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, ultimoError :: String, programa ::} deriving(Show)
>>>>>>> 32f785c013ef7f08d211320805a115f32d509bd4

type Instruccion = Microprocesador -> Microprocesador
data Microprocesador = Microprocesador{posiciones :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, ultimoError :: String, memoriaPrograma :: [Instruccion]} deriving(Show)

xt8088 = Microprocesador {posiciones = memoriaVacia, acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = [], memoriaPrograma = [nop,nop,nop]}
at8086 = Microprocesador {posiciones = [1..20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = [], memoriaPrograma = [(valorToAcumuladorB 0), (valorToAcumuladorA 2), divide]}
fp20 = Microprocesador {posiciones = memoriaVacia, acumuladorA = 7, acumuladorB = 4, programCounter = 0, ultimoError = [], memoriaPrograma = [nop,nop,nop]}

<<<<<<< HEAD
--Instrucciones del microprocesador
=======
--Punto 3
--Punto 3.1
>>>>>>> 32f785c013ef7f08d211320805a115f32d509bd4
nop :: Instruccion
nop = aumentarPC 

lodv :: Int -> Instruccion
lodv valor = aumentarPC.(valorToAcumuladorA valor)

swap :: Instruccion
swap microprocesador = (aumentarPC.(intercambio (acumuladorA microprocesador) (acumuladorB microprocesador))) microprocesador

<<<<<<< HEAD
add ::Instruccion
add =  (aumentarPC).(valorToAcumuladorB 0).(sumarAcumuladores)

divide :: Instruccion
divide (Microprocesador posiciones acumuladorA 0 programCounter ultimoError programa) = (Microprocesador posiciones acumuladorA 0 programCounter "DIVISION BY ZERO" programa)
divide microprocesador = (aumentarPC.(valorToAcumuladorB 0).(valorToAcumuladorA (dividirAcumuladores microprocesador))) microprocesador

str :: Int-> Int-> Instruccion
str addr val = (aumentarPC.(guardarElemento addr val))

lod :: Int -> Instruccion
lod addr microprocesador= (aumentarPC.(valorToAcumuladorA (obtenerValorMemoria addr microprocesador))) microprocesador

--Funciones Auxiliares

aumentarPC :: Microprocesador -> Microprocesador
aumentarPC microprocesador = microprocesador {programCounter = (programCounter microprocesador) + 1}

memoriaVacia :: [Int]
memoriaVacia = take 1024 (repeat 0) -- Las direcciones de memoria van de 1 a 1024

ejecutarInstrucciones ::[Instruccion]->Microprocesador->Microprocesador
ejecutarInstrucciones listaInstrucciones = (componerInstrucciones listaInstrucciones)

componerInstrucciones :: [Instruccion] -> Instruccion
componerInstrucciones = foldl (.) id

intercambio :: Int -> Int -> Instruccion
intercambio acumuladorA acumuladorB = (valorToAcumuladorA (acumuladorB)).(valorToAcumuladorB (acumuladorA))

valorToAcumuladorA :: Int -> Instruccion
valorToAcumuladorA valor microprocesador = microprocesador {acumuladorA = valor}

valorToAcumuladorB :: Int -> Instruccion
valorToAcumuladorB valor microprocesador = microprocesador {acumuladorB = valor}
=======
--Punto 3.3
valorToAcumuladorA :: Int -> Instruccion
valorToAcumuladorA valor microprocesador = microprocesador {acumuladorA = valor}

valorToAcumuladorB :: Int -> Instruccion
valorToAcumuladorB valor microprocesador = microprocesador {acumuladorB = valor}

lodv :: Int -> Instruccion
lodv valor = aumentarPC.(valorToAcumuladorA valor)

swap :: Instruccion
swap microprocesador = (aumentarPC.(intercambio (acumuladorA microprocesador) (acumuladorB microprocesador))) microprocesador
intercambio :: Int -> Int -> Instruccion
intercambio acumuladorA acumuladorB = (valorToAcumuladorA (acumuladorB)).(valorToAcumuladorB (acumuladorA))

sumarAcumuladores :: Instruccion
sumarAcumuladores microprocesador = microprocesador {acumuladorA = (acumuladorA microprocesador) + (acumuladorB microprocesador)}

add ::Instruccion
add =  (aumentarPC).(valorToAcumuladorB 0).(sumarAcumuladores)
>>>>>>> 32f785c013ef7f08d211320805a115f32d509bd4

sumarAcumuladores :: Instruccion
sumarAcumuladores microprocesador = microprocesador {acumuladorA = (acumuladorA microprocesador) + (acumuladorB microprocesador)}

<<<<<<< HEAD
dividirAcumuladores :: Microprocesador -> Int
dividirAcumuladores microprocesador = div (acumuladorA microprocesador) (acumuladorB microprocesador)

=======
divide :: Instruccion
divide (Microprocesador posiciones acumuladorA 0 programCounter ultimoError) = (Microprocesador posiciones acumuladorA 0 programCounter "DIVISION BY ZERO")
divide microprocesador = (aumentarPC.(valorToAcumuladorB 0).(valorToAcumuladorA (dividirAcumuladores microprocesador))) microprocesador
dividirAcumuladores :: Microprocesador -> Int
dividirAcumuladores microprocesador = div (acumuladorA microprocesador) (acumuladorB microprocesador)

dividirDosPorCero :: Microprocesador -> Microprocesador
dividirDosPorCero = divide.(valorToAcumuladorA 2).(valorToAcumuladorB 0) 

str :: Int-> Int-> Instruccion
str addr val = (aumentarPC.(guardarElemento addr val))
>>>>>>> 32f785c013ef7f08d211320805a115f32d509bd4
guardarElemento :: Int -> Int -> Instruccion
guardarElemento addr val microprocesador = microprocesador{posiciones = concatenarListas (anteriorAddr (addr) (posiciones microprocesador)) [val] (posteriorAddr (addr) (posiciones microprocesador))}

concatenarListas :: [Int] -> [Int] -> [Int] -> [Int]
concatenarListas principio elemento final = principio ++ elemento ++ final

anteriorAddr :: Int -> [Int] -> [Int]
anteriorAddr addr= take (addr-1)

posteriorAddr :: Int -> [Int] -> [Int]
posteriorAddr = drop

<<<<<<< HEAD
obtenerValorMemoria :: Int -> Microprocesador -> Int
obtenerValorMemoria addr microprocesador = (posiciones microprocesador) !! (addr-1)

--ProgramasDePrueba Entrega1

aumentarTresVecesPC :: Microprocesador -> Microprocesador
aumentarTresVecesPC = ejecutarInstrucciones [nop,nop,nop]

dividirDosPorCero :: Microprocesador -> Microprocesador
dividirDosPorCero = ejecutarInstrucciones [(valorToAcumuladorB 0), (valorToAcumuladorA 2), divide]

sumarDiezaVeintidos :: Microprocesador -> Microprocesador
sumarDiezaVeintidos = ejecutarInstrucciones [(lodv 10), swap, (lodv 22), add]

--Entrega dos

ejecutarPrograma :: Instruccion
ejecutarPrograma microprocesador = (ejecutarInstrucciones (memoriaPrograma microprocesador)) microprocesador








=======
lod :: Int -> Instruccion
lod addr microprocesador= (aumentarPC.(valorToAcumuladorA (obtenerValorMemoria addr microprocesador))) microprocesador
obtenerValorMemoria :: Int -> Microprocesador -> Int
obtenerValorMemoria addr microprocesador = (posiciones microprocesador) !! (addr-1)

--Entrega dos

>>>>>>> 32f785c013ef7f08d211320805a115f32d509bd4

