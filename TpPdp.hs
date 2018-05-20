import Text.Show.Functions

type Instruccion = Microprocesador -> Microprocesador
data Microprocesador = Microprocesador{posiciones :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, ultimoError :: String, memoriaPrograma :: [Instruccion]} deriving(Show)

xt8088 = Microprocesador {posiciones = memoriaVacia, acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = [], memoriaPrograma = sumarDiezaVeintidos }
at8086 = Microprocesador {posiciones = [1..20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = [], memoriaPrograma = dividirDosPorCero }
fp20 = Microprocesador {posiciones = [2,1], acumuladorA = 7, acumuladorB = 4, programCounter = 0, ultimoError = [], memoriaPrograma = [nop,nop,nop]}
i9 = Microprocesador {posiciones = memoriaInfinita, acumuladorA = 7, acumuladorB = 4, programCounter = 0, ultimoError = [], memoriaPrograma = [nop,nop,nop]}
--Instrucciones del microprocesador
nop :: Instruccion
nop = aumentarPC

lodv :: Int -> Instruccion
lodv valor = aumentarPC.(valorToAcumuladorA valor)

swap :: Instruccion
swap microprocesador = (aumentarPC.(intercambio (acumuladorA microprocesador) (acumuladorB microprocesador))) microprocesador

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

memoriaInfinita :: [Int]
memoriaInfinita = repeat 0 -- Memoria infinita inicializada 0

ejecutarInstrucciones ::[Instruccion]->Microprocesador->Microprocesador
ejecutarInstrucciones listaInstrucciones = (componerInstrucciones listaInstrucciones)

componerInstrucciones :: [Instruccion] -> Instruccion
componerInstrucciones = foldr (.) id

intercambio :: Int -> Int -> Instruccion
intercambio acumuladorA acumuladorB = (valorToAcumuladorA (acumuladorB)).(valorToAcumuladorB (acumuladorA))

valorToAcumuladorA :: Int -> Instruccion
valorToAcumuladorA valor microprocesador = microprocesador {acumuladorA = valor}

valorToAcumuladorB :: Int -> Instruccion
valorToAcumuladorB valor microprocesador = microprocesador {acumuladorB = valor}

sumarAcumuladores :: Instruccion
sumarAcumuladores microprocesador = microprocesador {acumuladorA = (acumuladorA microprocesador) + (acumuladorB microprocesador)}

dividirAcumuladores :: Microprocesador -> Int
dividirAcumuladores microprocesador = div (acumuladorA microprocesador) (acumuladorB microprocesador)

guardarElemento :: Int -> Int -> Instruccion
guardarElemento addr val microprocesador = microprocesador{posiciones = concatenarListas (anteriorAddr (addr) (posiciones microprocesador)) [val] (posteriorAddr (addr) (posiciones microprocesador))}

concatenarListas :: [Int] -> [Int] -> [Int] -> [Int]
concatenarListas principio elemento final = principio ++ elemento ++ final

anteriorAddr :: Int -> [Int] -> [Int]
anteriorAddr addr= take (addr-1)

posteriorAddr :: Int -> [Int] -> [Int]
posteriorAddr = drop

obtenerValorMemoria :: Int -> Microprocesador -> Int
obtenerValorMemoria addr microprocesador = (posiciones microprocesador) !! (addr-1)

--Programas de prueba Entrega1

aumentarTresVecesPC :: Microprocesador -> Microprocesador
aumentarTresVecesPC = ejecutarInstrucciones [nop,nop,nop]


--Entrega 2

--3.1

cargarPrograma :: [Instruccion] -> Microprocesador -> Microprocesador
cargarPrograma programa unMicroprocesador = unMicroprocesador{memoriaPrograma = programa}

dividirDosPorCero = [(valorToAcumuladorB 0),(valorToAcumuladorA 2),divide]

sumarDiezaVeintidos = [lodv 10,swap,lodv 22,add]

--3.2

ejecutarPrograma :: Instruccion
ejecutarPrograma unMicroprocesador = ejecutarProgramaHastaError (memoriaPrograma unMicroprocesador) unMicroprocesador

ejecutarProgramaHastaError :: [Instruccion] -> Instruccion
ejecutarProgramaHastaError [] unMicroprocesador = unMicroprocesador
ejecutarProgramaHastaError (x:xs) unMicroprocesador | instruccionConError unMicroprocesador x = x unMicroprocesador | otherwise = ejecutarProgramaHastaError xs (x unMicroprocesador)

instruccionConError :: Microprocesador -> Instruccion ->  Bool
instruccionConError unMicroprocesador instruccion = tieneError.ultimoError.instruccion $ unMicroprocesador

tieneError :: String -> Bool
tieneError [] = False
tieneError (x:xs) = True

--3.3

ifnz :: [Instruccion] -> Instruccion
ifnz listaInstrucciones unMicroprocesador | ((==0).acumuladorA) unMicroprocesador = unMicroprocesador | otherwise = ejecutarProgramaHastaError listaInstrucciones unMicroprocesador

--3.4

depurar :: Instruccion
depurar unMicroprocesador = unMicroprocesador{memoriaPrograma = depurarPrograma (memoriaPrograma unMicroprocesador)}

depurarPrograma :: [Instruccion] -> [Instruccion]
depurarPrograma = filter (not.esInstruccionInnecesaria)

esInstruccionInnecesaria :: Instruccion -> Bool
esInstruccionInnecesaria instruccion = (acumuladorQuedaEnCero acumuladorA instruccion)  && (acumuladorQuedaEnCero acumuladorB instruccion) && (all (==0) ((posiciones.instruccion) xt8088))

acumuladorQuedaEnCero :: (Microprocesador -> Int) -> Instruccion -> Bool
acumuladorQuedaEnCero acumulador instruccion = ((==0).acumulador.instruccion) xt8088

--3.5

memoriaOrdenada :: Microprocesador -> Bool
memoriaOrdenada unMicroprocesador = esListaOrdenada (posiciones unMicroprocesador)

esListaOrdenada :: [Int] -> Bool
esListaOrdenada [x] = True
esListaOrdenada (x:xs) = x <= head xs && esListaOrdenada xs

--3.6

cargarProgramaAMemoriaInfinita :: [Instruccion] -> [Instruccion]
cargarProgramaAMemoriaInfinita programa = (memoriaPrograma.(cargarPrograma programa)) i9
--El programa se carga igualmente solo que al querer mostrar todo el microprocesador la lista nunca termina de mostrarse, en este programa de prueba se ve que el programa esta cargado

ejecutarProgramaAMemoriaInfinita :: [Instruccion] -> Int
ejecutarProgramaAMemoriaInfinita programa= (acumuladorA.ejecutarPrograma.(cargarPrograma programa)) i9
--En este programa de muestra se ve que el programa se ejecuta normalmente

--Al preguntar si la memoria esta ordenada el programa nunca va a terminar de evaluar al no llegar a una condicion de corte

--La lista infinita es algo definido en haskell que no causa error, por eso podemos tener un microcontrolador con "memoria infinita" y es valido. Sin embargo, al querer mostrar una lista infinita completa u operar sobre absolutamente todos sus elementos, la ejecución no se detiene nunca.
