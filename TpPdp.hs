import Text.Show.Functions
import Data.List
type Instruccion = Microprocesador -> Microprocesador
data Microprocesador = Microprocesador{posiciones :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, ultimoError :: String, memoriaPrograma :: [Instruccion]} deriving(Show)

xt8088 = Microprocesador {posiciones = memoriaVacia, acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = [], memoriaPrograma = [lodv 3, swap] }
at8086 = Microprocesador {posiciones = [1..20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = [], memoriaPrograma = dividirDosPorCero }
fp20 = Microprocesador {posiciones = [2,1], acumuladorA = 7, acumuladorB = 24, programCounter = 0, ultimoError = [], memoriaPrograma = [lodv 3, swap]}
microDesorden = Microprocesador {posiciones = [2, 5, 1, 0, 6, 9], acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = [], memoriaPrograma = []}
i9 = Microprocesador {posiciones = memoriaInfinita, acumuladorA = 7, acumuladorB = 4, programCounter = 0, ultimoError = [], memoriaPrograma = [nop,nop,nop]}
--Instrucciones del microprocesador
nop :: Instruccion
nop = id

lodv :: Int -> Instruccion
lodv valor = (valorToAcumuladorA valor)

swap :: Instruccion
swap microprocesador = (intercambio (acumuladorA microprocesador) (acumuladorB microprocesador)) microprocesador

add ::Instruccion
add =  (valorToAcumuladorB 0).(sumarAcumuladores)

divide :: Instruccion
divide (Microprocesador posiciones acumuladorA 0 programCounter ultimoError programa) = (Microprocesador posiciones acumuladorA 0 programCounter "DIVISION BY ZERO" programa)
divide microprocesador = (valorToAcumuladorB 0).(valorToAcumuladorA (dividirAcumuladores microprocesador)) $ microprocesador

str :: Int-> Int-> Instruccion
str addr val = (guardarElemento addr val)

lod :: Int -> Instruccion
lod addr microprocesador= (valorToAcumuladorA (obtenerValorMemoria addr microprocesador)) microprocesador

--Funciones Auxiliares

aumentarPC :: Microprocesador -> Microprocesador
aumentarPC microprocesador = microprocesador {programCounter = (programCounter microprocesador) + 1}

memoriaVacia :: [Int]
memoriaVacia = take 1024 (repeat 0) -- Las direcciones de memoria van de 1 a 1024

memoriaInfinita :: [Int]
memoriaInfinita = repeat 0 -- Memoria infinita inicializada 0

ejecutarInstrucciones ::[Instruccion]->Microprocesador->Microprocesador
ejecutarInstrucciones listaInstrucciones = (componerInstrucciones (map (\instruccion->ejecutarInstruccion $ instruccion) listaInstrucciones))

ejecutarInstruccion :: Instruccion -> Microprocesador -> Microprocesador
ejecutarInstruccion instruccion = aumentarPC.instruccion

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

esCero :: (Num a,Eq a) => a -> Bool
esCero numero = 0 == numero

--Programas de prueba Entrega1

aumentarTresVecesPC :: Microprocesador -> Microprocesador
aumentarTresVecesPC = ejecutarInstrucciones [nop,nop,nop]


--Entrega 2

--3.1

cargarPrograma :: [Instruccion] -> Microprocesador -> Microprocesador
cargarPrograma programa unMicroprocesador = unMicroprocesador{memoriaPrograma = programa}

dividirDosPorCero = [str 1 2, str 2 0, lod 2, swap, lod 1, divide]

sumarDiezaVeintidos = [lodv 10,swap,lodv 22,add]

--3.2

ejecutarPrograma :: Microprocesador -> Microprocesador
ejecutarPrograma unMicroprocesador = ejecutarProgramaHastaError unMicroprocesador (memoriaPrograma unMicroprocesador) 

ejecutarProgramaHastaError :: Microprocesador -> [Instruccion] -> Microprocesador
ejecutarProgramaHastaError unMicroprocesador listaInstrucciones = foldl (flip validarYAplicar) unMicroprocesador listaInstrucciones

validarYAplicar :: Instruccion -> Microprocesador -> Microprocesador
validarYAplicar instruccion unMicroprocesador | instruccionConError unMicroprocesador instruccion = unMicroprocesador{ultimoError="Error al ejecutar una instruccion del programa"} | ultimoError unMicroprocesador /= [] = unMicroprocesador | otherwise = ejecutarInstruccion instruccion unMicroprocesador

instruccionConError :: Microprocesador -> Instruccion ->  Bool
instruccionConError unMicroprocesador instruccion = (/= "").ultimoError.instruccion $ unMicroprocesador

--3.3

ifnz :: [Instruccion] -> Instruccion
ifnz listaInstrucciones unMicroprocesador | ((esCero).acumuladorA) unMicroprocesador = unMicroprocesador | otherwise = ejecutarProgramaHastaError unMicroprocesador listaInstrucciones

--3.4

depurar :: Instruccion
depurar unMicroprocesador = unMicroprocesador{memoriaPrograma = depurarPrograma (memoriaPrograma unMicroprocesador)}

depurarPrograma :: [Instruccion] -> [Instruccion]
depurarPrograma = filter (not.esInstruccionInnecesaria)

esInstruccionInnecesaria :: Instruccion -> Bool
esInstruccionInnecesaria instruccion = (acumuladorQuedaEnCero acumuladorA instruccion)  && (acumuladorQuedaEnCero acumuladorB instruccion) && (all (esCero) ((posiciones.instruccion) xt8088))

acumuladorQuedaEnCero :: (Microprocesador -> Int) -> Instruccion -> Bool
acumuladorQuedaEnCero acumulador instruccion = ((esCero).acumulador.instruccion) xt8088

--3.5

memoriaOrdenada :: Microprocesador -> Bool
memoriaOrdenada unMicroprocesador = esListaOrdenada (posiciones unMicroprocesador)

esListaOrdenada :: [Int] -> Bool
esListaOrdenada lista = listasIguales lista (sort lista)

listasIguales :: [Int]->[Int]->Bool
listasIguales lista1 lista2 = all (uncurry (==)).(zip lista1) $ lista2

--3.6

cargarProgramaAMemoriaInfinita :: [Instruccion] -> [Instruccion]
cargarProgramaAMemoriaInfinita programa = (memoriaPrograma.(cargarPrograma programa)) i9
--El programa se carga igualmente solo que al querer mostrar todo el microprocesador la lista nunca termina de mostrarse, en este programa de prueba se ve que el programa esta cargado

ejecutarProgramaAMemoriaInfinita :: [Instruccion] -> Int
ejecutarProgramaAMemoriaInfinita programa= (acumuladorA.ejecutarPrograma.(cargarPrograma programa)) i9
--En este programa de muestra se ve que el programa se ejecuta normalmente

--Al preguntar si la memoria esta ordenada el programa nunca va a terminar de evaluar al no llegar a una condicion de corte

{-La lista infinita es algo definido en haskell que no causa error, por eso podemos tener un microcontrolador con "memoria infinita" y es valido.
Sin embargo, al querer mostrar una lista infinita completa u operar sobre absolutamente todos sus elementos, la ejecución (ya sea de mostrar o de operar) no se detiene nunca.
El único caso donde podemos evaluar una lista infinita es si tiene una condicion de corte que se cumpla. Esto gracias a una característica de haskell que es lazy
evaluation, que nos permite por ejemplo saber si una lista infinita no está ordenada. Ya que al encontrar un sólo elemento no ordenado, haskell dejará de evaluar
y devolverá False-}