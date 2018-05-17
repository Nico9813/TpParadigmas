data Microprocesador = Microprocesador{posiciones :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, ultimoError :: String} deriving(Show)

xt8088 = Microprocesador {posiciones = memoriaVacia, acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = []}
at8086 = Microprocesador {posiciones = [1..20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, ultimoError = []}
fp20 = Microprocesador {posiciones = memoriaVacia, acumuladorA = 7, acumuladorB = 4, programCounter = 0, ultimoError = []}

aumentarPC :: Microprocesador -> Microprocesador
aumentarPC microprocesador = microprocesador {programCounter = (programCounter microprocesador) + 1}
memoriaVacia :: [Int]
memoriaVacia = take 1024 (repeat 0)
-- Las direcciones de memoria van de 1 a 1024

--Punto 3
--Punto 3.1
nop :: Microprocesador -> Microprocesador
nop = aumentarPC 

-- Comentario

--Punto 3.2
aumentarTresVecesPC :: Microprocesador -> Microprocesador
aumentarTresVecesPC = nop.nop.nop --Se usa el concepto de composicion

--Punto 3.3
valorToAcumuladorA :: Int -> Microprocesador -> Microprocesador
valorToAcumuladorA valor microprocesador = microprocesador {acumuladorA = valor}

valorToAcumuladorB :: Int -> Microprocesador -> Microprocesador
valorToAcumuladorB valor microprocesador = microprocesador {acumuladorB = valor}

lodv :: Int -> Microprocesador -> Microprocesador
lodv valor = aumentarPC.(valorToAcumuladorA valor)

swap :: Microprocesador -> Microprocesador
swap microprocesador = (aumentarPC.(intercambio (acumuladorA microprocesador) (acumuladorB microprocesador))) microprocesador
intercambio :: Int -> Int -> Microprocesador -> Microprocesador
intercambio acumuladorA acumuladorB = (valorToAcumuladorA (acumuladorB)).(valorToAcumuladorB (acumuladorA))

sumarAcumuladores :: Microprocesador -> Microprocesador
sumarAcumuladores microprocesador = microprocesador {acumuladorA = (acumuladorA microprocesador) + (acumuladorB microprocesador)}

add :: Microprocesador -> Microprocesador
add =  (aumentarPC).(valorToAcumuladorB 0).(sumarAcumuladores)

programaPuntoTres :: Microprocesador -> Microprocesador
programaPuntoTres = add.(lodv 22).swap.(lodv 10)

divide :: Microprocesador -> Microprocesador
divide (Microprocesador posiciones acumuladorA 0 programCounter ultimoError) = (Microprocesador posiciones acumuladorA 0 programCounter "DIVISION BY ZERO")
divide microprocesador = (aumentarPC.(valorToAcumuladorB 0).(valorToAcumuladorA (dividirAcumuladores microprocesador))) microprocesador
dividirAcumuladores :: Microprocesador -> Int
dividirAcumuladores microprocesador = div (acumuladorA microprocesador) (acumuladorB microprocesador)

dividirDosPorCero :: Microprocesador -> Microprocesador
dividirDosPorCero = divide.(valorToAcumuladorA 2).(valorToAcumuladorB 0) 

str :: Int-> Int->Microprocesador -> Microprocesador
str addr val = (aumentarPC.(guardarElemento addr val))
guardarElemento :: Int -> Int -> Microprocesador -> Microprocesador
guardarElemento addr val microprocesador = microprocesador{posiciones = concatenarListas (anteriorAddr (addr) (posiciones microprocesador)) [val] (posteriorAddr (addr) (posiciones microprocesador))}
concatenarListas :: [Int] -> [Int] -> [Int] -> [Int]
concatenarListas principio elemento final = principio ++ elemento ++ final
anteriorAddr :: Int -> [Int] -> [Int]
anteriorAddr addr= take (addr-1)
posteriorAddr :: Int -> [Int] -> [Int]
posteriorAddr = drop

lod :: Int -> Microprocesador -> Microprocesador
lod addr microprocesador= (aumentarPC.(valorToAcumuladorA (obtenerValorMemoria addr microprocesador))) microprocesador
obtenerValorMemoria :: Int -> Microprocesador -> Int
obtenerValorMemoria addr microprocesador = (posiciones microprocesador) !! (addr-1)
