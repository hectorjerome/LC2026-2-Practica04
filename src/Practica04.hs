module Practica04 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

--IMPLEMENTACION PARTE 1
--Ejercicio 1
conflict :: Estado -> Bool
conflict (_, []) = False
conflict (i, (x:xs)) = if x == [] then True else conflict (i, xs) 

--Ejercicio 2
success :: Estado -> Bool
success (_, e) = if e == [] then True else False


--Funcón para saber si una clausula es unitaria
esUnitaria :: Clausula -> Bool
esUnitaria [] = False
esUnitaria [x] = True
esUnitaria xs = False


--Función que obtiene el String de una literal
obtenerNombre :: Literal -> String
obtenerNombre (Var p) = p
obtenerNombre (Not (Var p)) = p


--Función para saber si ya esta la literal complementaria en el modelo
tieneInterpretacion :: String -> Interpretacion -> Bool
tieneInterpretacion _ [] = False
tieneInterpretacion x ((y,b): ys) = if x == y then True else tieneInterpretacion x ys


--Función para "desenvolver" la literal de una claúsula unitaria
obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral xs = Var ""


--Función para formal las tuplas de valores del modelo
darValor :: Clausula -> Interpretacion
darValor [Var p] = [(p, True)]
darValor [Not (Var p)] = [(p, False)]


--Función para ir acumulando las clausulas que no cumplen las condiciones
acumularClausula :: Estado -> Estado -> Estado
acumularClausula (_, xs) (i, ys) = (i, xs ++ ys)


--Ejercicio 3
unit :: Estado -> Estado
unit (modelo, []) = (modelo, [])
unit (modelo, (x:xs)) = if esUnitaria x 
                        then if tieneInterpretacion (obtenerNombre (obtenerLiteral x)) modelo 
                            then acumularClausula ([], [x]) (unit (modelo, xs))
                            else (modelo ++ darValor x, xs)
                        else acumularClausula ([], [x]) (unit (modelo, xs))



--Función que dada una lista y un elemento te indica si un elemento esta dentro de la lista
estaEn :: (Eq a) => a -> [a] -> Bool
estaEn x [] = False
estaEn x (y:ys) = if x == y then True else estaEn x ys


--Función que dada una sola literal y una lista de clausulas, elimina las clausulas con esas literales
eliminaLiteral :: Literal -> [Clausula] -> [Clausula]
eliminaLiteral lit [] = []
eliminaLiteral lit (x:xs) = if estaEn lit x then eliminaLiteral lit xs else [x] ++ eliminaLiteral lit xs 

--Función para dar la literal segun su forma de interpretación
reversoInterpretacion :: (String, Bool) -> Literal
reversoInterpretacion (p, True) = Var p
reversoInterpretacion (p, False) = Not (Var p)

--Función que acumula las interpretaciones
preservaInterpretacion :: Estado -> Estado -> Estado
preservaInterpretacion (i_1, _) (i_2, cl) = (i_1 ++ i_2, cl)



--Ejercicio 4
elim :: Estado -> Estado
elim ([], cl) = ([], cl)
elim ((x:xs), cl) = preservaInterpretacion ([x], []) (elim (xs, eliminaLiteral (reversoInterpretacion x) cl))


--Función que verifica si dos literales son complementarias o no
esComplementaria :: Literal -> Literal -> Bool
esComplementaria (Var p) (Not (Var q)) = if p == q then True else False
esComplementaria (Not (Var p)) (Var q) = if p == q then True else False
esComplementaria p q = False

reduceClausula :: Literal -> Clausula -> Clausula
reduceClausula lit [] = []
reduceClausula lit (x:xs) = if esComplementaria lit x 
                            then reduceClausula lit xs
                            else [x] ++ reduceClausula lit xs


reduceTotal :: Literal -> [Clausula] -> [Clausula]
reduceTotal lit [] = []
reduceTotal lit (x:xs) = [reduceClausula lit x] ++ reduceTotal lit xs

--Ejercicio 5
red :: Estado -> Estado
red ([], cl) = ([], cl)
red ((x:xs), cl) = preservaInterpretacion ([x], []) (red (xs, (reduceTotal (reversoInterpretacion x) cl)))


--Da complemento
complemento :: Literal -> Literal
complemento (Var p) = Not (Var p)
complemento (Not (Var p)) = (Var p)

--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
--sep lit (modelo, clausulas) = ( preservaInterpretacion (darValor [lit], []) (modelo, clausulas), preservaInterpretacion (darValor [complemento lit], []) (modelo, clausulas) )  
sep lit (modelo, clausulas) = ( preservaInterpretacion (darValor [complemento lit], []) (modelo, clausulas), preservaInterpretacion (darValor [lit], []) (modelo, clausulas) )  


--IMPLEMENTACION PARTE 2

--Función para aplanar las clausulas
aplanaClausulas :: [Clausula] -> [Literal]
aplanaClausulas [] = []
aplanaClausulas (x:xs) = x ++ aplanaClausulas xs

regresaMaximoAux :: [(Literal, Int)] -> (Literal, Int) -> Literal
regresaMaximoAux [] (a, b) = a
regresaMaximoAux ((a, b):xs) (c, d) = if d > b 
                                        then regresaMaximoAux xs (c, d)
                                        else regresaMaximoAux xs (a, b) 

regresaMaximo :: [(Literal, Int)] -> Literal
regresaMaximo (x:xs) = regresaMaximoAux xs x  


sumaLiteral :: Literal -> [(Literal, Int)] -> [(Literal, Int)]
sumaLiteral x [] = [(x,1)]
sumaLiteral x ((a,b):ys) = if x == a then [(a,b+1)] ++ ys else [(a,b)] ++ sumaLiteral x ys


ocurrencias :: [Literal] -> [(Literal, Int)]
ocurrencias [] = []
ocurrencias (x:xs) = sumaLiteral x (ocurrencias xs)

--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral cl = regresaMaximo (ocurrencias (aplanaClausulas cl))

segundoElemento :: (a,b) -> b
segundoElemento (_, y) = y


construirArbolDPLL :: Estado -> ArbolDPLL
construirArbolDPLL estado 
    | conflict estado = Node estado Void
    | success estado = Node estado Void
    | segundoElemento estado /= segundoElemento propuesto = Node estado (construirArbolDPLL propuesto)
    | otherwise = Branch estado (construirArbolDPLL izq) (construirArbolDPLL der)
    where
        propuesto = red (elim (unit estado))
        literal = heuristicsLiteral (segundoElemento estado)
        (izq, der) = sep literal estado


explorarArbolDPLL :: ArbolDPLL -> Estado
explorarArbolDPLL (Node estado Void) = estado
explorarArbolDPLL (Node _ subArbol) = explorarArbolDPLL subArbol
explorarArbolDPLL (Branch estado izq der) 
    | conflict (explorarArbolDPLL izq) && conflict (explorarArbolDPLL der) = ([], [])
    | conflict (explorarArbolDPLL izq) = explorarArbolDPLL der
    | otherwise = explorarArbolDPLL izq

regresaInterpretacion :: Estado -> Interpretacion 
regresaInterpretacion (a, _) = a


--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll claus = if conflict (explorarArbolDPLL (construirArbolDPLL ([], claus))) 
                then [] 
                else  regresaInterpretacion (explorarArbolDPLL (construirArbolDPLL ([], claus)))

--EXTRA

--Funciones Implementadas en la práctica pasada
--Para pasar de una proposición a una lista de clausulas

{-
FORMAS NORMALES
-}

--Ejercicio 1

{-
Función auxiliar para quitar implicaciones y doble implicaciones de una proposición 
-}
quitaImpl :: Prop -> Prop
quitaImpl (Cons True) = Cons True
quitaImpl (Cons False) = Cons False
quitaImpl (Var p) = Var p
quitaImpl (Not p) = Not (quitaImpl p)
quitaImpl (Or p q) = Or (quitaImpl p) (quitaImpl q)
quitaImpl (And p q) = And (quitaImpl p) (quitaImpl q)
quitaImpl (Impl p q) = Or (Not (quitaImpl p)) (quitaImpl q)
quitaImpl (Syss p q) = And (Or (Not (quitaImpl p)) (quitaImpl q)) 
                        (Or (Not (quitaImpl q)) (quitaImpl p))

{- 
Función que maneja los casos de la negación para la forma normal negativa 
-}
casoNegacion :: Prop -> Prop
casoNegacion (Cons True) = Cons False
casoNegacion (Cons False) = Cons True
casoNegacion (Var p) = Not (Var p)
casoNegacion (Not p) = fnnAux p
casoNegacion (Or p q) = And ( fnnAux (Not p)) (fnnAux (Not q))
casoNegacion (And p q) = Or ( fnnAux (Not p)) (fnnAux (Not q)) 

{-
Función auxiliar para la forma normal neagtiva,
ya recibe fórmulas sin implicaciones ni doble implicaciones 
-}
fnnAux :: Prop -> Prop
fnnAux (Cons True) = Cons True
fnnAux (Cons False) = Cons False
fnnAux (Var p)      = Var p
fnnAux (Not p)      = casoNegacion p
fnnAux (Or p q)     = Or (fnnAux p) (fnnAux q)
fnnAux (And p q)    = And (fnnAux p) (fnnAux q)

fnn :: Prop -> Prop
fnn p = fnnAux (quitaImpl p)


--Ejercicio 2

{-
Función que toma dos proposiciones (dado que suponemos que solo se llamará
bajo el caso de una disyunción), regresa la disyunción en caso de literales,
distribuye en caso de And's y busca recursivamente en los Or's. 
-}
distribuir :: Prop -> Prop -> Prop
distribuir (Var p) (Var q) = Or (Var p) (Var q)
distribuir (Not (Var p)) (Var q) = Or (Not (Var p)) (Var q)
distribuir (Var p) (Not (Var q)) = Or (Var p) (Not (Var q))
distribuir (Not (Var p)) (Not (Var q)) = Or (Not (Var p)) (Not (Var q))
distribuir (And p q) r = And (fncAux (Or p r)) (fncAux (Or q r))
distribuir p (And q r) = And (fncAux (Or p q)) (fncAux (Or p r))
distribuir (Or p q) r = Or (fncAux(Or p q)) (fncAux r)
distribuir p (Or q r) = Or (fncAux p) (fncAux(Or q r))

{-
Función que ya recibe la fórmula en forma normal negativa
-}
fncAux :: Prop -> Prop
fncAux (Cons True) = Cons True
fncAux (Cons False) = Cons False
fncAux (Var p) = (Var p)
fncAux (Not (Var p)) = Not (Var p)
fncAux (And p q) = And (fncAux p) (fncAux q)
fncAux (Or p q) = distribuir (fncAux p) (fncAux q)


fnc :: Prop -> Prop
fnc p = fncAux(fnn p)


{-
Función auxiliar que dada una lista quita sus elementos repetidos
para dejar solo una aparición por elemento.
Se ocupa en la función de variables de una proposición.
-}
eliminaRepetidos :: (Eq a) =>  [a] -> [a]
eliminaRepetidos [] = []
eliminaRepetidos (x:xs) = if (estaEn x xs) then eliminaRepetidos xs else [x] ++ eliminaRepetidos xs

{-
Función auxiliar para regresar las literales de un Or en una lista
-}
casoOr :: Prop -> [Literal]
casoOr (Var p) = [Var p]
casoOr (Not p) = [Not p]
casoOr (Or p q) = eliminaRepetidos(casoOr p ++ casoOr q)

clausulas :: Prop -> [Clausula]
clausulas (Var p) = [[Var p]]
clausulas (Not p) = [[Not p]]
clausulas (Or p q) = [casoOr (Or p q)]
clausulas (And p q) = clausulas(p) ++ clausulas(q) 


dpll2 :: Prop -> Interpretacion
dpll2 p = dpll (clausulas (fnc p))