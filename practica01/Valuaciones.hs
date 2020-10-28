--Erick Martínez Piza
--Módulo donde definimos nuestras valuaciones y modelos
module Valuaciones

where

import Variables

type Valuacion = Variable -> Bool -- Valuacion con tipo de función: de una variable a un booleano
--Ejemplos de valuaciones:
-- "p" -> true
-- "q" -> false

type Modelo = [Variable] -- Modelos con tipo de lista de variables
--Ejemplos de modelos:
-- ["p"]
-- ["p","q"]

-- Funcion para ver si una fórmula de la PL satisface un modelo en específico
mSatisface :: Modelo -> PL -> Bool 
mSatisface m phi = case phi of 
                Bot             -> False     
                Top             -> True      
                Var x           -> x `elem` m 
                Imp alpha beta  -> not(mSatisface m alpha) || (mSatisface m beta)         
                Dis alpha beta  -> (mSatisface m alpha) || (mSatisface m beta)     
                Con alpha beta  -> (mSatisface m alpha) && (mSatisface m beta) 
                Neg alpha       -> not (mSatisface m alpha) 
--Ejemplos de test que se pueden correr en la terminal:
-- mSatisface ["p"] Bot
-- mSatisface ["p"] Top
-- mSatisface ["p"] (Var "p")
-- mSatisface ["q"] (Var "p")
-- mSatisface ["q"] (Neg(Var "p"))
-- mSatisface ["p","q"] ((Var "p") `PL.Dis` (Neg(Var "q")))
-- mSatisface ["p","q"] ((Var "p") `PL.Con` (Neg(Var "q")))
-- mSatisface ["p","q"] ((Var "p") `PL.Con` (Var "q"))
-- mSatisface ["p","q"] ((Var "p") `PL.Imp` (Neg(Var "q")))
