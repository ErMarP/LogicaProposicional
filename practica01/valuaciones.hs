module Valuaciones

where

import Variables

type Valuacion = Variable -> Bool

type Modelo = [Variable]

-- Funcion para ver si una PL satisface un modelo en específico
satModPL :: Modelo -> PL -> Bool 
satModPL m phi = case phi of 
                Bot             -> False     
                Top             -> True      
                Var x           -> x `elem` m 
                Imp alpha beta  -> not(satModPL m alpha) || (satModPL m beta)         
                Dis alpha beta  -> (satModPL m alpha) || (satModPL m beta)     
                Con alpha beta  -> (satModPL m alpha) && (satModPL m beta) 
                Neg alpha       -> not (satModPL m alpha) 
--Ejemplos de test que se pueden correr en la terminal:
-- satModPL ["p"] Bot
-- satModPL ["p"] Top
-- satModPL ["p"] (Var "p")
-- satModPL ["q"] (Var "p")
-- satModPL ["q"] (Neg(Var "p"))
-- satModPL ["p","q"] ((Var "p") `PL.Dis` (Neg(Var "q")))
-- satModPL ["p","q"] ((Var "p") `PL.Con` (Neg(Var "q")))
-- satModPL ["p","q"] ((Var "p") `PL.Imp` (Neg(Var "q")))
