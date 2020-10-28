--Erick Martínez Piza
--Módulo que se usará como principal
module Practica01

where

import Variables as PL (PL,PLvar(..),varsOf)
import Valuaciones (Valuacion,Modelo,mSatisface)

--Función que devuelve el número de conjunciones de una fórmula de la PL
conInPl :: PL -> Int
conInPl phi = case phi of
            Bot            -> 0
            Top            -> 0
            Var x          -> 0
            Imp alpha beta -> (conInPl alpha)  + (conInPl beta)
            Dis alpha beta -> (conInPl alpha)  + (conInPl beta)
            Con alpha beta -> (conInPl alpha)  + (conInPl beta) + 1
            Neg alpha      -> (conInPl alpha)
--Ejemplos de test que se pueden correr en la terminal:
-- conInPl Bot
-- conInPl Top
-- conInPl (Var "p")
-- conInPl ((Var "p") `PL.Dis` (Var "q"))
-- conInPl ((Var "p") `PL.Con` (Var "q"))
-- conInPl (((Var "p") `PL.Con` (Var "q"))`PL.Con` (Var "r"))
-- conInPl (((Var "p") `PL.Con` (Var "q"))`PL.Dis` ((Var "r") `PL.Con` (Var "s")))

-- Función que quita las implicaciones de una fórmula de la PL
quitaImp :: PL -> PL
quitaImp phi = case phi of
            Bot            -> ((Neg (Var "p")) `PL.Dis` (Var "p"))
            Top            -> Neg((Neg (Var "p")) `PL.Dis` (Var "p"))
            Var x          -> (Var x)
            Imp alpha beta -> ((Neg(quitaImp alpha)) `PL.Dis` (quitaImp beta))
            Dis alpha beta -> ((quitaImp alpha) `PL.Dis` (quitaImp beta))
            Con alpha beta -> ((quitaImp alpha) `PL.Con` (quitaImp beta))
            Neg alpha      -> Neg(quitaImp alpha)
--Ejemplos de test que se pueden correr en la terminal:
-- quitaImp Bot
-- quitaImp Top
-- quitaImp (Var "p")
-- quitaImp (Neg(Var "p"))
-- quitaImp ((Var "p") `PL.Con` (Var "q"))
-- quitaImp ((Var "p") `PL.Imp` (Var "q"))
-- quitaImp ((Neg(Var "p")) `PL.Imp` (Var "q"))
-- quitaImp (((Var "p") `PL.Imp` (Var "r")) `PL.Imp` (Var "q"))

-- Función que transforma a una fórmula de la PL con solamente negaciones y disyunciones
lNor :: PL -> PL
lNor phi = case phi of
            Bot            -> ((Neg (Var "p")) `PL.Dis` (   Var "p"))
            Top            -> Neg((Neg (Var "p")) `PL.Dis` (Var "p"))
            Var x          -> (Var x)
            Imp alpha beta -> ((Neg(lNor alpha)) `PL.Dis` (lNor beta))
            Dis alpha beta -> ((lNor alpha) `PL.Dis` (lNor beta))
            Con alpha beta -> (Neg(lNor alpha) `PL.Dis` Neg(lNor beta))
            Neg alpha      -> Neg(lNor alpha)
--Ejemplos de test que se pueden correr en la terminal:
-- lNor Bot
-- lNor Top
-- lNor (Var "p")
-- lNor (Neg(Var "p"))
-- lNor ((Var "p") `PL.Dis` (Var "q"))
-- lNor ((Var "p") `PL.Con` (Var "q"))
-- lNor ((Neg(Var "p")) `PL.Con` (Var "q"))
-- lNor (((Var "p") `PL.Con` (Var "q")) `PL.Imp` (Var "r"))