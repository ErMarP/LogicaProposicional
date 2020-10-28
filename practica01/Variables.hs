--Erick Martínez Piza
--Módulo donde definimos nuestras variables y a las fórmulas de PL
module Variables

where

import Data.List as L

type Variable = String -- Variable con tipo de cadena
--Ejemplos de variables:
-- "p"
-- "q"

data PLvar v = -- v es un parametro para el tipo de variables
          Bot                       -- Constructor para bottom
        | Top                       -- Constructor para top
        | Var v                     -- Constructor de variables
        | Imp (PLvar v) (PLvar v)   -- Constructor de implicaciones
        | Dis (PLvar v) (PLvar v)   -- Constructor de disyunciones
        | Con (PLvar v) (PLvar v)   -- Constructor de conjunciones
        | Neg (PLvar v)             -- Constructor de negaciones
        deriving (Eq,Show)

type PL = PLvar Variable  -- PL con tipo de variables Variable

--Función que regresa una lista de las variables que tiene la fórmula de la PL 
varsOf :: PL -> [Variable]
varsOf phi = case phi of
            Bot             -> []
            Top             -> []
            Var x           -> [x]
            Imp alpha beta  -> (varsOf alpha) `L.union` varsOf(beta)
            Dis alpha beta  -> (varsOf alpha) `L.union` varsOf(beta)
            Con alpha beta  -> (varsOf alpha) `L.union` varsOf(beta)
            Neg alpha       -> (varsOf alpha)
--Ejemplos de test que se pueden correr en la terminal:
-- varsOf Bot
-- varsOf Top
-- varsOf (Var "p")
-- varsOf (Neg(Var "p"))
-- varsOf ((Var "p") `PL.Dis` (Neg(Var "p")))
-- varsOf ((Var "p") `PL.Dis` (Neg(Var "q")))
-- varsOf (((Var "p") `PL.Dis` (Neg(Var "q"))) `PL.Imp`(Var "r"))