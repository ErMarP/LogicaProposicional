--Erick Martínez Piza
--Módulo que se usará como principal
module Practica02

where

import Data.List as L
import Variables as PL (PL,PLvar(..),varsOf)
import Valuaciones (Valuacion,Modelo,mSatisface)

-- Si phi en PL es una conjunción de literales,
-- entonces conLit2ListLit transforma phi en una lista de literales.
conLit2ListLit :: PL -> [PL]
conLit2ListLit phi = case phi of
            Bot             -> []
            Top             -> []
            Var x           -> [Var x]
            Neg (Var x)     -> [Neg (Var x)]
            Con alpha beta  -> (conLit2ListLit alpha) ++ (conLit2ListLit beta)
            _               -> error $ "conLit2ListLit: phi no es una conjuncion de literales, phi = "++(show phi)
-- Ejemplos: 
-- conLit2ListLit Top                                  --> []
-- conLit2ListLit ((Var "p") `PL.Con` (Var "q"))       --> [p,q]
-- conLit2ListLit ((Var "p") `PL.Con` (Neg(Var "q")))  --> [p,-q]
-- conLit2ListLit ((Var "p") `PL.Dis` (Var "q"))       --> conLit2ListLit: phi no es una conjuncion de literales, phi = ((Var "p") `PL.Dis` (Var "q"))


-- Dado un literal l en PL, litComp calcula el literal complementario de l.
litComp :: PL -> PL
litComp l = case l of
            Var x       -> Neg (Var x)
            Neg (Var x) -> Var x
            _           -> error $ "litComp: phi no es literal, phi = "++(show l)
-- Ejemplos: 
-- litComp (Var "p")      --> -p
-- litComp (Neg(Var "p")) --> p
-- litComp Top            --> litComp: phi no es literal, phi = Top


-- Dada una termino de PL, representada por una lista de literales phi,
-- terminoSat determina si phi es una termino satisfactible.
-- phi es satisfactible si y solo si phi no tiene literales complementarios.
terminoSat :: [PL] -> Bool
terminoSat phi = case phi of
            []     -> True
            (l:ls) -> (litComp l) `notElem` phi && terminoSat ls
-- Ejemplos: 
-- terminoSat []                                 --> True
-- terminoSat [Var "p"]                          --> True
-- terminoSat [Var "p", Var "q"]                 --> True
-- terminoSat [Var "p", (Neg(Var "q"))]          --> True
-- terminoSat [Var "p", (Neg(Var "q")), Var "q"] --> False


-- Dada phi en PL, dnf2LListLit transforma phi a una formula phi' en DNF,
-- donde phi' esta representada como una lista de listas de literales.
dnf2LListLit :: PL -> [[PL]]
dnf2LListLit phi = case phi of
            Top              -> [[]]
            Var x            -> [[Var x]]
            Neg (Var x)      -> [[Neg (Var x)]]
            (Con _ _)        -> [conLit2ListLit phi]
            (Dis alpha beta) -> (dnf2LListLit alpha) ++ (dnf2LListLit beta)
            _                -> error $ "dnf2LListLit: phi no esta en dnf, phi = "++(show phi)
-- Ejemplos: 
-- dnf2LListLit Top                                                                      --> [[]]
-- dnf2LListLit (Var "p")                                                                --> [[p]]
-- dnf2LListLit (Neg(Var "p"))                                                           --> [[-p]]
-- dnf2LListLit ((Var "p") `PL.Con` (Var "q"))                                           --> [[p,q]]
-- dnf2LListLit ((Var "p") `PL.Dis` (Var "q"))                                           --> [[p],[q]]
-- dnf2LListLit (((Var "p") `PL.Con` (Var "q")) `PL.Dis` ((Var "r") `PL.Con` (Var "s"))) --> [[p,q],[r,s]]
-- dnf2LListLit (((Var "p") `PL.Dis` (Var "q")) `PL.Dis` ((Var "r") `PL.Dis` (Var "s"))) --> [[p],[q],[r],[s]]
-- dnf2LListLit Bot                                                                      --> dnf2LListLit: phi no esta en dnf, phi = Bot
-- dnf2LListLit (((Var "p") `PL.Dis` (Var "q")) `PL.Con` ((Var "r") `PL.Dis` (Var "s"))) --> conLit2ListLit: phi no es una conjuncion de literales, phi = Dis (Var "p") (Var "q")


-- Dada phi en DNF, representada como una lista de listas de literales phi,
-- termListSat determina si alguno de los terminos de phi es satisfactible.
termListSat :: [[PL]] -> Bool
termListSat phi = case phi of
            []     -> False
            (l:ls) -> terminoSat l || termListSat ls
-- Ejemplos:
-- termListSat [[]]                                                --> True
-- termListSat [[Var"p"]]                                          --> True
-- termListSat [[Var"p",(Neg(Var "p"))]]                           --> False
-- termListSat [[Var "p",Var "q"],[Var "r",Var "s"]]               --> True
-- termListSat [[Var "p",Var "q"],[(Neg(Var "p")),Var "r"]]        --> True
-- termListSat [[Var "p",(Neg(Var "p"))],[Var "q",Var "r"]]        --> True
-- termListSat [[Var "p",(Neg(Var "p"))],[Var "q",(Neg(Var "q"))]] --> False


-- Dada phi en PL, decide si phi pertenece, o no, a SAT := {phi in PL | Existe m : m |= phi}.
-- Esto se hace transformando primero phi a una fórmula en DNF representada mediante una lista de listas de literales,
-- y luego aplicando termListSat a dicha lista.
decideDNFenSAT :: PL -> Bool
decideDNFenSAT phi = termListSat (dnf2LListLit phi)
-- Ejemplos:
-- decideDNFenSAT Top                                                                                --> True
-- decideDNFenSAT Bot                                                                                --> dnf2LListLit: phi no esta en dnf, phi = Bot
-- decideDNFenSAT (Var "p")                                                                          --> True
-- decideDNFenSAT ((Var "p") `PL.Con` (Var "q"))                                                     --> True
-- decideDNFenSAT ((Var "p") `PL.Con` (Neg(Var "p")))                                                --> False
-- decideDNFenSAT ((Var "p") `PL.Dis` (Var "q"))                                                     --> True
-- decideDNFenSAT (((Var "p") `PL.Con` (Var "q")) `PL.Dis` ((Var "r") `PL.Con` (Var "s")))           --> True
-- decideDNFenSAT (((Var "p") `PL.Dis` (Var "q")) `PL.Con` ((Var "r") `PL.Dis` (Var "s")))           --> conLit2ListLit: phi no es una conjuncion de literales, phi = Dis (Var "p") (Var "q")
-- decideDNFenSAT (((Var "p") `PL.Dis` (Var "q")) `PL.Dis` ((Var "r") `PL.Dis` (Var "s")))           --> True
-- decideDNFenSAT (((Var "p") `PL.Con` (Neg(Var "p"))) `PL.Dis` ((Var "q") `PL.Con` (Var "r")))      --> True
-- decideDNFenSAT (((Var "p") `PL.Con` (Neg(Var "p"))) `PL.Dis` ((Var "q") `PL.Con` (Neg(Var "q")))) --> False