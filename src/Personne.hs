--import Data.Foldable
--import Data.List

module Personne where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Char

type Nom = String
type Prenom = String
type Courriel = String
type Signature = (Prenom, Nom)
data Personne = Personne
                Courriel
                Signature
                deriving (Show, Read) -- Courriel unique
instance Eq Personne where
    (==) (Personne cour1 _ ) (Personne cour2 _ ) = cour1 == cour2

-- | Retourne lle courriel d'une personne
courriel :: Personne -> String
courriel (Personne c _)= c

-- | Vérifie si le courriel passé en paramètre est conforme. 
-- Un courriel smail conforme est de la forme <xxxx@smail.ca>
-- La partie xxxx ne peut contenir que des lettres en minuscules, des chiffres, des points ou des tirets '_' et '-' uniquement.
--
-- >>> courrielValide "tato.ange@samail.ca"
-- False
-- >>> courrielValide "ange-@tato@smail.ca"
-- False
-- >>> courrielValide "ang+-@tato@smail.ca"
-- False
-- >>> map courrielValide ["tatoooange@smail.ca", "ange.tato@smail.ca", "ange_tato@smail.ca", "Tato@smail.ca"]
-- [True,True,True,False]
courrielValide :: [Char] -> Bool
courrielValide xs
                | '@' `notElem`  xs = False
                | domain /= "@smail.ca" = False
                | not( checkName name ) = False 
                | otherwise = True
                where (name, domain) = splitByIndex '@' xs



getIndexOf:: Eq a => a -> [a] -> Int
getIndexOf x xs = fromJust $ elemIndex x xs

splitByIndex:: Eq a =>  a -> [a]-> ([a], [a])
splitByIndex x xs = splitAt (getIndexOf x xs) xs


getDigits :: [Char] -> [Char ]
getDigits [] = []
getDigits xs = filter isDigit xs

-- Get caracters from name except allowed caracters which are in list ['_', '-', '.']
getCaracters:: [Char] -> [Char] -> [Char]
getCaracters [] [] = []
getCaracters (x:xs) xs2
    | isDigit x = getCaracters xs xs2
    | x `elem` xs2 = getCaracters xs xs2
    | otherwise = x : getCaracters xs xs2


checkName:: [Char] -> Bool
checkName [] = False
checkName xs 
    | False `elem` map isAsciiLower caracters = False 
    | otherwise = True 
    where caracters = getCaracters xs ['_', '-', '.']