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
--courrielValide = error "à acompléter"
-- courrielValide xs = if not '@'  `elem` xs then return False
courrielValide xs
                | '@' `notElem`  xs = False
                | domain /= "@smail.ca" = False
                | otherwise = True
                where (name, domain) = splitByIndex '@' xs



getIndexOf:: Eq a => a -> [a] -> Int
getIndexOf x xs = fromJust $ elemIndex x xs

splitByIndex:: Eq a =>  a -> [a]-> ([a], [a])
splitByIndex x xs = splitAt (getIndexOf x xs) xs

-- takes a name and a list of allowed chars, 
-- Compare each char of name to allowed chars
--heckName:: Eq a => [a] -> [a]-> Bool
checkName [] = False
checkName (x:xs1) 
    | elem x numbers = True 
    | elem x caracters && isLower x = True 
    | otherwise = False 
    where numbers = getDigits xs1
          caracters = getCaracters xs1


getDigits :: [Char] -> [Char ]
getDigits [] = []
getDigits xs = filter isDigit xs

getCaracters:: [Char ] -> [Char ]
getCaracters [] = []
getCaracters (x:xs)
    | isDigit x =  getCaracters xs
    | otherwise = x : getCaracters xs

