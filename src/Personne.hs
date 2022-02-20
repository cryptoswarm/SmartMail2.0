--import Data.Foldable
--import Data.List

module Personne where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Char
import Data.List.Split

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

-- | Retourne le courriel d'une personne
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
                | not(checkName' xs) = False 
                | otherwise = True

checkName' :: [Char] -> Bool
checkName' [] = False
checkName' xs = foldl (\acc x -> (x `elem` accepted || isAsciiLower x ||  isDigit x) && acc ) True (head content) &&  lengthAndDomain
            where content = splitOn "@" xs
                  accepted = ['-','_', '.' ]
                  lengthAndDomain =  length content == 2 && last content == "smail.ca"
