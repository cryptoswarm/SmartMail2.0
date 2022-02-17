{-|
Module      : Trame
Description : Module pour la gestion des trames
Copyright   : (c) Ange Tato 
License     : GPL-3
Maintainer  : nyamen_tato.ange_adrienne@uqam.ca
Stability   : experimental

Ce module offre les fonctionalités permettant de manipuler des trames de messages. 
 -}

module Trame where

import Personne
import Data.Time.Clock
import Data.Time.Calendar
import System.IO.Unsafe
import Language.Haskell.TH (Callconv(Prim))

data TypeMessage = Spam | NonSpam deriving (Show, Eq, Read)
type Annee = Integer
type Mois = Int
type Jour = Int

data Date = Date Annee Mois Jour deriving (Show, Eq, Read, Ord)
dateAuj = unsafeDupablePerformIO (getCurrentTime >>= return . toGregorian . utctDay)

type Contenu = String

type Objet = [Char]

data Priorite = Important | Normal | Faible deriving (Show, Eq, Read)
data Entete = Entete Date Objet Personne [Personne] [Personne] [Personne] deriving  (Show,Eq, Read)
data Trame = Trame Entete Contenu deriving  (Show, Read, Eq)
type Message = (Courriel, [Courriel],[Courriel],[Courriel], Objet, Contenu) -- emetteur, receveurs principaux, ccs, ccis, objet, contenu 

{-Build a trame from a msg -}
todayDate = Date (year dateAuj) (month dateAuj) (day dateAuj)
enteteFromMsg :: Message -> Entete
enteteFromMsg msg = Entete todayDate  (msgObj msg) (msgEmetter msg) (msgRecepteurs msg) (msgCC msg) (msgCCi msg)

buildTrame :: Message -> (Message -> Entete) -> (Message -> Contenu) -> Trame
buildTrame msg f1 f2 = Trame (f1 msg)  (f2 msg)

year :: (Integer, Int, Int) -> Integer
year (y, _, _) = y

month :: (Integer, Int, Int) -> Int
month (_, m, _) = m

day :: (Integer, Int, Int) -> Int
day (_, _, d) = d


-- | Retourne la date d'envoi d'un message
date :: Trame -> Date
date (Trame (Entete d _ _ _ _ _) _ ) = d

-- | Retourne l'année d'une date'
annee :: Date -> Annee
annee (Date a _ _) = a

-- | Retourne le courriel de l'metteur d'un message
emetteur (Trame (Entete _ _ e _ _ _) _ ) = courriel e

-- | Retourne la liste des courriels du ou des recepteurs principaux d'un message
receveurs :: Trame -> [String]
receveurs (Trame (Entete _ _ _ r _ _) _ ) = map courriel r

-- | Retourne la liste des courriels du ou des recepteurs en copie conforme dans un message
receveurCc :: Trame -> [String]
receveurCc (Trame (Entete _ _ _ _ rcc _) _ ) = map courriel rcc

-- | Retourne la liste des courriels du ou des recepteurs en Cci dans un message
receveurCci :: Trame -> [String]
receveurCci (Trame (Entete _ _ _ _ _ rcci) _ ) = map courriel rcci

-- | Retourne l'objet d'un message
objet :: Trame -> String
objet (Trame (Entete _ o _ _ _ _) _ ) = o

-- | Retourne le contenu d'un message
contenu :: Trame -> String
contenu (Trame _ c ) = c


-- Message = (Courriel, [Courriel],[Courriel],[Courriel], Objet, Contenu)
msgEmetter :: Message -> Personne
msgEmetter (c, _, _, _, _, _) = Personne c ("", "")

msgRecepteurs :: Message -> [Personne]
msgRecepteurs (_, r, _, _, _, _) = map (\x -> Personne x  ("", "")) r

msgCC :: Message -> [Personne ]
msgCC (_, _, cc, _, _, _) = map (\x -> Personne x  ("", "")) cc

msgCCi :: Message -> [Personne ]
msgCCi (_, _, _, cci, _, _) = map (\x -> Personne x  ("", "")) cci

msgObj :: Message -> Objet
msgObj (_, _, _, _, o, _) = o

msgContenu :: Message -> Contenu
msgContenu (_, _, _, _, _, cn) = cn



emailEmetter :: Message -> Courriel 
emailEmetter (c, _, _, _, _, _) = c

emailRecepteurs :: Message -> [Courriel]
emailRecepteurs (_, r, _, _, _, _) = r

emailCC :: Message -> [Courriel]
emailCC (_, _, cc, _, _, _) = cc

emailCCi :: Message -> [Courriel]
emailCCi (_, _, _, cci, _, _) =  cci

emailObj :: Message -> Objet
emailObj (_, _, _, _, o, _) = o

emailContenu :: Message -> Contenu
emailContenu (_, _, _, _, _, cn) = cn