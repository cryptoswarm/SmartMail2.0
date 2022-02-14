module SmartMail where

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Char
import Personne
import Trame
import CompteSmail
import System.Directory


type SmartMail = Map.Map Courriel CompteSmail -- Dictionnaire de comptes smail
first_third (f,_,t) = (f,t) -- Petite fonction utilisée dans les tests

-- | Retourne un SmartMail vide
--
-- >>> nombreCompteSmail emptySmartMail
-- 0
emptySmartMail :: SmartMail
emptySmartMail = Map.fromList []


-- | Ajout d'un compteSmail 
--
-- Note: Si un compte existe déjà avec le même courriel alors ne rien faire
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> courrielsComptes s1
-- ["nkambou.roger@smail.ca","tato.ange@smail.ca"]
ajoutCompte :: CompteSmail -> SmartMail-> SmartMail
--ajoutCompte _ dict = error "Vous devez fournir un compteSmail"
ajoutCompte compte dict
                      | Map.member ( courriel ( personne compte ) ) dict = dict
                      | otherwise = Map.insert (courriel ( personne compte )) compte dict

-- | Ajout de plusieurs comptes Smail 
--
-- >>> courrielsComptes $ ajoutComptes [csmail1, csmail2] emptySmartMail
-- ["nkambou.roger@smail.ca","tato.ange@smail.ca"]
ajoutComptes::  [CompteSmail] -> SmartMail -> SmartMail
-- ajoutComptes xs sMail = foldl (flip ajoutCompte) sMail xs
ajoutComptes xs sMail = foldl (\sMail x  -> ajoutCompte x sMail) sMail xs

-- | Affiche tous les courriels de comptes Smail
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> courrielsComptes s1
-- ["nkambou.roger@smail.ca","tato.ange@smail.ca"]
-- >>>courrielsComptes emptySmartMail
-- []
courrielsComptes ::SmartMail-> [Courriel]
courrielsComptes = Map.keys

-- | Retourne nombre de CompteSmail contenu dans SmartMail
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> nombreCompteSmail s1
-- 2
-- >>> nombreCompteSmail emptySmartMail
-- 0
nombreCompteSmail :: SmartMail -> Int
nombreCompteSmail = Map.size

-- | Retoune le comptre Smail associé à un courriel dans SmartMail
-- Note : On suppose que le courriel passé en argumemnt est associé à un compte existant du smartmail
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> obtenirCompte "tato.ange@smail.ca" s1
-- CompteSmail "tato.ange@smail.ca":
-- Recus = [Trame (Entete (Date 2021 2 10) "Bienvenue" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !"],
-- Envois = [],
-- Spams = [],
-- Contacts = [("nkambou.roger@smail.ca",Blanc),("noel.alice@smail.ca",Noir)]
obtenirCompte :: Courriel -> SmartMail -> CompteSmail
obtenirCompte c sMail = fromJust $ Map.lookup c sMail


-- | Déterminer la priorité d'un message non spam, pour chacune des personnes dans les listes receveurs, cc et cci
-- Note: on suppose que le message est un message non spam.
-- Cette fonction retourne une liste de priorité correspondant à chacun des:
                -- receveurs, 
                -- les personnes en Cc 
                -- et en Cci.
--    (1) Important= expediteur est un contact direct (est dans la liste de contacts du recepteur) 
                -- qui n'est pas bloqué ou est "equipesmartmail@smail.ca" 
--    (2) Normal = expediteur est de niveau 2 
                -- (est dans la liste de contacts d'au moins un des contacts du recepteur et n'est pas bloqué). 
                -- Si il est bloqué par au moins un contact, alors il passe au niveau suivant.
--    (3) Faible = expediteur n'est ni un contact direct, n'est ni de niveau 2, n'est ni l'équipe smartMail.ca
--
-- >>> s1 = ajoutCompte csmail0 $ ajoutCompte csmail6 $ ajoutCompte csmail5 $ ajoutCompte csmail4 $ ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> prioritesMessage s1 (courriel pers0, map courriel [pers2, pers3], map courriel [pers2, pers3],[], "Bienvenue", "Bienvenue dans votre boite smartMail !")
-- [[Important,Important],[Important,Important],[]]
-- >>> prioritesMessage s1 (courriel pers1, map courriel [pers5, pers2], map courriel [pers4], map courriel [pers9, pers0, pers4 ], "Cool", "Allo Alex, mon premier message :).")
-- [[Important,Faible],[Important],[Faible,Faible,Important]]
-- >>> prioritesMessage s1 (courriel pers2, map courriel [pers5, pers2], map courriel [pers4], map courriel [pers9, pers0, pers4 ], "Cool", "Allo Alex, mon premier message :).")
-- [[Normal,Faible],[Normal],[Faible,Faible,Normal]]
prioritesMessage :: SmartMail -> Message -> [[Priorite]]
prioritesMessage sMail (c, r, ccs, ccis, _, _)
               | c == "equipesmartmail@smail.ca" = [replicate (length r)  Important, replicate (length ccs) Important, replicate (length ccis) Important]
               | otherwise = [pMsgRecp c sMail r, pMsgRecp c sMail ccs, pMsgRecp c sMail ccis]



-- pMsgRecp :: Courriel ->  SmartMail -> [Courriel] -> [Priorite]
-- pMsgRecp sender sm [] = []
-- pMsgRecp sender sm (x:xs)
--                         | isLevelOne sender (contacts $ fromJust $ Map.lookup x sm) = Important : pMsgRecp sender sm xs
--                         | otherwise = Faible : pMsgRecp sender sm xs

pMsgRecp :: Courriel ->  SmartMail -> [Courriel] -> [Priorite]
pMsgRecp sender sm [] = []
pMsgRecp sender sm (x:xs)
                        | isLevelOne sender (contacts $ fromJust $ Map.lookup x sm) = Important : pMsgRecp sender sm xs
                        | isLevelTwo sender (contacts $ fromJust $ Map.lookup x sm) sm = Normal : pMsgRecp sender sm xs
                        | otherwise = Faible : pMsgRecp sender sm xs
                         

isLevelOne :: Courriel ->  [Contact] -> Bool
isLevelOne c [] = False 
isLevelOne c xs = foldl (\acc x -> if (courriel (fst x ) == c && snd x == Blanc) then True else acc) False xs

--foldl (\acc x -> if x == y then True else acc) False ys 

isLevelTwo :: Courriel -> [Contact] -> SmartMail  -> Bool
isLevelTwo c [] sm = False 
isLevelTwo c xs sm = foldl (\acc x -> if isLevelOne c  (getContacts x sm ) then True else acc) False xs



getContacts :: Contact -> SmartMail -> [Contact]
getContacts x sm = contacts $ fromJust $ Map.lookup (courriel (fst x )) sm

-------------------------------------------------------------------
---------- FILTRES ANTI SPAM ET ANTI HAMEÇONNAGE ------------------ 
-------------------------------------------------------------------
-- | Filtrage de l'enveloppe de la trame
-- Ce filtre s’exécute uniquement sur l'entête de la trame et non sur son contenu.
-- Une trame est détectée comme spam par le filtrageEnveloppe si au moins l'une des conditions suivantes est vraie: 
--      (1) tous les caractères de l'objet sont en majuscules (Explications = "classique_enveloppe")
--      (2) l'objet contient au moins 2 points d'exclamation "!" (Explications = "classique_enveloppe")
--      (3) l'objet contient au moins 2 points d'interrogation "?" (Explications = "classique_enveloppe")
--      (4) l'objet est vide (Explications = "objet vide")
--      (5) l'objet contient le caractère "$" (Explications = "classique_enveloppe")
--      (6) l'emetteur a été bloqué (Etat = Noir) par le destinataire   (Explications = "contact bloque")  
-- Le courriel passé en paramètre à cette fonction servira pour le point (6). Le filtre est fait par rapport à la personne ayant le courriel passé en paramètre.
--
-- >>> ssm2 = Map.fromList [("nkambou.roger@smail.ca", CompteSmail (Personne "nkambou.roger@smail.ca" ("roger","nkambou")) [Trame (Entete (Date 2019 10 2) "Bienvenue" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !"] [] [] [] []),("robert.julien@smail.ca",CompteSmail (Personne "robert.julien@smail.ca" ("julien","robert")) [] [] [] [] [(Personne "tato.ange@smail.ca" ("",""),Noir)]),("tato.ange@smail.ca", CompteSmail (Personne "tato.ange@smail.ca" ("ange","tato")) [Trame (Entete (Date 2019 10 2) "Bienvenue" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !"] [] [] [] [])]
-- >>> filtrageEnveloppe trame6 ssm2 "robert.julien@smail.ca"
-- (Spam,Trame (Entete (Date 2019 5 9) "Bienvenue" (Personne "tato.ange@smail.ca" ("ange","tato")) [Personne "robert.julien@smail.ca" ("julien","robert")] [] []) "Allo Robert","contact bloque")
-- >>> filtrageEnveloppe trame1 ssm2 "tato.ange@smail.ca"
-- (Spam,Trame (Entete (Date 2021 1 18) "AB CD EF" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe")
-- >>> filtrageEnveloppe trame2 ssm2 "tato.ange@smail.ca"
-- (Spam,Trame (Entete (Date 2020 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe")
-- >>> filtrageEnveloppe trame3 ssm2 "tato.ange@smail.ca"
-- (Spam,Trame (Entete (Date 2021 1 1) "?Bien venue?" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe")
-- >>> filtrageEnveloppe trame4 ssm2 "tato.ange@smail.ca"
-- (Spam,Trame (Entete (Date 2019 10 5) "" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","objet vide")
-- >>> filtrageEnveloppe trame5 ssm2 "tato.ange@smail.ca"
-- (Spam,Trame (Entete (Date 2018 5 7) "Bienvenue $ " (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe")
-- >>> filtrageEnveloppe trameBienvenue1 ssm2 "tato.ange@smail.ca"
-- (NonSpam,Trame (Entete (Date 2021 2 10) "Bienvenue" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","")
-- >>> filtrageEnveloppe trameBienvenue2 ssm2 "nkambou.roger@smail.ca" 
-- (NonSpam,Trame (Entete (Date 2021 2 10) "Bienvenue" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "nkambou.roger@smail.ca" ("roger","nkambou")] [] []) "Bienvenue dans votre boite smartMail !","")
-- >>> s1 = ajoutComptes [csmail1, csmail21, csmail22, csmail23, csmail24] emptySmartMail
-- >>> filtrageEnveloppe trame17 s1 "gabrielle.joyce@smail.ca"
-- (Spam,Trame (Entete (Date 2022 2 14) "Je suis le prince de Namek" (Personne "satan.peticoeur@smail.ca" ("Satan","Peticoeur")) [Personne "tato.ange@smail.ca" ("ange","tato")] [Personne "adam.ronelle@smail.ca" ("Adam","Ronelle")] [Personne "gabrielle.joyce@smail.ca" ("Gabrielle","Joyce"),Personne "marsu.pilami@smail.ca" ("Marsu","Pilami")]) "Je suis satan petit coeur et je viens de la planete Namek.","contact bloque")
-- >>> filtrageEnveloppe trame17 s1 "adam.ronelle@smail.ca"
-- (NonSpam,Trame (Entete (Date 2022 2 14) "Je suis le prince de Namek" (Personne "satan.peticoeur@smail.ca" ("Satan","Peticoeur")) [Personne "tato.ange@smail.ca" ("ange","tato")] [Personne "adam.ronelle@smail.ca" ("Adam","Ronelle")] [Personne "gabrielle.joyce@smail.ca" ("Gabrielle","Joyce"),Personne "marsu.pilami@smail.ca" ("Marsu","Pilami")]) "Je suis satan petit coeur et je viens de la planete Namek.","")

filtrageEnveloppe :: Trame -> SmartMail -> Courriel -> (TypeMessage, Trame, Explications)
filtrageEnveloppe = error " à compléter"


-- | Filtrage du contenu
-- Les filtres de contenu analysent le contenu des messages et détectent les spams qui ont réussi à passer à travers le filtre d'enveloppe.
-- Le principe consiste généralement à détecter des mots précis ou des mots de formes particulières, par exemple des mots qui contiennent des chiffres ou certains symboles (C|AL|S, -1AGRA, 
-- PR0ZAC, ZYBAN and C3LEBREX cred1ted. Il y a donc deux sous catégories d'heuristiques: la détection des mots clés (heuristiqueMotsCles) et la recherche de mots comportant des caractères spéciaux (heuristiqueCaracteresSpeciaux). 
-- Un poids (proportion de mots clés ou de mots comportant les caractères spéciaux) de l'ordre de 10% suffit pour déclarer le message comme spam.
-- Vous devez définir les 3 filtres suivants:
--      (1) filtreSpecifiqueHams pour les mots-clés suivants: "sexe", "sexy", "viagra", "argent", "drogue", "money","credit", "$","chaud", "nu", "click", "amateur", "pics","videos", "gagner","lotterie","heritage".
--      (2) filtreSpecifiquePub pour les mots-clés "offre", "commande", "click", "videos","gratuit","publicite", "special","voyage"
--      (3) filtreCaracteresSpeciaux pour mots contenant un ou l'autre des caractères suivant '|','0'..'9'.
-- On supposera qu'un message, s'il est un spam, l'est à cause de l'un des filtres exclusivement
-- L'explication donnera les indices sur la nature du spam: "hameconnage, XX% de mots suspects", "publicitaire, XX% de mots suspects" et "classique_contenu, XX% de mots comportant des caracteres etranges" (caractères spéciaux dans les mots). 
-- L'information sur la proportion des mots suspects dans le contenu du message est requis pour les filtres de contenu. Vous devez arrondi le pourcentage. 
-- Remarque: Pour cette question, vous devez écrire des sous-fonctions nécessaires. 
--
-- >>> filtrageContenu trame7
-- (Spam,Trame (Entete (Date 2020 18 10) "un message" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans sexe du viagra chaud nu","hameconnage, 57% de mots suspects.")
-- >>> first_third $ filtrageContenu trame8
-- (Spam,"publicitaire, 56% de mots suspects.")
-- >>> first_third $ filtrageContenu trame9
-- (Spam,"classique_contenu, 67% de mots comportant des caracteres etranges.")
-- >>> first_third $ filtrageContenu trame10
-- (Spam,"classique_contenu, 100% de mots comportant des caracteres etranges.")
-- >>> first_third $ filtrageContenu trame11
-- (Spam,"classique_contenu, 100% de mots comportant des caracteres etranges.")
filtrageContenu :: Trame -> (TypeMessage, Trame, Explications)
filtrageContenu = error " à compléter"


-- | Envoyer message
-- Remarque 1: Il faut vérifier l'enveloppe de la trame ainsi que son contenu. Si c'est un spam, l'ajouter dans la boîte de spams et non de reception.
-- Remarque 2: Il peut avoir plusieurs receveurs principaux, plusieurs personnes en Cc et plusieurs personnes en Cci. Les receveurs et les personnes en Cc ne doivent pas voir les personnes en Cci. Les personnes en cci ne doivent pas voir les autres personnes en cci.
-- Remarque 3: On suppose que celui qui envoi a un compte dans SmartMail. Si un receveur n'a pas de compte SmartMail, il faut simplement l'ignorer.
-- Remarque 4: Les dates dans les messages sont les dates d'envoi de messages. Il faudra mettre la date courante dans l'entête de la Trame. Vous pouvez modifier les dates des messages dans les tests pour faire vos tests. Lors de la correction, ca sera la date courante qui sera pris en compte. Exemple: Si je corrige le 10 mars 2022 alors c'est cette date qui devra être dans l'entête des messages envoyés. 
-- 
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 $ ajoutCompte csmail0 emptySmartMail
-- >>> s2 = envoyerMessage  s1 ("equipesmartmail@smail.ca", ["tato.ange@smail.ca"],[],[], "Bi!en! venue!", "Bienvenue dans votre boite smartMail !")
-- >>> length (envoi $ obtenirCompte "equipesmartmail@smail.ca" s2)
-- 3
-- >>> length (reception $ obtenirCompte "tato.ange@smail.ca" s2)
-- 1
-- >>> s3 = ajoutCompte csmail3 s2
-- >>> s4 = envoyerMessage s3 ("equipesmartmail@smail.ca", ["tato.ange@smail.ca"],[],[], "?Bien venue?", "Bienvenue dans votre boite smartMail !")
-- >>> length (reception $ obtenirCompte "tato.ange@smail.ca" s4)
-- 1
-- >>> length (envoi $ obtenirCompte "tato.ange@smail.ca" s4)
-- 0
-- >>> s5 = ajoutComptes [csmail21,csmail22,csmail23,csmail24] s4
-- >>> s6 = envoyerMessage s5 ("satan.peticoeur@smail.ca",["tato.ange@smail.ca"],["adam.ronelle@smail.ca"],["marsu.pilami@smail.ca","gabrielle.joyce@smail.ca"],"Je suis le prince de Namek","Je suis satan petit coeur et je viens de la planete Namek.")
-- >>> obtenirCompte "tato.ange@smail.ca" s6
-- CompteSmail "tato.ange@smail.ca":
-- Recus = [Trame (Entete (Date 2022 2 22) "Je suis le prince de Namek" (Personne "satan.peticoeur@smail.ca" ("Satan","Peticoeur")) [Personne "tato.ange@smail.ca" ("ange","tato")] [Personne "adam.ronelle@smail.ca" ("Adam", "Ronelle")] []) "Je suis satan petit coeur et je viens de la planete Namek.",Trame (Entete (Date 2021 2 10) "Bienvenue" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !"],
-- Envois = [],
-- Spams = [(Trame (Entete (Date 2022 2 22) "?Bien venue?" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe"),(Trame (Entete (Date 2022 2 22) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe")],
-- Contacts = [("nkambou.roger@smail.ca",Blanc),("noel.alice@smail.ca",Noir)]
-- >>> obtenirCompte "gabrielle.joyce@smail.ca" s6
-- CompteSmail "gabrielle.joyce@smail.ca":
-- Recus = [],
-- Envois = [],
-- Spams = [(Trame (Entete (Date 2022 2 22) "Je suis le prince de Namek" (Personne "satan.peticoeur@smail.ca" ("Satan","Peticoeur")) [Personne "tato.ange@smail.ca" ("ange","tato")] [Personne "adam.ronelle@smail.ca" ("Adam", "Ronelle")] [Personne "gabrielle.joyce@smail.ca" ("Gabrielle", "Joyce")]) "Je suis satan petit coeur et je viens de la planete Namek.","contact bloque")],
-- Contacts = [("adam.ronelle@smail.ca",Blanc),("marsu.pilami@smail.ca",Noir),("satan.peticoeur@smail.ca",Noir)]
-- >>> length (reception $ obtenirCompte "tato.ange@smail.ca" s6)
-- 2
-- >>> length (reception $ obtenirCompte "satan.peticoeur@smail.ca" s6)
-- 0
-- >>> length (envoi $ obtenirCompte "satan.peticoeur@smail.ca" s6)
-- 1
envoyerMessage :: SmartMail -> Message -> SmartMail
envoyerMessage = error " à compléter"


-------------------------------------------------------------------
--------------------   STATISTIQUES ET AFFICHAGES   ---------------   
-------------------------------------------------------------------

-- | Donne le nombre de spams recus dans le systeme de messagerie au complet
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> nbTotalSpams s1
-- 0
-- >>> nbTotalSpams s
-- 8
nbTotalSpams :: SmartMail -> Int
nbTotalSpams= error " à compléter"

-- | Retourne l'ensemble de tous les spams du système dans une même liste
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> tousLesSpams s1
-- []
-- >>> tousLesSpams s
-- [(Trame (Entete (Date 2021 10 10) "un message" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("","")] [] []) "0de 3Bienvenue dans 1 et 9 | pour tp1et2","classique_contenu, 67% de mots comportant des caracteres etranges."),(Trame (Entete (Date 2021 1 15) "un message" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("","")] [] []) "offre de Bienvenue dans  publicite gratuit pour voyage special","publicitaire, 56% de mots suspects."),(Trame (Entete (Date 2020 18 10) "un message" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("","")] [] []) "Bienvenue dans sexe du viagra chaud nu","hameconnage, 57% de mots suspects."),(Trame (Entete (Date 2018 5 7) "Bienvenue $ " (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("","")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe"),(Trame (Entete (Date 2019 10 5) "" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("","")] [] []) "Bienvenue dans votre boite smartMail !","objet vide"),(Trame (Entete (Date 2020 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("","")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe"),(Trame (Entete (Date 2020 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("","")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe"),(Trame (Entete (Date 2021 1 18) "AB CD EF" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("","")] [] []) "Bienvenue dans votre boite smartMail !","classique_enveloppe")]
tousLesSpams:: SmartMail -> [(Trame, Explications)]
tousLesSpams = error " à compléter"

-- | Retourne la liste qui associe à chaque inscrit (son courriel seulement), le nombre de spams recus. Résultat par ordre alphabétique de courriels
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> statSpamsRecus s1
-- [("bourassa.alex@smail.ca",0),("equipesmartmail@smail.ca",0),("nkambou.roger@smail.ca",0),("noel.alice@smail.ca",0),("robert.julien@smail.ca",0),("tato.ange@smail.ca",0)]
-- >>> statSpamsRecus s
-- [("bourassa.alex@smail.ca",0),("equipesmartmail@smail.ca",0),("nkambou.roger@smail.ca",0),("noel.alice@smail.ca",0),("robert.julien@smail.ca",0),("tato.ange@smail.ca",8)]
statSpamsRecus :: SmartMail -> [(String, Int)]
statSpamsRecus = error " à compléter"

-- | Produit une liste qui associe à chaque inscrit (son courriel seulement), le nombre de spams produit
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> statSpamsEnvoyes s1
-- []
-- >>> statSpamsEnvoyes s
-- [("equipesmartmail@smail.ca",8)]
statSpamsEnvoyes :: SmartMail -> [(Courriel, Int)]
statSpamsEnvoyes = error " à compléter"



-- | Envoyer plusieurs trames en même temps
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> length $ spams $ obtenirCompte "tato.ange@smail.ca"  s
-- 8
envoyerMessage_Plusieurstrames :: SmartMail -> [Trame] -> SmartMail
envoyerMessage_Plusieurstrames = error " à compléter"


-- | Extraire les n premiers messages  de chacune des 3  boîtes : Reception, Envoi, Spams de chaque compte du SmartMail
-- Le résultat est un nouveau compte SmartMail dont les 3 boîtes de messageries contiennent uniquement les n premiers messages.
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> length $ spams $ obtenirCompte "tato.ange@smail.ca"  s
-- 8
-- >>> length $ spams $ obtenirCompte "tato.ange@smail.ca" $ extrairenMessages 2 s
-- 2
extrairenMessages :: Int -> SmartMail -> SmartMail
extrairenMessages = error " à compléter"

-- | Supprimer tous les messages (Reception, Envoi, Spam) du système Smartmail datant d'une date antérieure (strictement) à celle spécifiée
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9, trame10, trame11, trame12]
-- >>> s3 = supprimerOldMessages (Date 2020 10 01) s
-- >>> nbTotalSpams s
-- 10
-- >>> length $ spams $ obtenirCompte "tato.ange@smail.ca" s3
-- 8
-- >>> s4 = supprimerOldMessages (Date 2021 01 01) s
-- >>> length $ spams $ obtenirCompte "tato.ange@smail.ca"  s4
-- 5
supprimerOldMessages :: Date -> SmartMail-> SmartMail
supprimerOldMessages = error " à compléter"


-- | Reformater boîte
-- Afin de faciliter la gestion des messages, l'administrateur voudrait pouvoir reorganiser toutes les boîtes (reception, spams, envoi) de la manière suivante:
--       chaque boîte doit devenir un dictionnaire avec le courriel de l'emetteur comme clé  et comme valeur, une liste des messages (recus, envoyés, spams) de cette personne. 
--       Les messages de cette liste doivent comporter uniquement la date (au format "jj/mm/aaaa"),
--       l'objet et le contenu du message (sous forme d'un triplet). 
-- Vous devez donc écrire la fonction reformaterBoite qui reformate une boîte selon le principe décrit.
-- Donnée: Une boîte de messages (une liste de trames)
-- Sortie: Le dictionnaire correspondant. 
--
-- >>> reformaterBoite [trame7, trame8, trame9, trame10, trame11]
-- fromList [("equipesmartmail@smail.ca",[("18/10/2021","un message","allo|allo"),("1/2/2021","un message","bien1venue"),("10/10/2021","un message","0de 3Bienvenue dans 1 et 9 | pour tp1et2"),("15/1/2021","un message","offre de Bienvenue dans  publicite gratuit pour voyage special"),("10/18/2020","un message","Bienvenue dans sexe du viagra chaud nu")])]
-- >>> reformaterBoite [trame12, trame13, trame14, trame15, trame17]
-- fromList [("ariane.carotte@techno.co",[("9/2/2021","Bingo","J'ai trouve ce que tu cherchais hier")]),("pablo.adamo@blob.es",[("18/3/2021","Hola mimi","como estas ?")]),("robert.julien@smail.ca",[("18/1/2021","Salut roger","special voyage demain, viens vite"),("17/11/2020","Salut ange","Salut Ange tu vas bien ?")]),("satan.peticoeur@smail.ca",[("14/2/2022","Je suis le prince de Namek","Je suis satan petit coeur et je viens de la planete Namek.")])]
reformaterBoite :: [Trame] -> Map.Map String [(String, String, String)]
reformaterBoite = error " à compléter"




-- | Reformater compte
--  Utiliser la fonction reformaterBoite pour écrire la fonction reformaterCompte qui reconstruit  la structure d'un compte smail. Le résultat doit comporter l'ensemble des messages provenant des 
--  trois boîtes y compris celle des spams. Pour la boîte des spams, on ne s'interessera qu'aux Trames de message sans explications.
--  Donnée:  Un compte smail
--  Sortie:  Un triplet comportant les trois boîtes dans l'ordre et dans leur nouvelle format.
--
-- >>> ssm4 = Map.fromList [("nkambou.roger@smail.ca",CompteSmail (Personne "nkambou.roger@smail.ca" ("roger","nkambou")) [Trame (Entete (Date 2019 10 2) "Bienvenue" pers0 [pers2] [] []) "Bienvenue dans votre boite smartMail !"] [Trame (Entete (Date 2019 10 26) "toto" pers2 [pers1] [] []) "tato "] [] [] []),("tato.ange@smail.ca",CompteSmail (Personne "tato.ange@smail.ca" ("ange","tato")) [Trame (Entete (Date 2019 10 26) "toto" pers2 [pers1] [] []) "tato ", Trame (Entete (Date 2019 10 25) "ggg" pers4 [pers1] [] []) "jjjj",Trame (Entete (Date 2019 10 2) "Bienvenue" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !"] [] [(Trame (Entete (Date 2019 10 25) "ghgh jjhjh" pers3 [pers1] [] []) "hgg6gg jhh7hh","Classique!! Contient 100% de mots comportant des caracteres etranges.")] [] []),("toto.tartampion@smail.ca",CompteSmail (Personne "toto.tartampion@smail.ca" ("tartampion","toto")) [] [Trame (Entete (Date 2019 10 25) "ghgh jjhjh" pers3 [pers1] [] []) "hgg6gg jhh7hh",Trame (Entete (Date 2019 10 25) "ggg" pers3 [pers1] [] []) "jjjj"] [] [] [])]
-- >>> reformaterCompte $ obtenirCompte "tato.ange@smail.ca" ssm4
-- (fromList [("equipesmartmail@smail.ca",[("2/10/2019","Bienvenue","Bienvenue dans votre boite smartMail !")]),("nkambou.roger@smail.ca",[("26/10/2019","toto","tato ")]),("noel.alice@smail.ca",[("25/10/2019","ggg","jjjj")])],fromList [],fromList [("robert.julien@smail.ca",[("25/10/2019","ghgh jjhjh","hgg6gg jhh7hh")])])
reformaterCompte :: CompteSmail
     -> (Map.Map String [(String, String, String)],
         Map.Map String [(String, String, String)],
         Map.Map String [(String, String, String)])
reformaterCompte = error " à compléter"


-- | Filtrage par prefrences
-- Remarque : Pour simplifier, on ne considère que le recepteur principal. Je donne un bonus si vous écrivez une fonction qui retourne une liste [(TypeMessage, Trame, Explications)] pour chaque receveur (receveur, ccs et ccis).
-- Filtre une trame recu selon les préférences du recepteur principal de ces trames.
-- Si la trame respecte toutes les préférences du recepteur principal alors retourner (NonSpam, Trame, "")
-- Si la trame ne respecte pas au moins une préférence du recepteur alors retourner (Spam, Trame, "probleme de preferences")
--
-- >>> s = ajoutCompte csmail6 emptySmartMail
-- >>> map (flip (filtragePreference) s) [trame14, trame15, trame16]
-- [(Spam,Trame (Entete (Date 2021 3 18) "Hola mimi" (Personne "pablo.adamo@blob.es" ("olivier","adam")) [Personne "mimi.lafleur@smail.ca" ("mimi","lafleur")] [] []) "como estas ?","probleme de preferences"),(NonSpam,Trame (Entete (Date 2021 2 9) "Bingo" (Personne "ariane.carotte@techno.co" ("arianne","carotte")) [Personne "mimi.lafleur@smail.ca" ("mimi","lafleur")] [] []) "J'ai trouve ce que tu cherchais hier",""),(NonSpam,Trame (Entete (Date 2021 1 7) "Par rapport a Ivan" (Personne "michel.desrosiers@blob.ca" ("michel","desrosiers")) [Personne "mimi.lafleur@smail.ca" ("mimi","lafleur")] [] []) "Ivan ne viendra pas demain ?","")]
filtragePreference :: Trame -> SmartMail-> (TypeMessage, Trame, Explications)
filtragePreference = error " à compléter"
