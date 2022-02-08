{-|
Module      : CompteSmail
Description : Module pour la gestion des comptes de la messagerie SmartMail
Copyright   : (c) Ange Tato
License     : GPL-3
Maintainer  : nyamen_tato.ange_adrienne@uqam.ca
Stability   : experimental

Ce module offre les fonctionalités permettant de manipuler les comptes SmartMail. 
 -}
module CompteSmail where 


import Prelude


import Personne
import Trame
type Contact = (Personne, Etat)
type Explications = String 
data Etat = Noir | Blanc deriving (Show, Eq, Read) -- Noir = contact dans liste noire (bloqué) , Blanc contact non bloqué
type Reception = [Trame] -- boîte de reception
type Envoi = [Trame] -- boîte d'envoi
type Spams = [(Trame, Explications)] -- boîte des spams
type Preferences = [Trame -> Bool]
data CompteSmail  = CompteSmail Personne Reception Envoi Spams Preferences [Contact] -- Compte smail 
-- Vous devez faire du type CompteSmail une instance de la classe Show. Vous devez donc redéfinir la fonction show pour ce type. Voir les exemples pour voir comment l'affichage est gérer

listeContacts [] = []
listeContacts ((p,e):xs) = (courriel p, e):listeContacts xs

-- | Retourne la personne à qui appartient le compte Smail
personne :: CompteSmail -> Personne  -- boîte des spams
personne (CompteSmail p _ _ _ _ _) = p

-- | Retourne la liste des messages spams d'un compte Smail
spams :: CompteSmail -> Spams  -- boîte des spams
spams (CompteSmail _ _ _ s _ _) = s

-- | Retourne la liste des messages de la boîte d'envoi d'un compte Smail
envoi :: CompteSmail -> Envoi  -- boîte des messages envoyés
envoi  (CompteSmail _ _ e _ _ _)= e  

-- | Retourne la liste des messages de la boîte  de reception d'un compte Smail
reception :: CompteSmail -> Reception  -- boîte des messages reçus
reception (CompteSmail _ r _ _ _ _) = r

-- | Retourne la liste des préférences d'un compte Smail
--
-- filtres ou contraintes imposés par le titulaire d'un compte smail 
-- exemple: je ne veux aucun message dont le courriel de l'expéditeur se termine pas ".zz"
--          si la préférence n'est pas satisfaite, le message est automatiquement redirigé dans la boîte des spams
preferences :: CompteSmail  -> Preferences 
preferences (CompteSmail _ _ _ _ pr _) = pr

-- | Retourne la liste de tous les contacts d'un compte Smail
contacts :: CompteSmail  -> [Contact] 
contacts (CompteSmail _ _ _ _ _ c) = c 

instance Show CompteSmail where
      show (CompteSmail (Personne cu _) o1 o2 o3 _ element )=  "CompteSmail " ++ show cu++":"  ++ 
                                                        "\nRecus = " ++ show o1 ++"," ++ 
                                                       "\nEnvois = " ++ show o2++","  ++ 
                                                       "\nSpams = " ++ show o3++","  ++ 
                                                       "\nContacts = "  ++ show (listeContacts element)

-------------------------------------------------------------------
--------------------------NE PAS MODIFIER--------------------------
-------------------------------------------------------------------

-- Quelques données utilisées dans les tests
pers0 = Personne "equipesmartmail@smail.ca" ("equipe","smail")
pers1 = Personne "tato.ange@smail.ca" ("ange","tato")
pers2 = Personne "nkambou.roger@smail.ca" ("roger","nkambou")
pers3 = Personne "robert.julien@smail.ca" ("julien","robert")
pers4 = Personne "noel.alice@smail.ca" ("alice","noel")
pers5 = Personne "bourassa.alex@smail.ca" ("alex","bourassa")
pers6 = Personne "ariane.carotte@techno.co" ("arianne","carotte")
pers7 = Personne "pablo.adamo@blob.es" ("olivier","adam")
pers8 = Personne "michel.desrosiers@blob.ca" ("michel","desrosiers")
pers9 = Personne "mimi.lafleur@smail.ca" ("mimi","lafleur")
pers10 = Personne "adam.ronelle@smail.ca" ("Adam", "Ronelle")
pers11 = Personne "gabrielle.joyce@smail.ca" ("Gabrielle", "Joyce")
pers12 = Personne "marsu.pilami@smail.ca" ("Marsu", "Pilami")
pers13 = Personne "satan.peticoeur@smail.ca" ("Satan", "Peticoeur")

-- Exemples de trame de message
trameBienvenue1 = (Trame (Entete (Date 2021 02 10) "Bienvenue" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trameBienvenue2 = (Trame (Entete (Date 2021 02 10) "Bienvenue" pers0 [pers2] [] []) "Bienvenue dans votre boite smartMail !")
trame1 = (Trame (Entete (Date 2021 01 18) "AB CD EF" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trame2 = (Trame (Entete (Date 2020 12 21) "Bi!en! venue!" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trame3 = (Trame (Entete (Date 2021 01 01) "?Bien venue?" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !")  
trame4 = (Trame (Entete (Date 2019 10 05) "" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trame5 = (Trame (Entete (Date 2018 05 07) "Bienvenue $ " pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trame6 = (Trame (Entete (Date 2019 05 09) "Bienvenue" pers1 [pers3] [] []) "Allo Robert") 
trame7 = (Trame (Entete (Date 2020 18 10) "un message" pers0 [pers1] [] []) "Bienvenue dans sexe du viagra chaud nu") 
trame8 = (Trame (Entete (Date 2021 01 15) "un message" pers0 [pers1] [] []) "offre de Bienvenue dans  publicite gratuit pour voyage special") 
trame9 = (Trame (Entete (Date 2021 10 10) "un message" pers0 [pers1] [] []) "0de 3Bienvenue dans 1 et 9 | pour tp1et2") 
trame10 = (Trame (Entete (Date 2021 02 01) "un message" pers0 [pers1] [] []) "bien1venue") 
trame11 = (Trame (Entete (Date 2021 10 18) "un message" pers0 [pers1] [] []) "allo|allo") 
trame12 = (Trame (Entete (Date 2020 11 17) "Salut ange" pers3 [pers1] [] []) "Salut Ange tu vas bien ?") 
trame13 = (Trame (Entete (Date 2021 01 18) "Salut roger" pers3 [pers2] [] []) "special voyage demain, viens vite" ) 
trame14 = (Trame (Entete (Date 2021 03 18) "Hola mimi" pers7 [pers9] [] []) "como estas ?" ) 
trame15 = (Trame (Entete (Date 2021 02 09) "Bingo" pers6 [pers9] [] []) "J'ai trouve ce que tu cherchais hier" ) 
trame16 = (Trame (Entete (Date 2021 01 07) "Par rapport a Ivan" pers8 [pers9] [] []) "Ivan ne viendra pas demain ?" ) 
trame17 = (Trame (Entete (Date 2022 02 14) "Je suis le prince de Namek" pers13 [pers1] [pers10] [pers11, pers12]) "Je suis satan petit coeur et je viens de la planete Namek." ) 

tramet = (Trame (Entete (Date 2021 01 13) "Cool" pers4 [pers5] [] []) "Allo Alex") 
tramett = (Trame (Entete (Date 2021 01 25) "Nouvelles" pers2 [pers5] [] []) "Tu vas bien ?") 

-- Exemples de compte smail
csmail0 = CompteSmail pers0 [] [trameBienvenue1, trameBienvenue2] [] [] []
csmail1 = CompteSmail pers1 [trameBienvenue1] [] [] [] [(pers2,Blanc),(pers4,Noir)]
csmail2 = CompteSmail pers2 [trameBienvenue2] [] [] [] []
csmail3 = CompteSmail pers3  [] [] [] [] []
csmail4 = CompteSmail pers4  [] [] [] [] [(pers1,Blanc)] 
csmail5 = CompteSmail pers5  [] [] [] [] [(pers1,Blanc)]
csmail6 = CompteSmail pers9  [] [] [] [\(Trame (Entete d ob p1 _ _ _) c) -> (tail $ dropWhile (/='.') (dropWhile (/='@') (courriel p1))) /= "es" ] []

csmail21 = CompteSmail pers10  [] [] [] [] [(pers11,Blanc), (pers13,Blanc)] 
csmail22 = CompteSmail pers11  [] [] [] [] [(pers10,Blanc),(pers12,Noir),(pers13,Noir)] 
csmail23 = CompteSmail pers12  [] [] [] [] [] 
csmail24 = CompteSmail pers13  [] [] [] [] [] 

-------------------------------------------------------------------
-----------------FIN DE LA ZONE À NE PAS MODIFIER------------------
-------------------------------------------------------------------


-- | Ajouter un contact
--
-- Les paramètres sont : les informations du contact à ajouter, et le compte à modifier
-- Note1: De base, un contact est ajouté avec l'état = Blanc et en entête de la liste de contacts. 
-- Note2: Pas besoin de vérifier si le courriel est bon ou pas car le courriel passé en paramètre sera vérifié (pas par vous) avant d'être envoyé à cette fonction.
-- Note3: Vous devez vous assurez que le contact n'existe pas déjà
--
-- >>> csmail4' = ajouterContact "robert.julien@smail.ca" "julien" "robert" csmail4
-- >>> csmail4'
-- CompteSmail "noel.alice@smail.ca":
-- Recus = [],
-- Envois = [],
-- Spams = [],


ajouterContact :: Courriel -> Prenom -> Nom -> CompteSmail -> CompteSmail 
ajouterContact a b c d = if (elem a ((map (Personne.courriel) (map fst (contacts d) )))) then
                             d 
                         else 
                             (CompteSmail (personne d) (reception d)
                             (envoi d) (spams d) (preferences d)                             
                             ((Personne a  (b,c), Blanc):(contacts d))
                             ) 

 





-- | bloquer un contact
-- Les paramètres sont : le compte à modifier, le contact à bloquer, le compte modifié
--
-- >>> csmail4'' = ajouterContact "robert.julien@smail.ca" "julien" "robert" $ ajouterContact "bourassa.alex@smail.ca" "alex" "bourassa" csmail4
-- >>> bloquerContact csmail4'' pers5  
-- CompteSmail "noel.alice@smail.ca":
-- Recus = [],
-- Envois = [],
-- Spams = [],
-- Contacts = [("robert.julien@smail.ca",Blanc),("bourassa.alex@smail.ca",Noir),("tato.ange@smail.ca",Blanc)]
bloquerContact :: CompteSmail -> Personne -> CompteSmail 
bloquerContact = error " à compléter"


-- | Supprimer messages de la boîte de reception, d'envoi ou de spams d'un compte en fonction d'un filtre.
-- Tous les messages passant le filtre doivent être supprimés de la boîte spécifié.
-- Les paramètres : Le compte à vider, le type de la boîte : Spams, Envoi ou Reception, un filtre, le comptre modifié
--
-- >>> csmail4_1 = CompteSmail (Personne "noel.alice@smail.ca" ("alice","noel")) [trame3,trame4] [trame5,trame6] [(trame1,"majuscules"),(trame2,"points d'exclamation")] [] [(Personne "bourassa.alex@smail.ca" ("alex","bourassa"),Blanc),(Personne "robert.julien@smail.ca" ("julien","robert"),Blanc)]
-- >>> reception $ supprimerMessagesAvecFiltre csmail4_1 "Reception" (\x -> elem '?' $ objet x)
-- [Trame (Entete (Date 2019 10 5) "" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !"]
-- >>> envoi $ supprimerMessagesAvecFiltre csmail4_1 "Envoi" (\x -> annee (date x) == 2018)
-- [Trame (Entete (Date 2019 5 9) "Bienvenue" (Personne "tato.ange@smail.ca" ("ange","tato")) [Personne "robert.julien@smail.ca" ("julien","robert")] [] []) "Allo Robert"]
-- >>> supprimerMessagesAvecFiltre csmail4_1 "Spams" (\x -> all isUpper (filter isAlpha $ objet x))
-- CompteSmail "noel.alice@smail.ca":
-- Recus = [Trame (Entete (Date 2021 1 1) "?Bien venue?" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !",Trame (Entete (Date 2019 10 5) "" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !"],
-- Envois = [Trame (Entete (Date 2018 5 7) "Bienvenue $ " (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !",Trame (Entete (Date 2019 5 9) "Bienvenue" (Personne "tato.ange@smail.ca" ("ange","tato")) [Personne "robert.julien@smail.ca" ("julien","robert")] [] []) "Allo Robert"],
-- Spams = [(Trame (Entete (Date 2020 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","points d'exclamation")],
-- Contacts = [("bourassa.alex@smail.ca",Blanc),("robert.julien@smail.ca",Blanc)]

supprimerMessagesAvecFiltre :: CompteSmail  -> String ->(Trame -> Bool)-> CompteSmail 
supprimerMessagesAvecFiltre = error " à compléter"
