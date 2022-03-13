# Travail pratique 1 - Messagerie SmartMail 2.0

## Description

Le but de ce travail est de compléter l'implémentation de modules Haskell permettant la gestion d'une messagerie électronique intelligente appelé SmartMail 2.0.<br>
SmartMail 2.0 permet à un utilisateur inscrit de disposer d’un service de messagerie électronique intelligent offrant trois boîtes (Envoi, Réception et Spams).<br>
Les messages suspects sont automatiquement envoyés dans la boîte des Spams. Ils sont filtrés selon un ensemble de principes ou d’heuristiques spécifiques.<br> L’utilisateur peut aussi entretenir un ensemble de contacts et éventuellement de préférences.<br>
L’état d’un contact suspect passera de « blanc » à « noir » indiquant ainsi son inscription dans la liste noire et provoquant une redirection automatique des messages reçus de ce contact dans la boîte Spams.<br>
Un service d’administration de SmartMail 2.0 est offert pour la gestion des messages et la production de quelques statistiques d’intérêt.

## Auteur

  * Mokhtar Safir (SAFM141186)
  * Mahmood Gholami (GHOM26107608)
## Fonctionnement
    
- Compilation : 
    * `ghc -o compteSmail  CompteSmail.hs`
    * `ghc -o personne  Personne.hs`
    * `ghc -o smartMail  SmartMail.hs`
- Sous l'interpreteur `ghci`:
    * charger les fichiers: 
        * `:l CompteSmail.hs`
        * `:l Personne.hs`
        * `:l SmartMail.hs`
## Contenu du projet

Le dépôt contient 4 fichiers `haskell` qui sont :<br>
  * `CompteSmail.hs`
  * `Personne.hs`
  * `SmartMail.hs`
  * `Trame.hs`

et un fichier *markdown* qui est:
  * `README.md`
## Dépendances

- Dépendances à installées
    * Le projet utilise le module `Data.List.Split` qu'on doit installer de la façon suivante:
        * `cabal install --lib split`
        * Il est à noté que ce n'est pas nécissaire d'installer quoi que ce soit si la correction est faite sur le serveur `Java` de l'UQAM. 
    * Le module se trouve dans : [Data.List.Split](https://hackage.haskell.org/package/split-0.2.3.4/docs/Data-List-Split.html) 

## Statut

Le projet est complet.
Toutes les methodes passent les test à l'exception de :<br>
  * `supprimerOldMessages`
  * Nous coyons que le probleme est relatif au test et non à notre implementation pour les raisons suivantes:
     * Ajoutant les 6 `CompteSmail` à un `SmartMail` vide :<br>
      `s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail`
     * Avant qu'on envoie des messages on a :<br>
       `nbTotalSpams s1` = `0`
     * Apres qu'on envoie des messages:<br>
         * `s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9, trame10, trame11, trame12]` 
         * `nbTotalSpams s` = `10` sachant que la date d'envoie est la date courante. Disant qu'on a envoyé aujourd'hui le `2022 3 13`
     * Quand on supprime : <br>
         * Si la date spécifiée est dans le passé :<br>
             * `s3 = supprimerOldMessages (Date 2020 10 01) s`
             * `length $ spams $ obtenirCompte "tato.ange@smail.ca" s3` devrait donné `10`
         * Si la date spécifiée est égale :<br>
             * `s3 = supprimerOldMessages (Date 2022 3 13) s`
             * `length $ spams $ obtenirCompte "tato.ange@smail.ca" s3` devrait donné `10`
         * Si la date spécifiée est dans le future :<br>
             * `s3 = supprimerOldMessages (Date 2020 3 14) s`
             * `length $ spams $ obtenirCompte "tato.ange@smail.ca" s3` devrait donné `0`

      * Nous nous ne comprenons pas d'ou vient le `8` dans le test suivant:
          * `-- >>> length $ spams $ obtenirCompte "tato.ange@smail.ca" s3`
          * `-- 8`


