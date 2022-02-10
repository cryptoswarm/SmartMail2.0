import CompteSmail
-- bloquerContact :: CompteSmail -> Personne -> CompteSmail
-- --bloquerContact = error " à compléter"
-- bloquerContact compte p = do
--                               result <- trouverContact ( courriel p ) ( contacts compte )
--                           case result of 
--                               Just contact -> CompteSmail (personne compte)
--                                           (reception compte) (envoi compte) (spams compte)
--                                           (preferences compte) ( bloquerContact'( splitListContact (contacts compte)))
--                               Nothing -> compte
--pers5 = Personne "bourassa.alex@smail.ca" ("alex","bourassa")
-- | contact = (trouverContact ( courriel p ) ( contacts compte )) == Nothing = compte
-- | otherwise = compte

compteSmail = CompteSmail
                pers0
                [trame1]
                [trame11]
                [(trame15, "spam15")]
                []
                [(pers10, Blanc)]