# Algorithme_Initial_Amelioration_Ventes

Ceci est un __algorithme de remplissage des canaux de ventes__ amélioré qui se base sur les __corrélations__ entre les produits pour savoir quels produits sont les plus liés à un certain produit cible.  
Cet algorithme permet d'établir des régles de ventes, par exemple, si un client achète __x__ quantités d'un produit __A__ et il achète __y__ quantités d'un produit __B__, alors on estime qu'il va acheter __3x+2b__ quantités d'un autre produit noté __C__.  
La démarche suivie consiste à déterminer pour chaque produit cible les __5 produits les plus corrélés__ avec lui et de construire ensuite un modèle de __régression linéaire__ pour établir les régles citées dans le paragraphe précédent.  
Le code est constitué de plusieurs étapes parmi lesquelles on peut trouver:  

-__Data préparation__ pour que les données soient dans le format attendu en effectuant des opérations d'aggrégation  
-__Feature séléction__ qui consiste à déterminer pour chaque produit cible les __5 produits les plus corrélés__ avec lui  
-__Application du modèle__ de __régression linéaire__ pour établir les régles de ventes  

