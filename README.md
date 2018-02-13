(***********************************************************************)
(*                                                                     *)
(*                            **OCaml**                                *)
(*                                                                     *)
(*            Victor Boone - Gabrielle PAUVERT, projet Tableur         *)
(*                           ENS de LYON                               *)
(*                                                                     *)
(*  Copyright 2018 École Normale Supérieure de Lyon.  All rights       *)
(*  reserved.  This file is distributed under the terms of the GNU     *)
(*  Library General Public License, with the special exception on      *)
(*  linking described in file ../LICENSE.                              *)
(*                                                                     *)
(***********************************************************************)


### tableur-projet2


PROJ2 : Conception d'un tableur minimaliste en OCAML.
**Niveau du rendu** : Avancé
**Binôme** : Victor BOONE - Gabrielle PAUVERT

## TYPES et CONVENTIONS

	Les nouveaux types sont listés dans `cell.ml`.
	On considère que la valeur d'une cellule `c` est à jour si et seulement si `c.value != None`.
	Le type des nombres définit et déplacé dans le fichier `numbers.ml`, où l'on donne de nombreuses méthodes pour manipuler les nombres.

	Vous pouvez mettre l'option `-naive` pour que les feuilles soient calculée de façon naïve.


## GESTION DES DÉPENDANCES

	On gère les dépendances avec une structure arborescente. Chaque cellule wlog `A1` contient une formule qui mentionne possiblement d'autres cellules, wlog `A2`. Le problème est que si la valeur de `A2` se trouve être modifiée, il faut en conséquence modifier celle de `A1`. C'est pourquoi chaque cellule contient un champs `dependancies`. C'est un arbre-AVL qui contient toutes les cellules qui dépendent directement de `A1` (c'est-à-dire qui ont `A1` dans leur formule).
	On parle dans notre code de dépendances *montantes* pour les formules qui dépendent de la valeur de la cellule courante ; et on parle de dépendances *descendantes* les formules dont la valeur de la cellule courante dépend. Ainsi, l'arbre des dépendances contient les dépendances montantes directes.


 - **Pourquoi un arbre ?**

	Les dépendances se doivent d'être une structure sans doublon. Dès lors, si on utilise des listes chaînées pour stocker ces dépendances, on a un temps d'insertion d'une nouvelle dépendance en `O(n)`, et suppression d'une dépendance en `O(n)` aussi. On utilise alors un arbre-AVL pour assurer un temps `O(n log n)` sur ces deux opérations. Cette structure d'arbre a été implantée par nos soins et se trouve dans le fichier `tree.ml`.

 - **Utilisation**

	Lorsqu'on change la formule d'une case, on commence vérifier que la nouvelle formule ne créera pas de dépendance cyclique. La méthode employée est détaillée dans `sheet.ml`. Si la formule crée une dépendance cyclique, on ignore la mise-à-jour. Sinon, on met à jour la formule de la cellule. Il faut alors remettre la valeur de la cellule à jour, ainsi que les valeurs des dépendances montantes. On met donc toutes les dépendances montantes à `None`, puis on calcule la nouvelle valeur de la cellule, puis les valeurs des dépendances montantes.

## FEUILLES DE CALCUL et INDICES

	Il est possible de changer de feuille de calcul courante avec la commande `SwitchTo s<i>`, avec i entre 1 et 10. Bien évidemment, chaque feuille est indépendantes des autres, si aucune fonction n'est utilisée (voir section suivante).
	L'intervalle possible des indices a été modifié : une feuille de calcul est maintenant de taille 100 × 100. On indexe ses indices de `A1` jusqu'à `D-100` __TODO : quelle est la valeur de '-' ?__ : _laissé en exercice au correcteur_.


## FONCTIONS

	Les fonctions ont été implémentées comme demandé... Ou on a au moins essayé. Ci-dessous, nous allons parler de la difficulté d'implémentation de ces fonctions.

	Dans la suite, je note en début de ligne et entre parenthèses le numéro de la feuille courante. Imaginons le cas suivant :

`(2) A3=A4`
`(2) A4=A1`
`(1) B1=s2(1.;1.)`	Ici, (1)B1 vaut 1, (2)A1 (2)A4 (2)A3 valent 1
`(2) A1=5.`         Cette ligne ne doit pas modifier (1)B1
`(2) A4=1000`		Ici B1, doit être modifié, et valoir 1000

	Ainsi, on comprend ici que on doit avoir une dépendance montante de (2)A4 vers B1. Il y a donc des dépendances entre les feuilles. C'est pourquoi on va actualiser notre champs `dependancies` pour le mettre de type `int * (int * int)`, dont la composante gauche correspond au numéro de la feuille de la cellule.

	Malgré tout, une question se pose. **Lors de l'opération `(1) B1=s2(1.;1.)`, qu'advient-il de `(2)A1,A2` ?** Doit-on mettre leur formule à `(2)A1.formula = Cst 1.` et `(2)A2.formula = Cst 1.` ? LA réponse est non, à cause de l'exemple suivant :

`(1) B1=s2(1.;1.)`
`(1) B2=s2(3.;1.)`

	Ici, il est clair que pour que les feuilles soient cohérentes, on a besoin que `(2)A1=1.` **et** `(2)A1=3.`, ce qui est bien évidemment impossible. Comme le sujet demande d'actualiser la feuille s2 lors d'un appel à la fonction s2, la stratégie suivante ne s'applique pas :

*Lors d'un appel à s2, copier les valeurs rencontrées au fur et à mesure, pour calculer la valeur demandée par l'appel de fonction dans A3, puis restaurer la feuille de calcul s2 dans l'état où elle était avant l'appel de fonction.*

	Ce genre de choses aurait permis, avec le `IFTE` introduit, de programmer la fonction qui calcule la factorielle. Il aurait été possible, de fait, d'introduire un semblant de langage de programmation dans le tableur (au détail près que le nommage des variables n'est pas possibles et que toutes nos fonctions n'auraient eu que deux arguments). Cette stratégie est envisageable, mais est extrèmement coûteuse en espace. Il se pourrait (à preuve près), que ce coût devienne raisonnable si on interdit strictement n'importe quel cycle sur les appels de fonctions *mais alors, on ne peut plus faire la factorielle*.

	**Notre choix**

	On décidé de taper dans les *valeurs* de A1 et A2 lors d'un appel de fonction, et non pas dans leurs formules. Cependant, il est clair qu'ensuite, **la feuille de calcul s2 devient eronnée**.

	*Une stratégie qui me semble appréciable mais qu'on n'a pas adoptée car elle ne respecte pas l'énoncé : remettre les valeurs des dépendances montantes de A1, A2, et des dépendances descendantes de A3 à __None__ après utilisation de la feuille comme d'une fonction. Ceci se rajouterait en très peu de lignes de codes au niveau de la fonction eval_form de sheet.ml dans le code.*



## FICHIERS DE TEST

	Nous avons mis à disposition nos fichiers de tests. Ceux-ci se trouvent tous dans `tests/`. Pour des notions de commodité, il y a dans le fichier principal un script qui exécute automatiquement tous les tests : il est à utiliser avec `./auto_tests`. Il n'y a pas besoin de compiler au préalable ; cette action est incluse dans le script. Ce dernier utilise les fichiers dans `solutions/` pour vérifier automatiquement les résultats des tests. Chaque test `tests/toto.txt` a son semblable de même nom `solutions/toto.txt` qui contient les résultats attendus du test. Si le fichier solution est absent, l'algorithme le signal.

#description brève de ces fichiers de test

Testing AVG.txt    : 	Teste AVERAGE
Testing colon.txt  :    Teste l'implémentation de ':'
Testing coord.txt  :    Teste l'extension de votre système de
					    coordonnées à plusieurs lettres
Testing cycle.txt  :    Vérifie que vous ignorez des commandes si
					    elles sont cycliques - niveau 1 !
Testing dep.txt    :    Test primaire de vos dépendances
Testing flot2.txt  :    Vérifie que vous avez amélioré le parsing
					    des flottants pour permettre plusieurs
                       	chiffres après la virgule
Testing flot-neg.txt :  Idem, vérifie que vous pouvez faire des
            	   	 	flottants négatifs
Testing fun-parse.txt :   Vérifie que vos fonctions passent au
						  parser
Testing fun-sum.txt :     Vérifie que vous gérer les dépendances
						  correctement lors des appels de fonction
Testing MAX.txt    :      Teste MAX
Testing MULT.txt   :      Teste MULT
Testing nombre.txt :      Vérifie que vous pouvez effectivement
						  parser de l'entier en INT et pas en FLOAT
Testing SUM.txt    :      Teste SUM
Testing switchTo.txt :    Vérifie le SwitchTo, et l'indépendance
					      entre vos feuilles
Testing t1.txt     :      un des tests vanilla
Testing t2.txt     :      idem
Testing t3.txt     :      cat tests/t3.txt pour comprendre ce que
						  c'est (j'ai oublié)
Testing update-ok.txt :   Vérifie que vous ignorez des commandes si
	  					  elles sont cycliques - niveau 2 !

Il y en a quelques autres qui sont relatives aux opérandes *extras*.

## OPÉRANDES UTILISABLES

	Voici la liste des opérandes utilisables dans les fichiers de test :

- `Show <cell>`      : affiche la cellule spécifiée
- `ShowAll`          : affiche toutes les cellules de la feuille
- `<cell>=<formula>` : affecte la formule spécifiée à la cellule
- `SwitchTo <sheet>` : change de feuille courante

- `MAX(...)`		 : calcule le max de ses arguments
- `SUM(...)`	     : calcule le min de ses arguments
- `MULT(...)`		 : calcule le produit de ses arguments
- `AVERAGE(...)`	 : calcule la moyenne de ses arguments

**EXTRAs**

- `MIN(...)`		 : calcule le minimum de ses arguments
- `DIV(...)`		 : DIV(x1;...;xn) calcule x1 / x2 / ... / xn
- `MOD(...)`		 : MOD(x1;...;xn) calcule x1 % x2 % ... % xn
- `MINUS(...)`		 : MINUS(x1;...;xn) calcule x1 - x2 - ... - xn
- `INV(<cell>)`	     : INV(x1) calcule 1 / x1 en flottant
- `OPPOSITE(<cell>)` : OPPOSITE(x1) calcule - x1
- `IF <form> THEN <form> ELSE <form>` : doit dans les faits s'écrire `IFTE(<form>;<form>;<form>)`
					   (IF f1 THEN f2 ELSE f3) renvoie f2 si l'évaluation de f1 est non nulle, et renvoie f3 sinon.

*Remarque : l'opération `:` qu'on a introduite liste les éléments strictement décroissants pour l'ordre lexicographique. Ainsi, il faut faire attention à l'utilisation pour des opération non commutatives ! (`MINUS`, `DIV`...)*


**C'est tout ! Bonne correction !**
