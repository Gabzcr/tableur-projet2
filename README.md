(***********************************************************************)
(*                                                                     *)
(*                            **OCaml**                                *)
(*                                                                     *)
(*            Victor Boone - Gabrielle PAUVERT, projet Tableur         *)
(*  						   ENS LYON                                *)
(*                                                                     *)
(*  Copyright 2018 École Normale Supérieure de Lyon.  All rights       *)
(*  reserved.  This file is distributed under the terms of the GNU     *)
(*  Library General Public License, with the special exception on      *)
(*  linking described in file ../LICENSE.     						   *)
(*                                                                     *)
(***********************************************************************)


### tableur-projet2


PROJ2 : Conception d'un tableur minimaliste en OCAML.
**Niveau du rendu** : Avancé
**Binôme** : Victor BOONE - Gabrielle PAUVERT

## TYPES et CONVENTIONS

	Les nouveaux types sont listés dans `cell.ml`. 
	On considère que la valeur d'une cellule `c` est à jour si et seulement si `c.value != None`.


## GESTION DES DÉPENDANCES

	On gère les dépendances avec une structure arborescente. Chaque cellule wlog `A1` contient une formule qui mentionne possiblement d'autres cellules, wlog `A2`. Le problème est que si la valeur de `A2` se trouve être modifiée, il faut en conséquence modifier celle de `A1`. C'est pourquoi chaque cellule contient un champs `dependancies`. C'est un arbre-AVL qui contient toutes les cellules qui dépendent directement de `A1` (c'est-à-dire qui ont `A1` dans leur formule).
	On parle dans notre code de dépendances *montantes* pour les formules qui dépendent de la valeur de la cellule courante ; et on parle de dépendances *descendantes* les formules dont la valeur de la cellule courante dépend. Ainsi, l'arbre des dépendances contient les dépendances montantes directes.


 - **Pourquoi un arbre ?**

	Les dépendances se doivent d'être une structure sans doublon. Dès lors, si on utilise des listes chaînées pour stocker ces dépendances, on a un temps d'insertion d'une nouvelle dépendance en `O(n)`, et suppression d'une dépendance en `O(n)` aussi. On utilise alors un arbre-AVL pour assurer un temps `O(n log n)` sur ces deux opérations. Cette structure d'arbre a été implantée par nos soins et se trouve dans le fichier `tree.ml`.

 - **Utilisation** 

	Lorsqu'on change la formule d'une case, on commence vérifier que la nouvelle formule ne créera pas de dépendance cyclique. La méthode employée est détaillée dans `sheet.ml`. Si la formule crée une dépendance cyclique, on ignore la mise-à-jour. Sinon, on met à jour la formule de la cellule. Il faut alors remettre la valeur de la cellule à jour, ainsi que les valeurs des dépendances montantes. On met donc toutes les dépendances montantes à `None`, puis on calcule la nouvelle valeur de la cellule, puis les valeurs des dépendances montantes. 

## FEUILLES DE CALCUL et INDICES

	Il est possible de changer de feuille de calcul courante avec la commande `SwitchTo s<i>`, avec i entre 1 et 10. Bien évidemment, chaque feuille est indépendantes des autres, si aucune fonction n'est utilisée (voir section suivante).
	L'intervalle possible des indices a été modifié : une feuille de calcul est maintenant de taille 100 × 100. On indexe ses indices de `A1` jusqu'à `D-100` __TODO : quelle est la valeur de '-' ?__.


## FONCTIONS

	Les fonctions ont été implémentées comme demandé.


## FICHIERS DE TEST

	Nous avons mis à disposition nos fichiers de tests. Ceux-ci se trouvent tous dans `tests/`. Pour des notions de commodité, il y a dans le fichier principal un script qui exécute automatiquement tous les tests : il est à utiliser avec `./auto_tests`. Il n'y a pas besoin de compiler au préalable ; cette action est incluse dans le script. Ce dernier utilise les fichiers dans `solutions/` pour vérifier automatiquement les résultats des tests. Chaque test `tests/toto.txt` a son semblable de même nom `solutions/toto.txt` qui contient les résultats attendus du test. Si le fichier solution est absent, l'algorithme le signal.

## OPÉRANDES UTILISABLES

	Voici la liste des opérandes utilisables dans les fichiers de test :

- `Show <cell>`
- `ShowAll`
- `<cell>=<formula>`
- `SwitchTo <sheet>`

- `MAX(...)`
- `SUM(...)`
- `MULT(...)`
- `AVERAGE(...)`

**À rajouter :**
- `MIN(...)`
- `DIV(...)`
- `MOD(...)`
- `MINUS(...)`
- `INV(<cell>)`
- `OPPOSITE(<cell>)`
- `IF <form> THEN <form> ELSE <form>`




