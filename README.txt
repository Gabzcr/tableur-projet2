
pour compiler, lancer
make
dans un terminal (il faut pour cela avoir fait des "cd" pour aller
dans le répertoire où se trouvent ces fichiers).

pour executer le programme, lancer (par exemple -- regardez le contenu du fichier examples.txt)
./main.native < tests/t1.txt



--- Petit guide au code ---

Si vous êtes débutant (ou intermédiaire), vous n'avez pas, dans un
premier temps, à regarder les fichiers lexer.mll et parser.mly.
Lisez en revanche les autres fichiers, en vous servant des indications
suivantes.

cell.ml : le fichier fixant la représentation d'une cellule.

debug.ml : petites fonctions d'affichage pour debugger

sheet.ml : le fichier fixant la représentation d'une feuille de
calcul, que l'on appellera tableau.
Ici se trouve le "coeur" du programme, dans la mesure où l'on trouve,
outre des fonctions pour manipuler le tableau, les fonctions pour
recalculer le tableau lorsqu'une cellule est mise à jour. C'est là que
vous devrez coder dans un premier temps.

command.ml : le fichier définissant un petit langage pour interagir
avec le tableau. le type "comm" correspond à ce qui peut être saisi
par l'utilisateur (cf. examples.txt dans le repertoire tests/).

main.ml : le fichier principal. Une partie de ce fichier consiste en
des incantation que vous n'avez pas trop à comprendre, l'essentiel est
de voir qu'on y appelle les fonctions définies dans les autres
fichiers.


Vous remarquerez que l'énumération ci-dessus suit l'architecture du
programme : on fait "open Cell" dans sheet.ml car on a besoin de faire
référence à ce qui est défini dans cell.ml (la convention impose de
majusculer la première lettre), on fait similairement "open Cell" et
"open Sheet" dans command.ml, etc. 


--- Comment coder ---

Éditez les fichiers .ml, et recompilez régulièrement pour vérifier que
vos modifications sont cohérentes. Faites aussi régulièrement des
tests (notez le pluriel !) afin de voir que le programme que vous
écrivez est correct.
