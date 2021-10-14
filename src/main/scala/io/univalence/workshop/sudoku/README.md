# Programmation par contrainte pour résoudre des Sudoku

## Programmation par contrainte

* Ensemble de variables (les cases vides du Sudoku)
* Domaine de valeurs sur chaque variable (1, 2, 3, ..., 9 pour chaque case vide)
* Ensemble de contraintes sur les variables (toutes les cases d'une même ligne ont des valeurs différentes)

### Contrainte : X = Y
* Si X in [1..9] et Y in [1..9] => /
* Si X in [1, 2] et Y in [1..9] => Y in [1, 2]

### Contrainte : X = cst
* Si X in [1..9] et cst = 3 => X = 3

### Contrainte : X != Y
* Si X in [3] et Y in [1..9] => Y in [1, 2, 4..9]
* Si X in [3, 4] et Y in [1..9] => /

=> ALL_DIFF(X1, X2, X3, ...)

## Algorithme

X - != - Y - != - Z

def ac3(variables, domaines, contraintes)

* Chercher un élément dans une liste : O(N)
