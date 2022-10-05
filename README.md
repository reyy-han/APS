# APS 2021/2022

**21107646** ELDAKAR Joumana	

**3800099** KAYA Delphine

Projet d’Analyse de Programmes et Sémantique
==========================================

## Implémentation :

Lors de la réalisation du projet, nous avons implémenté le code jusqu’à la version APS2. 
**APS0**, **APS1**, **APS1a** et **APS2** sont fonctionnelles.

## Fichier ZIP :

Le fichier zip du rendu contient :
-	Les 3 exécutables : `prologTerm`, `typrog.sh` et `exeprog.sh`
-	Un répertoire de tests **Samples/** que nous avons utilisé pour tester nos versions.
-	Le fichier README.md
-   Un script **all_test_typrog.sh** pour executer tous les tests contenus dans le répertoire **Samples/**
-   Un script **all_test_exeprog.sh**
-   les trois répertoires qui continennent le codes de nos algorithmesL

## Exécution :
Pour exécuter les executables : 
-	Pour le syntaxe, il suffit de lancer en ligne de commande dans un terminal une commande telle que : `./prologTerm Samples/[nom_de_fichier].aps`
-	Pour le typage, `./typrog.sh Samples/[nom_de_fichier].aps` ou `bash typrog.sh Samples/[nom_de_fichier].aps`
-	Pour la sémantique, `./exeprog.sh Samples/[nom_de_fichier].aps` ou `bash exeprog.sh Samples/[nom_de_fichier].aps`

Notes : Lors de l'exécution des fichiers tests, certains tests retournent une erreur `ko`, si le fichier provient du répertoire Samples/ alors la faute est attendue.
