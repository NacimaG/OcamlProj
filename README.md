# PROJET :: Compression des arbres binaire 
Master 1 Sciences et Technologie du Logiciel (Sorbonne Université )

UE d'Ouverture 2019/2020
Le projet consiste en la création d'une nouvelle structure de données qui compresse les arbres binaires dans le but d'obtenir des arbres plus optimaux qui occupe moins de place en mémoire.Le projet est entièrement codé en Ocaml. On a utilisé "Gnuplot" pour dessiner les graphes.

Réalisé par : 

	Hichem Rami AIT EL HARA 			
	GHOUT Nassima 
	AHMADI SIMAB Shokoufeh 

Responsables de l'UE :

	Antoine GENITRINI
	Emmannuel CHAILLOUX 

Le projet est composé en 5 fichier différents 

	--> Abr.ml :: concerne la partie de "ArbreBinaire de Recherche simple"
	--> Acl.ml :: Arbre binaire compressé avec des liste  (compression et recherche )
	--> Acm.ml :: Arbre binaire compressé avec des map (Compression et recherche )
	--> AcmlC :: Arbre binaire compressé utlisant des ( map transformé en abr acm )
	--> Exp.ml :: La partie étude expérimentale  
			=> on y retrouve les différents tests qu'on a effectué 
			   pour la compression et recherche 
			=> test de complexité en temps et mémoire 


Exécution

	$ ocamlc -o main Abr.ml Acl.ml Acm.ml AcmC.ml Exp.ml
	$ ./main
