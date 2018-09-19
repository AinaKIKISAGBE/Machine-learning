#############################################################
##### Auteur de ce fichier: Aina KIKI-SAGBE         #########
##### Livre: Analyse multivarié                     #########
##### Auteur du livre étudié: François HUSSON       #########
##### Edition du livre étudié: 2017                 #########
#############################################################



# importation
orange <- read.table("http://factominer.free.fr/bookV2/orange.csv",
                     header=TRUE,sep=";",dec=".",row.names=1)
dim(orange)# on observe le nombre de ligne et nombre de colonne
summary(orange) # on vérifie ceux qui sont quanti et quali 
# et on convertie si besoin en fonction de ce qu'on veu 
# définir comme quanti ou quali


# install.packages("FactoMineR")
library(FactoMineR)
###################
####### ACP #######
###################

res.pca <- PCA(orange,quanti.sup=8:14,quali.sup=15:16)# ???.sup pour indiquer les variables suplémentaires, donc qui de participent pas à la construction des axes factoriels
# il affiche par defaut deux graphiques:

# - le graph des variables actives et des variables suplémentaires avec le cercle de corrélation
x11()
plot(res.pca,choix="var")
# les variables suplémentaire sont coloré en bleu avec des trait en pointillé
# dans le premier plan factoriel, généré, par l'axe 1 et 2 
# (qui conservent 67.77+19.05=86.82% de l'inertie totale), 

# axe 1:
# le premier axe oppose les jus d'orange acide(Acidity), amer(Bitterness), gout intense(Intensity.of.taste), peu doux (sucré)
# aux jus d'orange pas très acide, légèrement amer et doux (Sweetness) 


# constat avec les var suplémentaires n'ayant pas contribué à la constiction des axes, le constat confirme l'analyse avec les variables réalisée actives ci-dessus
# le premier axe oppose les jus d'orange acide, amer et légèrement sucré: (criti.acid élevé indique jus acide, amer)
# aux jus d'orange pas très acide, légèrement amer et doux (sucré) : (ph élevé indique légère acidité, sacharose élevé indique bien sucré)

# axe 2: il opose les jus ayant de fortes odeurs au jus ayant des odeurs moins fortes
# la variables suplémentaire vitamine C n'est pas bien représenté car elle est loin des bords du cercle de corélation, donc elle ne servira pas d'interprétation pour auccune des axe, même pas pour l'axe 2.



# - le graph des individus complété des modalité des variables qualitatives (qui sont bien sur suplémentaire, et donc coloré en rouge)
x11()
plot(res.pca,choix="ind") # ou plot(res.pca)
# ou
plot.PCA(res.pca,choix="ind")# ou plot.PCA(res.pca)

# le premier axe opose les individus préféfant les produit frais et provenant de la floride 
# par raport aux individus préférant les produits ambiant et provenant d'autre régions

# on interprétera mieux les axes 1 et 2 en projetant les variables sur le graph des individus


# afficher uniquement le graph des individus et rendre invisible les variables qualitatives
plot(res.pca,invisible="quali")

# voir les stats sommaire de la construction de l'ACP
summary(res.pca)
# par défaut, il travaille jusqu'à 5 axes (dimensions)
# il affiche les stat des tableaux des valeurs propres (Eigenvalues) sur les 5 dimension, leur variance, leurs contribution à la variance et le cumulé des variance
# il affiche les stat des tableaux des individus sur les 5 dimensions : 
  # - la distance de chaque individus par rapport à l'individu moyen (barycentre),
  # - ses coordonnées sur chaques axes, 
  # - sa contribution à la formation de l'axe et
  # - son cosinus carré
# il affiche les stat des tableaux des variables sur les 5 dimensions : 
  # - leurs coordonnées sur chaques axes, 
  # - leurs contribution à la formation de l'axe et
  # - leurs cosinus carré


# il affiche les stat des tableaux des variables suplémentaire quantitatives continu s'il y en a sur les 5 dimensions : 
# - leurs coordonnées sur chaques axes, 
# - leurs cosinus carré

# il affiche les stat des tableaux des variables suplémentaire qualitative s'il y en a sur les 5 dimensions : 
  # ici les stats sont donné sur les modalité de ces variables qualitatives
    # - leurs distance de chaque individus par rapport à l'individu moyen (barycentre),
    # - leurs coordonnées sur chaques axes, 
    # - leurs cosinus carré
    # - ??? leurs v.test par raport au barycentre 


# corrélation entre les variables et les deux premiers axes factoriels
round(res.pca$var$coord[,1:2],2)

# Décomposition de la variabilité des valeurs propres par composante (axes) 
round(res.pca$eig,2)
# on peut prendre les 2 ou 3 premiers axes factoriels car leur cumulé donnent 86.81 et 98.53% de la variance total, elles sont suffisante pour bien interpréter  


# Distances entre les individus et le centre de la Nuage (l'individu moyen)
  round(res.pca$ind$dist,2)
# ce tableau permet de détecter des individus extrèmes afin de les enlever de l'ACP ou de les pettre en supplémentaire
  #   Un individu est considéré comme remarquable s'il a des extrêmes
  #   valeurs pour plusieurs variables. Dans le nuage NI, un tel individu est loin
  #   à partir du centre de gravité du nuage, et sa nature remarquable peut être évaluée
  #   de sa distance du centre du nuage dans l'espace global R^K.
  #   Dans notre exemple, aucun des jus d'orange n'est particulièrement extrême. 
  #   Les deux individus les plus extrêmes sont Tropicana frais et Pampryl
  #   ambiant. on poura donc décider de les enlever et de reprendre l'ACP ou de les mettre en suplémentaire ind.dup=data[,c(?,?,?,.etc..)]

  
# contribution des individus à la construction des composantes (axes)   
  round(res.pca$ind$contrib[,1:2],2)
# on remarque que nos deux individus extrèmes Tropicana frais et Pampryl ambiant contribut très fortement chacun pris isolement à la construction d'un des axes tandisqu'il contribut très très faiblement à la construction du second axe.
# Ainsi, l'inpacte des individus abérant influe beaucoup nos analyse, on poura donc les suprimer et reprendre l'ACP, en reprenant donc l'ACP, ne pas s'étonner de constater de légères modification dans l'interprétation des nouvelles sorties.  

      
# contribution des variables actives à la construction des composantes (axes)   
    round(res.pca$var$contrib[,1:2],2)
# petit constat, on remarque que la variable Odour.intensity contribut beaucoup à l'axe 2 et très peux à l'axe 1, on pourai etre tenté de l'enlever des actives et de le mettre en suplémentaire

    
# description du premier axe avec "dimdesc"
dimdesc(res.pca)    
 d<- dimdesc(res.pca)
 d$Dim.1$quanti
 d$Dim.1$quali
 d$Dim.1$category
# valeur arrondi à 2 chiffres décimal avec (round,2), 
 # le lapply permet juste d'apliquer la fonction (round,2) sur l'objet (dimdesc(res.pca))
lapply(dimdesc(res.pca),lapply,round,2)
  d_arrondi<-lapply(dimdesc(res.pca),lapply,round,2)
  
  # description des variables quantitative suplementaire sur premier axe (axe 1)
  d_arrondi$Dim.1$quanti
  d$Dim.1$quanti
  # on retient les variables ayant les corrélations 
  # les plus élevé en valeur absolue. dans notre exemple, 
  # on a (Odour.typicality, Sweetness, Bitterness et Acidity )

  # description des variables qualitatives suplémentaire et 
  # categoriel sur premier axe (axe 1)

    d_arrondi$Dim.1$quali
    d$Dim.1$quali
      # la p-value de "origin" est egal à 0.00941 <5%, donc 
      # la corrélation entre l'axe 1 et la variable "origin" est 
      # significativement différente de 0
    d_arrondi$Dim.1$category
    d$Dim.1$category
      # les stat de student des modalité "florida" et "other" 
      # sont <5% donc on peut tous deux les interpréter car elles 
      # sont toutes deux significatives 
          # les coordonnées des jus d'orange de Floride sont 
          # significativement plus élevés (signe positif du 
          # estimate: +2) que la moyenne sur le premier composant,   
          # tandis que les coordonnées des autres jus d'orange sont 
          # inférieures (signe négatif du estimat: -2) à la moyenne  
 
    
#################################
# gestion des valeurs manquantes
        
  library(missMDA)
# création de table avec donnée manquante
    donNA<-orange[1:7]
    donNA[1,1]<-NA
    nb_est <- estim_ncpPCA(donNA)
    nb_est
    
    # choisir le nombre de dimension (le numero du Minimum, dans l'objectif d'un ACP)
    nb<-nb_est$ncp
  Imputed <- imputePCA(donNA, ncp=nb) # imputation des valeurs manquantes
  donNA_no_na<-Imputed$completeObs # recuperation de la table imputée
  res.pca <- PCA(donNA_no_na) # utilisation de la table imputé , exemple de l'ACP 

################
  # exemple etude de cas : "decathlon"

  library(FactoMineR)
  # chargement de la donnée "decathlon" (par défaut dans un objet nommé "decathlon" si on ne le renomme pas)
  data(decathlon)

  # il faut normaliser cette table car les varriable quanti actives ne sont pas dans la meme unité de mesure et on des vatiances et moyennes trop distant les une des autres
  # par défaut, la fonction PCA normalise car l'option (scale.unit=TRUE) par defaut.
  res.pca <- PCA(decathlon,quanti.sup=11:12,quali.sup=13)
  # petite remarque: d'après le graph des individus, on remarque que Casara est un individu extrem sur les axes 1 et 2

  summary(res.pca, nb.dec=2, nbelements=Inf)
  # ou
  summary.PCA(res.pca)
  
  # choix du nombre d'axe à retenir 
    # diagramme des valeurs propres
  barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=paste("dim",1:nrow(res.pca$eig)))
# Les deux premiers axes expliquent 50% de l'inertie totale.
 #   si le cumul était très faible, on pourai relativiser et tenir
 #   compt aussi du nombre d'individu et de variable explicatives comme en ACM.
 #   mais comme ce cumulé est élevé, on peut ne pas  tenir compte 
 #   du nombre d'individus  actives et de variables actives. 
 #   Il peut être intéressant de comparer ce pourcentage avec le 
 #    0,95-quantile de la répartition des pourcentages 
 #    obtenus en simulant des tableaux de données de taille
 #    équivalente sur la base d'une distribution normale. 
 #    Selon le tableau A.3 en annexe , ce obtenu quantile 
 #    pour 40 personnes et 10 variables vaut 38%: même si 
 #    le pourcentage de 50% semble relativement faible, il 
 #    exprime une structure importante dans les données. 
 #    
 # Cependant, la variabilité des performances ne peut être
 # résumée par les deux premières dimensions seulement. 
 # Il peut également être intéressant d'interpréter les 
 # composants  3 et 4 pour lesquels l'inertie est supérieure à 1
 # (cette valeur "1" est utilisée comme référence, car elle 
 # représente, dans le cas des variables standardisées, 
 # la contribution d'une seule variable).


  # représentation sur les axes 3 et 4. 
    # on peut utiliser "plot" ou "plot.PCA"
  plot(res.pca,choix="ind",axes=3:4)
  
  # colorage des individus par couleur selons les modalité 
    #  de la variable qualitative de la colonne 13 avec l'option (habillage=13)
  # on peut changer le poids de la police d'affichage avec
    #  l'option (cex=0.7)
  plot(res.pca,choix="ind",habillage=13,cex=0.7)
  
  # Ellipses de confiance 
  # elles peuvent être tirées autour des catégories 
  # d'une variable supplémentaire (c'. -à-'est à dire autour du barycentre
  # des individus caractérisé par la catégorie). Ces ellipses 
  # nous permettent de visualiser ou non deux catégories sensiblement diffèrent. 
  plotellipses(res.pca,cex=0.8)
  
  
  #Le graphe des variables sur les composants 3 et 4 
  plot(res.pca,choix="var",axes=3:4)
  
  # description 
  # l'affichage donne pour de l'axe 1, 2 et 3 pour les variables quantitatives
  dimdesc(res.pca)

  # aucune description pour les variables qualitative ou catégirielles,  
  # on peut donc modifier Le niveau de confiance (proba = 0,2, 
  # au lieu de proba = 0,05 par défaut)
  dimdesc(res.pca,proba=0.2)
  # puis on interprète en comparant les p-value avec "0.2"

  # moyenne par variable  
  res.pca$call$centre
  # ecart type par variable
  res.pca$call$ecart.type
  
  # calcul des données normalisées
  round(scale(decathlon[,1:12]),2)
  
  
  
  # La matrice de corrélation
  round(cor(decathlon[,1:12]),2)
  
  # La matrice de covariance
  round(cov(decathlon[,1:12]),2)
  
  #  graph corrélations entre les variables
  pairs(decathlon[,c(1,2,6,10)])
  

#########################################
  # exemple: données temperature
  
  library(FactoMineR)
  temperature <- read.table("http://factominer.free.fr/bookV2/temperature.csv",
                            header=TRUE,sep=";",dec=".",row.names=1)
  
  res<-PCA(temperature,ind.sup=24:35,quanti.sup=13:16,quali.sup=17)
  # petit rapel sur la graph individu:
    # - point noir: individu actives 
    # - rond ou cercle Bleue avec libele Bleu: individu suplémentaire
    # - carré rouge avec libelé rouge: modalité des variable qualitative suplémentaires
  
  
  plot.PCA(res,choix="ind",habillage=17)
  summary(res)
  dimdesc(res)
  
  # calculer les données normalisées pour les variables quantitatives des individus actifs seulement
  scale(temperature[1:23,1:16])*sqrt(22/23)
  
  # calculer la matrice de corrélation
  cor(temperature[1:23,1:16])
  
  
  
  summary(res,nb.dec=2)
  scale(temperature[1:23,1:12])*sqrt(22/23)
  dimdesc(res)
  # voir les cooedonnées des variables quanti_sup sur les axes notemment sur l'axe 1
  res$quanti.sup$coord
 
  
  
  #########################################
  # exemple: données génomiques (poulet)
  # type de donné particulier: 43 poulet (individus) et 7407 variables 
  
  # importation / NB: cette base est très volumineuse, son importation prend du temps
  # chicken <- read.table("http://factominer.free.fr/bookV2/chicken.csv",header=TRUE,
  #                       sep=";",dec=".",row.names=1)
  chicken <- read.table("C:/Users/ASK32/Documents/m2/bibliotheque numerique Aina/clustering et classification/r/husson/chicken.csv",header=TRUE,
                        sep=";",dec=".",row.names=1)

  # transposé
  chicken <- as.data.frame(t(chicken))
  
  # fusion de la variable qualitative (catégorielle)
  diet <- as.factor(c(rep("N",6),rep("F16",5),rep("F16R5",8),rep("F16R16",9),
                      rep("F48",6),rep("F48R24",9)))

  chicken <- cbind.data.frame(diet,chicken)
  colnames(chicken)[1] <- "Diet"

  # ACP
  res.pca <- PCA(chicken,quali.sup=1)
  summary(res.pca)
  plot(res.pca,habillage=1,cex=0.7)
  # ici, comme nb_variable>nb_individus, alors on obtient 
  # (nb_individus-1=43-1=42 dimensions)
  # on interprète le graphique des nuages individus en notant 
  # leurs repartitions selon les modalités des variable qualitative suplementaire d'habillage'
  
  
  # les 2 premiers axes concervent 29% de l'inertie'
  res.pca$eig
  
  # le graph des variable est toufu et illisible 
  plot(res.pca,choix="var",invisible="var")
  points(res.pca$var$coord[,1:2], cex=0.5)
  
  # alors, il faut sortir les stats de tableaux descriptives 
  # dans le summary sur les variables et analyser les coordonnée 
  # des variables 
  res.pca$var$coord
  res.pca$var$cos2
  res.pca$var$contrib
  res.pca$var$cor
  
  res.pca$quali.sup
  res.pca$svd
  
  dimdesc(res.pca,proba=1e-5)
  plot(res.pca,habillage=1,axes=3:4)
  plot(res.pca,choix="var",invisible="var",axes=3:4)
  points(res.pca$var$coord[,3:4],cex=0.5)
  plotellipses(res.pca)
  plotellipses(res.pca, axes=3:4)


  ###################
  ####### AF (CA) ###
  ###################
  
  # NB:
  # Il faut noter que dans CA, contrairement à PCA, 
  # les éléments les plus extrêmes
  # ne sont pas nécessairement ceux qui ont le plus 
  # contribué à la construction de
  # les dimensions, car les poids diffèrent d'un 
  # élément à l'autre
  
  library(FactoMineR)
  work <- read.table("http://factominer.free.fr/bookV2/work_women.csv",
                     header=TRUE,row.names=1,sep=";")
  # ici, on importe un tableau de conteingence, 
  # le tableau est composé de deux variables qualitatives:

    # en lignes on a les modalités de la variable 1 : 
      # Dans une famille idéale: 
         # les deux parents travaillent à un rythme  égal,
         # le mari travaille plus, 
         # seulement le mari travaille
        
    # en colonne on utilisera les 3 modalités de la variable 2 :
      # L'activité la plus appropriée pour une mère
      # quand les enfants vont à l'école: 
          # Rester à la maison Travail 
          # travailler à temps partiel 
          # Travailler à temps plein
  
  
  
    
  # ainsi, nou allons prendre les Trois premières
  # colonnes car ce sont elles qui correspondent 
  # aux modalités de la variable 2. les colonnes allant
  # de 4 à 7 correspondent aux modalité d'une autre 
  # variable qualitative 3 : 
      # les femmes coupées du monde sont: 
        # en accord, 
        # peu en accord, 
        # peut en désaccord,
        # puis totalement en désaccord.
  
  # Et comme en AF (CA) on travaille seulement avec 
  # le tableau de conteingence de deux variables qualitatives, 
  # alors dans cet exemple, 
  # on a choisi la variable 2 (en colonne) 
  # pour acompagner  la variable 1 (lignes)
  
  
  # stat sommaire ; 
   # les variables sont qualitatives, mais les valeurs 
   # prises dans le tableau de contingence 
   # sont numériques (quantitatives) car ces valeurs 
   # correspondent au nombre d'individus ayant choisi 
   # tel modalité "i" et tel modalité "j" de nos 
   # deux variables. c'est donc normal que le sommaire donne 
   # des stat desc de variable quantitatives (min, mean, max, ...)
  summary(work)

  # Échantillons réels ou tableau de contingence initial importé 
  work[,1:3]
  
  # test de khi-2 d'indépendance entre les deux variable qualitatives
  res.test.chi2 <- chisq.test(work[,1:3])
  res.test.chi2
    # la p-value est inférieur à 5% donc 
    # l'hypothèse Ho selon laquel les deux variables 
    # sont indépendantes est rejeté, 
    # alors les deux variables sont liées (dépendants).
    # on peut donc continuer l'AF

  # échantillons théoriques ou valeurs espéré si les deux 
  # variables étaient indépendant
  
  round(res.test.chi2$expected,1)
  # interprétation: 
     # Dans notre echantillon réel, 13 femmes ont répondu 
     # que les deux parents travaillaient 
     # à un rythme  égale et que L'activité la plus appropriée 
     # pour une mère quand les enfants vont à l'école est de 
     # rester à la maison. 
     # Alors que d'après l'échantillon théorique, si le
     # deux les questions étaient indépendantes (pas liées), 
     # 43 personnes (en moyenne) auraient choisi
     # cette paire de réponses
            # ici, comme la valeur réel (13) est inférieur 
            #   à la valeur théorique (43), alors on dira que 
            #   les deux catégories (modalités) se repoussent, 
            #   c'est à dire que la modalité "travail égale" repousse 
            #   la modalité "mère reste à la maison", ce qui est normal 
            # et attendu, car elle se comprend facilement en disant que 
            # ( plus les deux parent travaillent à part égale, moins les 
            # mères restent à la maison), ce qui est logique. 
 
    #   Si la valeur réel était supétieur à la valeur 
    #   théorique, alors on dira les deux catégorie 
    #   (modalités) s'attirent
   
    #   Si les deux valeurs sont proches, l'interprétation 
    #   reste le même selon le sens de supériorité ou d'infériorité, 
    #   mais seulement que l'effet est faible (se repousse légerement,
    #   ou s'attirent légèrement)
  
  # Décomposition par cellule, ligne et colonne (valeurs brutes et Pourcentages)
  # contribution de chaque cellules à la déviation de l'indépendance
  round(res.test.chi2$residuals^2, 2)
  round(100*res.test.chi2$residuals^2/res.test.chi2$stat,2)
  # au sein des cellules (i,j) on remarque que le croisement 
  #  (les parent travaillent à part égale) et (la mère travaille
  #      à temps plein) exprime le plus grand ecart par rapport 
  #      à l'indépendance (car 30,04 est la valeur max des cellules (i,j)). 
  # en suite, lorsque nous prenons les sommes lignes (i,.) 
  # et les sommes colonnes (.,j), 
  # on remarque que la modalité "travail à temps partiel" est celle 
  # qui a la contribution la plus faible (1.98+2.56+0.25=4.78) 
  # à l'ecart de l'indépendance
  

    
  # profil ligne : somme des lignes i = 1,2,3,... pour chaque colonne j
  dd_ligne <- rbind(work[,1:3],apply(work[,1:3],2,sum))
  # profil moyen colonne : moyenne des lignes i = 1,2,3,...
  rownames(dd_ligne)[4] <- "Mean profile"
  dd_ligne<-round(prop.table(as.matrix(dd_ligne),margin=1),3)

 
 # profil colonne : somme des colonnes j = 1,2,3, pour chaque ligne i
  dd_col <- cbind(work[,1:3],apply(work[,1:3],1,sum))
  # profil moyen ligne : moyenne des colonnes i = 1,2,3,...
  colnames(dd_col)[4] <- "Mean profile"
  dd_col<-round(prop.table(as.matrix(dd_col),margin=2),3)


  
  # AF  (CA)
  res.ca <- CA(work[,1:3])
  plot(res.ca)
# le graph présente :
  # le profil ligne moyen en rouge représenté par des triangle (stay,part,full)
  # le profil colonne moyen en bleue représenté par des points (both,only,husband)  
  
 
  # interprétation:
  # La première dimension s'oppose aux catégories (modalités)
  # "la mère rester à la maison" et "la mère travail à temps plein".
  # Cette opposition sur le graphique représente 
  # une opposition en termes de  profil.
  # /:)les femmes qui ont répondu "rester à la maison" (colonne 1 ) ont 
  #  répondu sur la variable 1: 
  #   - "Seul le mari travaille le plus" 
  #     représente 84,9% > 52,7% (profil moyen colonne dd_col).
  # 
  #   - "Les deux parents travaillent à part egal" 
  #     représente 4,6% < 15,1% (profil moyen colonne dd_col).
  
  
  # /:) analogiquement, les femmes qui ont répondu au 
  #     "travail à temps plein" ont répondu inversement  
  #     aux les modalités de la variable 1
  
    
#   c'est donc normal que l'axe1 positionne séparément de 
# part et d'autre les modalité "rester à la maison" 
# et "travail à temps plein de la femme".    
# # Ainsi, l'axe 1 exprime la graduation des heures 
# de travailles de la femme.  

# l'analyse semblable mené sur l'axe 2 indique que 
# l'axe deux exprime la graduation de la relativité 
# du temps de travail du marie. on remarque que l'axe 1
# exprime egalement cette graduation. 


# association des modalités:
  # - les femmes qui "restent à la maison" préfère  que seul le mari travail
  # - les femmes qui travaillent à temps partiel préferent que leurs mari travaille encore plus
  # - les qui travaillent à temps plein préfèrent que les deux parents travaillent à part égale.
  
  # graph profil ligne 
  plot(res.ca,invisible="col")
  # graph profil colonne 
  plot(res.ca,invisible="row")
  
  
# Les graphiques représentant les barycentres exactes
  # ici, on interprète aussi les intensité d'association de modalité 
  
  plot(res.ca,invisible="col")
  # recupération des barycentre exacte des profil ligne
  coord.col = sweep(res.ca$col$coord,2,sqrt(res.ca$eig[,1]),FUN="*")
  points(coord.col,pch=17,col="red")
  text(coord.col,rownames(coord.col),col="red")

  
  plot(res.ca,invisible="row")
  # recupération des barycentre exacte des profiles colonne
  coord.row = sweep(res.ca$row$coord,2,sqrt(res.ca$eig[,1]),FUN="*")
  points(coord.row,pch=20,col="blue")
  text(coord.row,rownames(coord.row),col="blue")


# stat sommaire
  summary(res.ca)
  # Les principaux résultats : 
    # - inertie associée à chaque composant, 
    # - les coordonnées, 
    # - les contributions 
    # - et les qualités de représentation des lignes et des colonnes 
  
  
# graph des valeurs propres  
  barplot(res.ca$eig[,1],main="Eigenvalues",
          names.arg=1:nrow(res.ca$eig))
  # ici, on a juste 2 dimension car (min(N-1,K-1)=min(3-1,3-1)=2),
  # donc on a juste 
  # deux valeurs propres et donc juste 2 axes factoriels
  
# inertie ligne
  res.ca$row$inertia/res.ca$call$marge.row
# inertie colonne
  res.ca$col$inertia/res.ca$call$marge.col

# analyse factoriel AF (CA) avec elements suplémentaires (ilustratives)  
  res.ca2 <- CA(work,col.sup=4:ncol(work))
  x11()
  plot(res.ca2)

  # les variables suplementaires sont en couleur rouge foncé (des triangles)
  # et les variable actives sont en couleur rouge vif (des triangles)
  # la variable 1 (lignes) est toujours en bleue (des points)

  # on interprète les associations de modalité des variables.
   # exemple: 
     # - les femmes qui "restent à la maison" et donc ne 
     #    travaillent pas n'acceptent pas dutout " être coupé du reste du monde". 
     # - les femmes qui "travaillent à temps partiel"  
     #   acceptent peu "être coupé du reste du monde". 
     # - les femmes qui "travaillent à temps plein"  
     #   acceptent peu entierement (totalement) "être coupé du reste du monde". 
    

 
##### cas Très util de l' AF (CA) #######  
# text mining
# analyse de corpus de texte
  # on peut analyser un texte pour:
    # des sequences d'événement
    # confrontant les personnages et les actes, afin de 
    # suivre l'évolution  de ces personnages.
    # ... etc ........

  # traitement des données textuelles
  work # ??? il n'affiche pas la table voulue.
  # table contenant 3 colonnes dont 2 sont catégoriel quali 
  # et les troisème colonne est textuel 
  
  
  # constuit des tableaux de contingence à partir des données textuel
  textual(work,num.text=3,contingence.by=list(2,1:2),sep.word=", ",maj.in.min=TRUE)
  

    
#### exemple textmining : Jeux Olympiques 
  # importation directe du tableau de contingence
  # ici, on a beaucoup de colonnes en modalité ou individus (pays)
  library(FactoMineR)
  data(JO)
  
  # on va chercher à voir les lien entre événement et pays
  # quel pays reussi mieu à quel type d'évenemtn (100m, 200m, Javelo, poids, ...)
  res.ca <- CA(JO)
  
  summary(res.ca, nb.dec=2) # ou summary.CA(res.ca, nb.dec=2)
  # par defaut (nbelements=10) et affiche les 10 première lignes
  # de chaques tables. Pour tout afficher, on fait: ( nbelements=inf)
  # on interprète les contributions, .... 
  # on mène les interprétations réalisées plus haut dans l'exemple précédent "work"
  
  
  # voir le test de khi-2
  
  # valeurs propres
  barplot(res.ca$eig[,1],main="Eigenvalues",
          names.arg=paste("dim",1:nrow(res.ca$eig)))
  # ici, on a plusieurs dimensions. les deux premiers axes 
  # expliquent 24.4% de l'inertie total. 
  # ici dans ce type de CA, les valeurs propres varient de 0 à 1. 

  plot(res.ca)
  # puis on interprète comme on sit dèjà interprèter ce type de graph (voir le premier exemple de CA, "work")
  plot(res.ca$row)
  plot(res.ca$col)
  # on peut aussi chercher à observer les projections sur les axes 3 et 4  
  plot(res.ca,axes=3:4)
  # on sait déjà comment interpréter
  # Pour les événements discus et marteau, les pays d'Europe de l'Est tels que
  # La Lituanie, la Hongrie, la Slovénie et la Turquie ont également remporté le plus de médailles.
  # Les dimensions 3 et 4 séparent également le disque et le marteau du
  # des marches (20 km et 50 km). Le javelot est un événement très différent de
  # ceux du marteau et du disque. Pays d'Europe du Nord (Norvège,
  # La Tchécoslovaquie, la République tchèque, la Finlande et la Lettonie) obtiennent
  # événement de javelot
  
  
  
  # Les contributions  des pays qui ont le plus contribué à la construction de l'axe 1
  res.ca$col$contrib[rev(order(res.ca$col$contrib[,1])),1]
  # L'Éthiopie, le Kenya et le Maroc représentent 65% de la construction de
  # première dimension. Ces pays ont remporté un grand nombre de médailles lors de ces événements.
  # Les trois pays ont remporté un total de 60 médailles, dont 59 ont été obtenues
  # en événements d'endurance.
  
  # Les pays qui ont le plus contribué à la construction de l'axe 2
  res.ca$col$contrib[rev(order(res.ca$col$contrib[,2])),2]
  # La deuxième dimension sépare les événements de sprint du disque, le marteau,
  # et marche (20 km et 50 km). Encore une fois, ici, il y a un gradient entre
  # les épreuves de sprint: le 100 m est plus extrême que le 200 m et le 400 m.
     # Les Etats-Unis ont contribué de manière significative à la construction de cette axe  même si
     # La coordonnée est relativement proche de 0. Ceci est dû au grand nombre de médailles
     # remporté par les USA: un total de 82, dont 49 ont été remportés dans des épreuves de sprint
       # Les pays qui ont remporté des médailles dans les épreuves de sprint sont la Barbade,
       # Namibie, Trinité-et-Tobago, Jamaïque, République dominicaine, etc ...
  
  
  # Les marges de ligne et colonne
  res.ca$call$marge.row
  res.ca$call$marge.col[rev(order(res.ca$call$marge.col))]

  # le nombre de médailles obtenues par chaque pays, en multipliant la marge de la colonne par
  # nombre total de médailles, 360
  res.ca$call$marge.col[rev(order(res.ca$call$marge.col))]*360

  
  
######## exemple test mining CA avec la base des "Vin"

  library(FactoMineR)
  data.wine = read.table("http://factominer.free.fr/bookV2/wine.csv",
                         header=TRUE,row.names=1,sep=";",check.names=FALSE)
  res.ca=CA(data.wine,col.sup=11,row.sup=nrow(data.wine))
  barplot(res.ca$eig[,1],main="Eigenvalues",
          names.arg=1:nrow(res.ca$eig))
  summary(res.ca, nb.dec=2, ncp=2)

  
######## exemple test mining CA avec la base des "mortalité"
  library(FactoMineR)
  death <- read.table("http://factominer.free.fr/bookV2/death.csv",
                      header=TRUE,sep=";",row.names=1)
  colnames(death) <- c("0-1","1-4","5-14","15-24","25-34","35-44",
                       "45-54","55-64","65-74","75-84","85-94","95+")
# CA, ici, le graph est trop chargé, ilisible, 
  res.ca=CA(death,row.sup=66:nrow(death), graph=FALSE) 
  round(res.ca$call$marge.col,3)
  round(res.ca$call$marge.row[order(res.ca$call$marge.row)],3)

# diagramme barplot et interprétation des deux diagrammes  
  par(las=1)
  barplot(res.ca$call$marge.col,horiz=TRUE)
  barplot(res.ca$call$marge.row[order(res.ca$call$marge.row)],horiz=TRUE)
  par(las=0)
 # interprétation des deux diagrammes en barre :
  # La cause la plus fréquente de décès est liée aux maladies cérébrovasculaires.
  # Le groupe d'âge ayant le plus grand nombre de décès se situe entre 75 et
  # 84 ans. Les groupes d'âge supérieurs (85-94 ans et 95 ans et plus)
  # moins de décès simplement parce qu'il y a beaucoup moins de personnes de cet âge. Cela pourrait
  # noter également que le nombre de décès dans le groupe d'âge le plus bas (0-1 an)
  # est relativement élevé par rapport aux groupes d'âge suivants. Ceci est d'autant plus surprenant que ce groupe d'âge comprend des individus tous nés la même année,
  # tandis que les autres groupes comprennent des groupes de 4 et 10 ans.
  # Le pourcentage d'enfants de 0 à 1 an qui meurent est donc beaucoup plus élevé que
  # le pourcentage pour les enfants de 1 à 4 ans ou de 5 à 14 ans.
   
  
  
  
  res.ca=CA(death,row.sup=66:nrow(death))
  summary(res.ca, nb.dec=4)
  
  # diagramme des valeurs propres 
  barplot(res.ca$eig[,1],main="Eigenvalues",
          names.arg=1:nrow(res.ca$eig))
  # on retient 3 dimensions 
  
 # inertie totale décomposée par colonne en pourcentage
 100*res.ca$col$inertia/sum(res.ca$col$inertia)
  # on interprète en terme de contribution des modalités
 
 # inertie totale décomposée par ligne en pourcentage
 100*res.ca$row$inertia[rev(order(res.ca$row$inertia))]/sum(res.ca$row$inertia)
  # on interprète en terme de contribution des modalités
 
 # distance par rapport à l'origine et la inertie (brute, et en pourcentage) pour chaque colonne
  bb<-round(cbind.data.frame(res.ca$call$marge.col,
                             sqrt(res.ca$col$inertia/res.ca$call$marge.col),
                             res.ca$col$inertia,res.ca$col$inertia/sum(res.ca$col$inertia)),4)
  colnames(bb)<-c("Weight","Distance","Inertia","% of inertia")
  # Il semblerait donc que la forte contribution du groupe d'âge 15
  # à 24 ans provient principalement de la distance de l'origine, et donc
  # est un profil de mortalité très spécifique.

  # ?plot.CA()
  # graph variable ligne
  plot(res.ca,invisible=c("row","row.sup","col.sup"))
  # La première dimension sépare les nouveau-nés de 0 à 1 an de l'autre groupes d'âge 

  # graph variable colonne
  plot.CA(res.ca,invisible=c("col","row.sup","col.sup"))
  # La figure 2.20 attire l'attention sur les causes spécifiques de mortalité dans ce groupe d'âge,
  # c'est-à-dire les maladies infantiles qui affectent
  # groupe d'âge exclusivement ou quasi-exclusif (infection périnatale, SMSN, etc.).

  # contributions  colonnes pour l'axe 1
  round(res.ca$col$contrib[,1],3)
  # Les contributions confirment que le groupe
  # d'âge 0-1 ans contribue (presque)
  # entièrement à la première dimension, 
  # ainsi les causes de mortalité sont extrêmement spécifiques.
  
  
  
  # contributions  lignes pour l'axe 1
  res.ca$row$contrib[rev(order(res.ca$row$contrib[,1])),1]
  # La dimension met en évidence les causes spécifiques
  #   de la mort (presque par définition,
  #   comme on le voit dans les termes "périnatal" et 
  # "nourrisson" dans le groupe d'âge de 0 à 1 an.
  #   Ces contributions complètent le graphique en 
  # indiquant le rôle clé de l'infection.
  
  
  ###### on peut compléter aussi  analyse 
  ###### des axes 2,3 et 1,3 en plus de celui de 1,2

  
  # distances entre les groupes d'âge dans espace d'axe(1,2)
  res.ca<-CA(death,row.sup=c(66:nrow(death)),ncp=Inf)
  round(dist(res.ca$col$coord),3)

  # contributions aux constructions des axes 2 à 5
  round(cbind(res.ca$col$contrib[,2:5],res.ca$col$cos2[,2:5]),3)

  # les 5 contributions aux constructions des axes 2
  cbind(res.ca$row$contrib[,2],res.ca$row$cos2[,2],res.ca$call$marge.row)[rev(order(res.ca$row$contrib[,2])),]
  
  # les 5 contributions aux constructions des axes 3
  cbind(res.ca$row$contrib[,3],res.ca$row$cos2[,3],res.ca$call$marge.row)[rev(order(res.ca$row$contrib[,3])),]

  
# ajout des variable suplémentaire et interprétations:
  
  res.ca$row.sup$coord <- res.ca$row.sup$coord[130:157,]

  # évolution du nombre total de décès par année et par groupe d'âge
  plot.CA(res.ca,invisible=c("row","col"),axes=2:3)
  points(res.ca$row.sup$coord[,2:3],type="l")

  
#  Nous menons une autre AC avec seulement le nombre total de décès par
#  année entre 1976 et 2006 en tant que rangées supplémentaires. Nous construisons ensuite
#  un autre graphique sans les éléments actifs en utilisant l'argument
#  invisible = c ("ligne", "col"). Ainsi, nous visualisons le supplément
#  éléments seuls, puis connectez les points:
  
  tab.evol <- death[-(66:194),]
  res.evol <- CA(tab.evol,row.sup=66:nrow(tab.evol),graph=FALSE)
  plot.CA(res.evol,invisible=c("row","col"),axes=2:3)
  points(res.evol$row.sup$coord[,2:3],type="l")


  
  
  
#########################
####### ACM (MCA) #######
#########################   
  
#####  différence ente ACM ET AF
# ACM (MCA) : lignes_type= individus (personnes) et colonnes_type= variables qualitative (factor) /// tableau de valeur textuel et factor et binaire
# AF (AC) : lignes_type= colonnes_type= "modalité" des variables qualitatives (factor) /// tableau de numerique (tableau de contingence)  
    
  
  library(FactoMineR)
  tea <- read.table("http://factominer.free.fr/bookV2/tea.csv",header=T,sep=";")
  summary(tea)

  # MCA
  res.mca<-MCA(tea,quanti.sup=22,quali.sup=c(19:21,23:36))
  
  # graph des individus
  plot(res.mca,invisible=c("var","quali.sup"),cex=0.7)
  # le nuage d'individus est composé de nombreux points et
  # notre objectif est de voir si nous pouvons identifier une forme spécifique, ou des groupes de
  # personnes. Dans l'exemple, il n'y a pas de groupes notables d'individus: le
  # nuage de points est une forme plutôt cohérente.
  
  
  # graph des modalité des variavles actives
  plot(res.mca,invisible=c("ind","quali.sup"))
     # l'axe 1 oppose les modalité (catégories) << salon de thé, supermarché + magasin spécialisé,
     # sachet de thé + lâche, bar, restaurant, travailler >> avec les catégories << non.avec les amis,
     # pas.travail, pas.restaurant, pas.accueil >> . Cette première composante oppose donc << ceux qui boivent 
     # beaucoup le thé >> avec << ceux qui boivent du thé seulement de temps en temps>> . 
 
     # l'axe 2 distingue les catégories << magasin spécialisé, lâche, luxe, à moindre
     # mesure, vert et après le dîner >> , de toutes les autres catégories.
  
  
  # graph : superposition des individus et modalité variable actives 
  plot(res.mca,invisible="quali.sup")
  # voir les proximité entre les individus et variables. 
  #  ici, deux modalités sont proches si elles sont porté par le même individu,
  # ou aussi, deux individus sont proches (qui ont des profils similaires ) si ils sont 
  # capté par une même modalité ou groupe de modalité.

    
  # graph des modalité des variables actives et des variables suplémentaires
  plot(res.mca,invisible="ind")
  # observer les relation ou similarité ou association entre les modalités 
  
  
  # graph des modalité des variables suplémentaires
  plot(res.mca,invisible=c("ind","var"))
  # analyser le graph comme on sait le faire
  # les axes opposent telles modalités à telles modalités ...

  # valeurs propres
  summary(res.mca)
  lapply(dimdesc(res.mca),lapply,round,4)
  # ici, les inertie et valeurs propres sont plus faible que dans ACP 
  # car en ACM, on est en très grande dimension
    # en effet, C'est parce que, dans PCA, seul le linéaire
    # les relations sont étudiées: un seul composant devrait suffire à représenter
    # toutes les variables si elles sont fortement corrélées. En MCA, nous étudions
    # relations beaucoup plus générales et au moins min (Kj, Kl) - 1 dimensions sont
    # nécessaire pour représenter la relation entre deux variables
  
  # description automatique des dimention (axe 1) par les variables qualitatives et des modalités aussi
  lapply(dimdesc(res.mca),lapply,signif,3)
  # le premier composant est caractérisé par
    # variables lieu d'achat, salon de thé, et ainsi de suite. En outre, nous pouvons voir que
    # certaines variables supplémentaires sont également liées à cette composante (sexe et
    # la convivialité)
  # interprétation similaire à celui des modalités
  
  # tracé des élipse autour des barycentre de chaque catégories (modalité) de quelques variable qulitatives (4 var choisi ici) 
  plotellipses(res.mca,keepvar=c("restaurant","place.of.purchase",
                                 "relaxant","profession"))
  # Ces ellipses sont utilisés pour visualiser si deux catégories sont significativement
  # différent ou pas.
  
  # représentation des ellipses de confiance autour des catégories de le variable Restaurant 
  res.mca <- MCA(tea,quanti.sup=22,quali.sup=c(19:21,23:36),graph=FALSE)
  plotellipses(res.mca, keepvar=11, label="none")

  
  # description d'une variable catégorielle spécifique ainsi que de groupes d'individus
  # défini par les catégories de cette variable
  catdes(tea,num.var=18)

#  catdes(tea,num.var=18)

# gestion de Valeurs manquante ACM  
  library(missMDA)
  
tea_NA<-tea
tea_NA[1,1]<-NA 

# comme c'est pour faire de l'ACM, il faut que toute les variables
# soient qualitative, donc je soustrait la variable quantitative "age"
tea_NA$age<-NULL

  nb <- estim_ncpMCA(tea_NA) 
  # choix du nombre de composants
  nb_choix<-nb$ncp
  
  # imputation de la matrice de l'indicateur
  IndMat <- imputeMCA(tea_NA, ncp=nb_choix) 
  
  tea_NA_gere<-IndMat$completeObs
  # exécution de ACM
  res.pca <- MCA(tea_NA_gere, tab.disj=IndMat$tab.disj) 
  
  
  
####### exemple : base des OGM (organisme génétiquement modifié)
  
  gmo <- read.table("http://factominer.free.fr/bookV2/gmo.csv",
                    header=TRUE,sep=";",dec=".")
  summary(gmo[,1:16])

  # discrétisation de variables, regroupement de modalité et ou de variables
  levels(gmo$Position.Al.H)[4] <- levels(gmo$Position.Al.H)[1]

  levels(gmo$Position.Culture) <- c("Favourable","Somewhat Against",
                                    "Totally opposed","Favourable")
  # observation descriptive pour voir si il y a encore
  # des variables qu'on peu discrétiser recoder ou regrouper
  summary(gmo[,1:16])

  summary(gmo[,17:21],maxsum=Inf)


  # ACM
  res <- MCA(gmo,ncp=5,quali.sup=17:21,graph=FALSE)
  summary(res)
  res

  # discrétisation automatique (regroupement) automatiques des modalités 
  # juste avant de faire l' ACM avec la ventilation : level.ventil=0.05
  res <- MCA(gmo,ncp=5,quali.sup=17:21,graph=FALSE,level.ventil=0.05)
  # level.ventil désigne le seuil en dessous duquel une catégorie est ventilée.
  # Dans l'exemple, si une catégorie est sélectionnée par moins de 5% des individus,
  # ils sont répartis aléatoirement parmi les catégories existantes.

  
  # graph
  plot.MCA(res)
  
  # graph individus
  plot.MCA(res,invisible=c("var","quali.sup")) 
  # puis analyser avec les axes
  plot.MCA(res,invisible=c("var","quali.sup"),label="none") # ??? remarque: c'est label="none" et non pas label=FALSE
  # Le nuage d'individus sur le premier plan est
  # en forme de parabole: c'est ce qu'on appelle l'effet Guttman (ou l'effet "fer à cheval" )
  #  Cet effet illustre la redondance des variables actives

  # graph des modalité
  plot.MCA(res,invisible=c("ind","quali.sup")) 
  # puis analyser avec les axes
  plot.MCA(res,invisible=c("ind","quali.sup"),label="none") # idem 
  # même constat sur le graph des modalité, 
  # les graph des modalité et des individus doivent 
  # être analysé conjointement cote à cote
  # on divise l'écran d'affichage en 2 pour le faire
  

  # les graph des modalité suplémentaire
  plot.MCA(res,col.quali.sup="brown",invisible=c("quanti.sup","ind","var"))
  # puis analyser avec les axes
  
  
  ##### exemple de parfun 
  ### analyse sensoriel ou classification a l'aide 
  # des analyse d'ACM ou on essaie de voir des regroupe des parfuns(lignes=individus) selon leurs comportement sensoriel des testeurs (colonnes)
  perfume <- read.table("http://factominer.free.fr/bookV2/perfume.csv",
                        header=TRUE,sep=";",row.names=1)
  res.perfume <- MCA(perfume)
  summary(res.perfume)

  # graph individus
  plot.MCA(res.perfume,invisible="var",col.ind="black")

  # graph modalité 
  plot.MCA(res.perfume,invisible="ind",col.var="black")

  
  
  
  
  ##########################################
  ####### CLUSTERING                 #######
  ##########################################
  
  
  library(FactoMineR)
  temperature <- read.table("http://factominer.free.fr/bookV2/temperature.csv",
                            header=TRUE,sep=";",dec=".",row.names=1)
  res.pca <- PCA(temperature[1:23,],scale.unit=TRUE,ncp=Inf,
                 graph=FALSE,quanti.sup=13:16,quali.sup=17)

  # classification ascendant hiérarchique agglomérative
  # ?HCPC()
  res.hcpc <- HCPC(res.pca, nb.clust=-1)
  # ou
  res.hcpc <- HCPC(res.pca, nb.clust=-1,consol=TRUE, iter.max=10, min=3, 
  max=NULL, metric="euclidean", method="ward", order=TRUE,
  graph.scale="inertia", nb.par=5, graph=TRUE, proba=0.05, 
  cluster.CA="rows",kk=Inf,description=TRUE)
  # analyse du dendogramme
     # La forme du dendrogramme suggère de partitionner
     # les individus (les capitales) en trois groupes
  # Le niveau optimal de division calculé 
  # suggère également trois groupes (voir la barre noir qui coupe les classes)
  
  # si on ne précise pas "nb.clust=-1"
  # cliquer sur le graph au niveau des sous classes 
  # désirées pour continuer
  
  # il est possible de changer les branches de chaque noud dans l'arbre afin
  # pour organiser les individus selon le premier 
  # composant principal autant que possible. Ceci est 
  # fait en utilisant l'argument par défaut order = TRUE

   
  # Si nous voulons trier les individus selon un autre 
  # critère, les individus doivent d'abord être triés en 
  # fonction de ce critère dans l'ensemble de données 
  # avant de faire PCA, 
  # puis classer en utilisant l'argument order = FALSE 
  # dans HCPC

  
  plot(res.hcpc)
  
  # résultats du clustering hiérarchie agglomérative
    res.hcpc$call$t
    
    res.hcpc$call$t$tree
    plot(res.hcpc$call$t$tree)
    
    # nombre de cluster (classe) optimal
    res.hcpc$call$t$nb.clust
    
    
    # L'inertie intra-grappe
    res.hcpc$call$t$within
    # pour n = 1 cluster, à l'intérieur d'un cluster
    # l'inertie vaut 12, pour 2 clusters il vaut 5.237,
    # et ainsi de suite

    
    # L'augmentation de l'inertie entre les grappes lors du
    # passage de n grappes à n + 1
    res.hcpc$call$t$inert.gain
    
    # Le rapport entre deux inerties internes 
    # successives : inert.gain(n+1) / inert.gain(n) 
    res.hcpc$call$t$quot 
    
    # les individus triés en grappes et la distance 
    # entre chaque individu et le centre de sa classe
    res.hcpc$desc.ind$para
    # Oslo est la capitale qui représente le mieux
    # les villes du groupe 1, tandis que Berlin et Rome 
    # sont les modèles des groupes 2 et 3
    
    
    
    # les individus triés par groupe et la distance entre chaque 
    # individu et le plus proche centre du cluster
    res.hcpc$desc.ind$dist
    # Reykjavik est spécifique au cluster 1 car c'est la ville
    # le plus éloigné des centres des groupes 2 et 3, 
    # nous pouvons donc considérer que c'est le
    # plus spécifique au groupe 1. Paris et Athènes sont 
    # spécifiques aux groupes 2 et 3

    
  # description  
  res.hcpc$desc.var
  
  # description
  res.hcpc$desc.axe


### exemple: HCPC sur AF  
  ### appliquer l'HCPC sur les AF permettent 
  ## simplement de distinguer des classes des parfum (individus en lidnes)
    
### exemple Tea: HCPC sur les ACM
  ## ici, l'HCPC récupère les coordonnées sur les composantes 
  ## principales issues de l'ACM puis les utilises comme données 
  ## quantitaive ilustrant chaque individu, pour pouvoir faire 
  ## le clusterind  (classification)
  
  library(FactoMineR)
  tea <- read.table("http://factominer.free.fr/bookV2/tea.csv",header=T,sep=";")

  # ici, on selectionne les 20 premiers axes factoriel de l'ACM 
  # puisse que d'après l'ACM réalisé plus haut, les 20 premiers axes
  # expliquaient 87% de l'inertie total
  res.mca<-MCA(tea,ncp=20,quanti.sup=22,quali.sup=c(19:21,23:36),graph=F)
  res.hcpc <- HCPC(res.mca,nb.clust=-1)
  # le dendogramme propose 3 groupes
  
  plot(res.hcpc,choice="map",ind.names=FALSE)

  # para et dist : voir methode d'interpretation plus haut
  res.hcpc$desc.ind
  res.hcpc$desc.ind$para
  res.hcpc$desc.ind$dist
  
  # inertie sans partition
  round(res.hcpc$call$t$inert.gain,3)
  # inertie avec partition
  round(res.mca$eig[,1],3)
  # sur le plan généré par les deux premiers axes factoriels,
  # Les composantes principales expriment plus 
  # d'inertie (0.148 + 0.122 = 0.270) que
  # partitionnement en trois groupes 
  # (0,085 + 0,069 = 0,154). C'est un avantage
  # lorsque nous souhaitons résumer facilement l'information
  
  # Descriptions des classes
  res.hcpc$desc.var
  res.hcpc$desc.var$test.chi2
  #   La variable <<lieu d'achat >> et la variable << format >> 
  # des variables caractérisent le partitionnement
  # en trois groupes. Plus précisément, chacun des clusters 
  # est caractérisé par une catégorie ( modalité ) de
  #   La variable <<lieu d'achat >> et 
  #   une catégorie ( modalité ) la variable << format >>
  
  res.hcpc$desc.var$category$`1`
  # La première cluster est caractérisé par des individus 
  # qui achètent du thé dans les <<supermarchés=supermarket>> 
  # en <<sachets=sachet>>. 85,9% (Cla/Mod) des personnes qui achètent
  # du thé dans les <<supermarchés>> appartiennent au 
  # groupe 1 et 93,8% (Mod/Cla) les individus du groupe 1 achètent 
  # leur thé dans les <<supermarchés>>
  
  res.hcpc$desc.var$category$`2`
  # le cluster 2 est caractérisée par ceux qui achètent 
  # du thé en <<vrac=loose>> dans 
  # les <<magasins spécialisés=specialist.shop>>
  
  
  res.hcpc$desc.var$category$`3`
  # le cluster 3 est caractérisé par ceux qui achètent 
  # dans les deux types de magasin (supermarché et spécialiste = supermarket+specialist), 
  # et dans les deux formats (sachet et en vrac=sachet+loose)
  
  
  res.hcpc$desc.axe
  # La description des groupes à partir des composantes 
  # principales  montre que les individus du groupe 1 ont 
  # des coordonnées extrêmement faibles (v-test > 3 ou < 3)
  # sur les axes 1 et 2 (par rapport aux individus d'autres groupes). 
  # Les individus du groupe 2 ont de fortes coordonnées sur la composant 2 
  # et les individus du groupe 3 ont de fortes coordonnées sur la composant 1. 
  # Nous retenons ici la paires-de-classe-composant avec un test v supérieur à 3 
  # car les composants étaient utilisé pour construire les clusters (3 cluster).
  

### discrétisation de variable quantitative
  ## exemple: on va diviser et recoder l'age en 4 classes
  tea <- read.table("http://factominer.free.fr/bookV2/tea.csv",header=T,sep=";")
  nb.clusters <- 4 # nombre pris au hasard
  breaks <- quantile(tea[,22],seq(0,1,1/nb.clusters))
  Xqual <- cut(tea[,22],breaks,include.lowest=TRUE)
  summary(Xqual)

  # on peut aussi utiliser l'histogramme pour diviser les 
  # classes par tranche. 
  hist(tea$age,col="grey",main="Histogram of the variable age",
       freq=FALSE,xlab="age",nclass=15)
  # ici, l'histogramme nous propose 3 tranches 

  
  # on peut aussi utiliser la classification hiérarchique pour
  # déterminer les nombres ou tranches de classes 
  # pour recoder une variable (quantitative (HCPC) 
  # ou qualitative(MCA+HCPC).  
  # Les clusters peuvent ensuite être définis avec la hiérarchie
  # regroupement ou en utilisant par exemple, la méthode K-means.
  # (la méthode K-means converge très rapidement 
  # lorsqu'il est mis en ouvre sur une seule variable).
  vari <- tea[,22]
  vs<-as.data.frame(vari)
  # vs$ind<-seq(1:nrow(vs))
  vs$ind<-vs$vari
  res.hcpc <- HCPC(vs,iter.max=10,nb.clust=-1)
  # l'indication iter.max = 10 conduit aussi un algorithme K-means
  # on distingue ici 4 tranche pour recoder l'age
  # nous pouvons maintenant créer la variable age_recod
  res.hcpc$data.clust
  fd<-as.data.frame(res.hcpc$data.clust)
  # ??? contacter l'auteur Husson François, cette partie 
  # du code ci-dessous ne maeche pas
# { # max.cla <- unlist(by(res.hcpc$data.clust[,1],res.hcpc$data.clust[,2],max))
#   # breaks = c(min(vari), max.cla)
#   # age_recod <- cut(vari, breaks, include.lowest=TRUE)
# } 

  #  alternative personnelle
  age_recod<-fd$clust
  summary(age_recod)
  # rm(breaks)
  # ensuite, on peut choisir nous meme comment on décide de garder le recodage
  # par exemple, dans le recodage ci-dessus, on a l'age recodé par 
  # tranche qualitative et non par tranche quantitative
  # cela fait qu'on a des ages isolé dans des classe et d'autres 
  # qui n'appartiennent pas à la classe alors que les tranche autours
  # appartiennent à la classe. 
  t_0<-as.data.frame(table(fd$clust,fd$vari))
  library(dplyr)
  # t_0$Var1<-as.numeric(t_0$Var1)
  t_1<- 
    t_0 %>% 
    filter(as.numeric(t_0$Var1)==1)
  t_1<- 
    t_1 %>% 
    arrange(Var2)
  
  t_1$cumul<-t_1[1,3]
  t_1$cumul<-as.numeric(t_1$cumul)

  for (i in 2:nrow(t_1)) {
    j<-i-1
    d<-t_1[i,3]+t_1[j,4]
    t_1[i,4]<-d
  }
  
  plot(t_1$cumul)
  # à partir de ce graph donc, on peut choisir ou scinder 
  # en suivant les evolution et les pauses du graph, 
  # par exemple, s'arreter avant la grande pause ou 
  # s'arreter lorsque l'accéleration de la croissance diminue.
  # Puis les tranches de valeurs non choisi serons étudier dans 
  # d'autres classes afin d'essayer de les classer.
  # ... etc... pour les autres classes
  
  
  # pour la méthode du prof, il est très util quand on a 
  # plusieurs variables quantitatives à recoder. 
  # pour exécuter le code sur toutes les variables quantitatives
  # d'une table afin de les recoder automatiquement, on fait :
  
  # on récupère d'abors toutes les variables quantitatives 
  # uniquement dans un objet. ??? corriger le code similaire plus haut et ceci-si dessous pourra etre corrigé aussi 
# { donnee_quanti_a_recoder_en_quali <- donnee[,c("liste des variables quantitative qu'on veut recoder en qualitative")]
#   don_recode<-donnee_quanti_a_recoder_en_quali
#   for (i in 1:ncol(don_recode)){
#   vari <- don_recode[,i]
#   res.hcpc <- HCPC(vari,nb.clust=-1,graph=FALSE)
#   maxi <- unlist(by(res.hcpc$data.clust[,1],res.hcpc$data.clust[,2],max))
#   age_recod <- cut(vari,breaks = c(min(vari),maxi),include.lowest=TRUE)
#   don_recode[,i] <- age_recod
#   }
# }


  
  
  #############################
  ####### visualisation #######
  #############################
  
# visualisation des graphiques en ACP  
  library(FactoMineR)
  data(decathlon)
  res.pca <- PCA(decathlon, quanti.sup=11:12, quali.sup=13, ind.sup=c(36:41),
                 graph=FALSE)
  # voir tous les graphiques de l'objet contenant l'ACP
  plot.PCA(res.pca) 

  # faire des filtres à afficher
  plot.PCA(res.pca, invisible=c("ind.sup","quali"))
  plot.PCA(res.pca, invisible="quali")

  # représenter les 10 individus qui sont le 
  # mieux représentés sur le plan choisi, 
  # << plan(axe1,axe2) par defaut >>. 
  # ou ceux (les 10 premiers) qui ont le plus 
  # contribué à  la construction du plan
  # représentation les 10 individus actifs
  # et les 10 individus illustratifs qui sont les mieux représentés
  # sur le plan créé par les dimensions 1 et 2
  plot.PCA(res.pca, select="cos2 10", invisible="quali")

  # représentation des individus  sur un seuil donné (0.5) 
  # que nous considérons acceptable en termes de 
  # représentation qualité 
  # représentation des individus pour lesquels la qualité
  # de représentation sur le plan est supérieur à 0.5
  plot.PCA(res.pca, select="cos2 0.5", invisible="quali")
  
  # représenter une sélection d'individus à partir de 
  # leur identité
  plot.PCA(res.pca, select=c("Casarsa","Clay","Sebrle"), invisible="quali")

  
  # on peut changer l'affichage des element non sélectionné
  # les non sélectionné en couleur gris par défaut
  plot.PCA(res.pca,select="cos2 10", invisible=c("ind.sup","quali"))
  # les non sélectionné en couleur noir
  plot.PCA(res.pca,select="cos2 10", invis=c("ind.sup","quali"), unselect=0)
  # les non sélectionné en couleur transparant (invisible complet)
  plot.PCA(res.pca,select="cos2 10", invis=c("ind.sup","quali"), unselect=1)
  # les non sélectionné en couleur gris un peut foncé. ici, on a préciser le degré de gris nous même
  plot.PCA(res.pca,select="cos2 10", invis=c("ind.sup","quali"), unselect=0.5)
  # les non sélectionné en couleur jaune
  plot.PCA(res.pca,select="cos2 10", invis=c("ind.sup","quali"),
           unselect="yellow")

  # graph variable uniquement
  plot.PCA(res.pca, choix="var")
  # quelque modifications
  plot.PCA(res.pca, choix="var", invisible="quanti.sup")
  plot.PCA(res.pca, choix="var", select="cos2 5")
  
  # choix de certaines variables à représenter
  plot.PCA(res.pca, choix="var", select=c("Long.jump","Rank","400m","100m"))
  plot.PCA(res.pca, choix="var", invisible="quanti.sup",
           select=c("Long.jump","400m","100m"), unselect=0)
  
  # choix de couleur pour les individu, individu_suplémentaire, var_quali, ...
  plot.PCA(res.pca,col.ind="yellow", col.ind.sup="grey60", col.quali="orange")

  plot.PCA(res.pca, choix="var", col.var="orange", col.quanti.sup="grey")
  
  # colorer les individus selon les modalité d'une variable qualitative
  # exemple : colorage selon variable qualitative en colonne 13
  plot.PCA(res.pca, habillage=13)
  # exemple : colorage selon variable qualitative nommé <<competition>>
  plot.PCA(res.pca, habillage="Competition")

  # afficher les modalité d'une variable qualitative (factor)
  levels(decathlon[,13])

  
  plot.PCA(res.pca, invisible=c("ind.sup","quali"), habillage=13,
           col.hab=c("orange","grey"))
           
  # voir la variabilité des individus selon les modalité 
  # de la variable qualitative situé en colonne 13 . 
  # on affiche les ellipse autour des barycentre 
  # des individus selon chaque modalité de la varible qualitative
             plotellipses(res.pca, keepvar=13)
           
             plotellipses(res.pca, keepvar=13, select="cos2 0.6")
  
 ##########            
 # visualisation des graphiques en AF 
 ##########         
   library(FactoMineR)
             data(children)
             res.ca <- CA(children, row.sup = 15:18, col.sup = 6:8, graph=FALSE)
             plot.CA(res.ca)
             plot.CA(res.ca, invisible=c("col","row.sup","col.sup"))
             plot.CA(res.ca, invisible=c("row.sup","col.sup"))
           
             plot.CA(res.ca, selectRow="cos2 3", selectCol="cos2 2")
             plot.CA(res.ca, selectRow="cos2 0.8", selectCol="cos2 0.8")
             plot.CA(res.ca, selectRow=c("work","hard"), selectCol=c("cep","university"))
           
             plot.CA(res.ca, selectCol="cos2 5", selectRow="cos2 5", unselect=0)
             plot.CA(res.ca, selectCol="cos2 5", selectRow="cos2 5", unselect=1)
             plot.CA(res.ca, selectCol="cos2 5", selectRow="cos2 5", unselect="yellow")
           
             # ellipse de confiance autour de chaque modalité de toutes les variables
             ellipseCA(res.ca)
           
             
             # Pour représenter juste un sous-ensemble d'ellipses, l'argument ellipse peut être modifié
             # pour sélectionner uniquement les ellipses autour des lignes ou autour des colonnes. 
             # nous pouvons  utilisez également les arguments col.row.ell ou col.col.ell pour choisir la couleur.
             # on peut mettre "transparent" pour qu'aucune ellipse ne s'affiche

             ellipseCA(res.ca, ellipse="row", invisible=c("row.sup","col.sup"),
                       col.row.ell=c(rep("grey",2),rep("transparent",12)))
           
             
 ########
 # visualisation des graphiques en ACM
 ########
             plot.MCA(res.mca, invisible=c("var","quali.sup"))
             plot.MCA(res.mca, invisible=c("var","quali.sup"), label="none")
             plot.MCA(res.mca, invisible=c("var","quali.sup"), label="ind.sup")
           
             plot.MCA(res.mca, invisible=c("var","quali.sup"), select="cos2 50")
             plot.MCA(res.mca, invisible=c("var","quali.sup"), select="cos2 0.5")
           
             plot.MCA(res.mca, invisible=c("var","quali.sup"), label="none",
                      select="cos2 0.5", unselect=0.9)
           
             plot.MCA(res.mca, invisible=c("ind","ind.sup","quali.sup"))
             plot.MCA(res.mca, invisible=c("ind","ind.sup","var"))
           
             # les 10 catégories actives et les 10 valeurs illustratives lis mieux représentées
             plot.MCA(res.mca, invisible=c("ind","ind.sup"), selectMod="cos2 10",
                      unselect=0.9)
             
             # les 15 catégories qui ont contribué à la construction du plan ( donc seul les categories actives sont représentées)
             plot.MCA(res.mca, invisible=c("ind","ind.sup"), selectMod="contrib 15",
                      unselect=0.9)
           
           
             plot.MCA(res.mca, choix="quanti.sup", title="Quantitative variables graph")
             plot.MCA(res.mca, choix="var", title="Graph of the variables")
           
             plot.MCA(res.mca, choix="var", invisible=c("quali.sup","quanti"))
           
             plot.MCA(res.mca, col.var="black", col.ind="yellow", col.quali.sup="orange")
           
           
             plotellipses(res.mca) # elipse sur toutes les variables
             plotellipses(res.mca, keepvar="sport") # ellipse sur la variable <<sport>>
           
             
 ############             
 ### visualisation avec Factoshiny          
 ############
             
  # installer d'abord le package "FactoInvestigate"
    # avant d'installer le package "Factoshiny"
    library(FactoMineR)
      #     install.packages("FactoInvestigate")
           library(FactoInvestigate)
      #     install.packages("Factoshiny")
           library(Factoshiny)
           
             data(decathlon)
             res.shiny <- PCAshiny(decathlon) # PCA
             # NB: ne pas appuyer sur le paneau <<stop>> dans <<R>> pour quiter 
             # le navigateur de l'application, sinon, l'objet 
             # contenant les résultat << res.shiny >> ne sera pas créé. Donc il faut cliquer 
             # sur << quitter l'application >> dans le navigateur, ainsi l'objet est créé normalement dans <<R>>.
             # et ensuite on peut fermer le fenetre web dans notre navigateur
             
             res.pca <- PCA(decathlon, quanti.sup=11:12, quali.sup=13, ind.sup=c(36:41),
                            graph=FALSE)
             res.shiny <- PCAshiny(res.pca)
             res.shiny2 <- PCAshiny(res.shiny)
  
#######                      
#   install.packages("factoextra")
#######
    # ce package offre des méthodes de graphique du ggplot
    # avec des graphs plus élaboré et posibilité d'ajout de couche de graph
             library(factoextra)
             data(decathlon)
             res.pca <- PCA(decathlon, quanti.sup=11:12, quali.sup=13, ind.sup=c(36:41),
                            graph=FALSE)
            # graph individu ACP
             fviz_pca_ind(res.pca)
           
             library(factoextra)
             res.pca <- PCA(decathlon, quanti.sup=11:12, quali.sup=13, ind.sup=c(36:41),
                            graph=FALSE)
             # ajout du titre au graph
             fviz_pca_ind(res.pca) + labs(title="titre du graph PCA", x="PC1", y="PC2")
           
             # graph des individus en fonction de leurs contributions
             fviz_pca_ind(res.pca, col.ind="contrib")
             # changer la couleur du gradiant
             fviz_pca_ind(res.pca, col.ind="contrib") + scale_color_gradient2(low="blue",
                      mid="white",high="red", midpoint=4)
             # désencombrer l'arrière-plan du graphique
             fviz_pca_ind(res.pca, col.ind="contrib") + scale_color_gradient2(low="blue",
                      mid="white", high="red", midpoint=4) + theme_minimal()
           
           
           
             library(FactoMineR)
             data(decathlon)
             res.pca <- PCA(decathlon, quanti.sup=11:12, quali.sup=13, ind.sup=c(36:41),
                            graph=FALSE)
             
             
             plot.PCA(res.pca, select="cos2 0.6", unselect=1, habillage="Competition",
                      title="Graphe des individus", autoLab="yes", shadow=TRUE,
                      cex=0.8, cex.main=1.2, cex.axis=0.8, cex.lab=0.8)
             # <<autoLab = "yes">>  pour créer le moins de chevauchement possible entre les étiquettes sur le graph
             # <<autoLab = "no">>  pour le déselectionner
             # <<autoLab = "auto">> valeur par défaut:  lorsqu'il y a moins de 50 objets il applique <<autoLab="yes">>
             #                      sinon si il y a plus de 50 objet, il applique autoLab = "no"
             # <<shadow = TRUE>> est utilisé pour ajouter une légère ombre sous les étiquettes pour améliorer leur lisibilité sur les axes
             #
             #  <<cex>> : changer la taille des fontes des points 
             #  <<cex.main>> : changer la taille du titre  
             #  <<cex.axis>> : changer la taille du titre des axes  
             #  <<cex.lab>> : changer la taille des étiquettes 
    
                      
####            
# exportation des sortu en csv 
####
             library(FactoMineR)
             temperature <- read.table("http://factominer.free.fr/bookV2/temperature.csv",
                                       header=TRUE,sep=";",dec=".",row.names=1)
             res <- PCA(temperature,ind.sup=24:35,quanti.sup=13:16,quali.sup=17)
             plot.PCA(res,choix="ind",habillage=17,cex=0.7,title="My PCA")
             plot.PCA(res, choix="var", select=c("June","Annual"))
             summary.PCA(res, nb.dec=2, nbelements=Inf)
             
             # exportation de toutes les stats desc contanu dan l'objet le l'ACP dans un fichier .CSV
             write.infile(res,file="C:/Users/ASK32/Documents/m2/bibliotheque numerique Aina/clustering et classification/r/husson/file_ACP_output.csv",sep=";")
           
           
             res<-PCA(temperature[,1:12])
             res<-PCA(temperature[c(1:10,15:20),1:12])
             res<-PCA(temperature[-c(4:6,8,10),1:12])
           
             # library(Rcmdr)
             # library(FactoMineR)
