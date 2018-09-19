#############################################################
##### Auteur de ce fichier: Aina KIKI-SAGBE         #########
##### Livre: Analyse multivarié : Practical Guide   #########
#####        to Cluster Analysis in R               #########
#####        Unsupervised Machine Learning          #########
##### Auteur du livre étudié: Alboukadel Kassambara #########
##### Edition du livre étudié: 2017                 #########
#############################################################








# (www.genomicscape.com
#   http://www.sthda.com/english/rpkgs/factoextra
#   (http://www.sthda.com
#     <alboukadel.kassambara@gmail.com
#     www.sthda.com/english
#     http://www.sthda.com/english/wiki/r-packages # apprendre plus
#     https://goo.gl/vJ0OYb # joli graph
#     https://goo.gl/v5gwl0 # 3D graph
    
    
    
install.packages("cluster")
install.packages("devtools")
devtools::install_github("kassambara/factoextra")
# ou
# 
install.packages(c("cluster", "factoextra") 
                 
                 library("cluster")
                 
                 
                 
                 
                 
                 # importation de données 
                 # .txt file: Read tab separated values
                 my_data <- read.delim(file.choose())
                 # .csv file: Read comma (",") separated values
                 my_data <- read.csv(file.choose())
                 # .csv file: Read semicolon (";") separated values
                 my_data <- read.csv2(file.choose())
                 
                 data("USArrests") # Loading
                 head(USArrests, 3) # Print the first 3 rows
                 
                 # Access the data in Murder
                 column
                 # dollar sign is used
                 head(USArrests$Murder)
                 
                 
                 # Or use this
                 USArrests[, 'Murder']
                 
  ########## 2
                 data("USArrests") # Load the data set
                 df <- USArrests # Use df as shorter name
                 
                 # suppression des valeurs manquantes 
                 df <- na.omit(df)
                 
                 # standarisation des valeurs manquantes
                 df.scaled <- scale(df)
                 head(df, n = 3)
                 
      
                 ## documentation sur les function et packages utilisé
                 # http://www.sthda.com/english/rpkgs/factoextra.
                 # Functions                   : Description
                 # dist(fviz_dist, get_dist)   : Calcul de la matrice de distance et visualisation
                 # get_clust_tendency          : Évaluer la tendance au regroupement
                 # fviz_nbclust(fviz_gap_stat) : Déterminer le nombre optimal de grappes
                 # fviz_dend                   : Visualisation améliorée du dendrogramme
                 # fviz_cluster                : Visualiser les résultats du clustering
                 # fviz_mclust                 : Visualiser les résultats de clustering basés sur un modèle
                 # fviz_silhouette             : Visualiser les informations de silhouette à partir du clustering
                 # hcut                        : Calcule le clustering hiérarchique et coupe l'arbre
                 # hkmeans                     : Cluster du k-means hiérarchique
                 # eclust                      : Amélioration visuelle de l'analyse de clustering                 
                 
  ########## 3
                # I Méthodes de mesure des distances
                #     distance euclidienne 
                #     distance de Manhattan
                #     distances basées sur la corrélation : beaucoup plus utilisé dans la génétique (etude des genes et patogènes), on peut utiliser différents types de corrélation :
                #        - la distance de corrélation de Pearson: elle mesure le degré d'une relation linéaire entre deux profils.
                #        - le cosinus de la distance de corrélation d'Eisen : C'est un cas particulier de la corrélation de Pearson avec ¯x et ¯y tous deux remplacés par zéro
                #        - la distance de corrélation de Spearman : elle calcule la corrélation entre le rang de x et le rang des variables y                
                #        - la Distance de corrélation Kendall : elle  mesure la correspondance entre le classement de x et  des variables y . 
                #                Le nombre total d'appariements possibles de x avec y observations est
                #                n (n - 1) / 2, où n est la taille de x et y. Commencons par ordonner les paires par les valeurs x.
                #                Si x et y sont corrélés, ils auront les mêmes ordres de rang relatif. À présent,
                #                pour chaque yi, compter le nombre de yj> yi (paires concordantes (c)) et le nombre de
                #                yj <yi (paires discordantes (d)).
                # 
                #  II Comment choisir le type de distance :
                #     Selon le type de données et les questions du chercheur, d'autres différences mesures de distance pourraient être préférées. 
                #         Par exemple, la distance basée sur la corrélation est souvent utilisée dans l'analyse des données d'expression génique.
                #         La distance basée sur la corrélation considère que deux objets sont similaires si leurs caractéristiques sont
                #   fortement corrélé, même si les valeurs observées sont peut-être très éloignées en termes de
                #   Distance euclidienne. La distance entre deux objets est 0 quand ils sont parfaitement
                #   corrélé. 
                #         La corrélation de Pearson est assez sensible aux valeurs aberrantes. Cela n'a pas d'importance
                #   lors du regroupement d'échantillons, car la corrélation est sur des milliers de gènes. Quand
                #   aux gènes en cluster, il est important d'être conscient de l'impact possible des valeurs aberrantes. 
                #         Cela peut être atténué en utilisant la corrélation de Spearman au lieu de la corrélation de Pearson.
                #   Si nous voulons identifier des groupes d'observations avec les mêmes profils globaux indépendamment
                #   de leurs grandeurs, alors nous devrions aller avec la distance basée sur la corrélation comme une mesure de dissemblance.
                #        Ceci est particulièrement le cas dans l'analyse des données d'expression génique, où nous
                #   pourrait vouloir considérer des gènes semblables quand ils sont «en haut» et «en bas» ensemble. C'est
                #   aussi le cas, dans le marketing si nous voulons identifier le groupe de clients avec le même
                #   préférence en terme d'articles, quel que soit le volume d'articles qu'ils ont acheté.
                #        Si la distance euclidienne est choisie, alors les observations avec des valeurs élevées de caractéristiques seront
                #   regroupés ensemble. La même chose vaut pour les observations avec de faibles valeurs de caractéristiques.
                #             

                # III  Standardisation (Normalisation des données) 
                # IV.1   Calcul de la matrice de distance
                 # set.seed(123)
                 # ss <- sample(1:50, 15) # prendre  15 lignes aleatoirement
                 # df <- USArrests[ss, ] 
                 # df.scaled <- scale(df) # Standardisation
                 # 
                 # IV.2 les fonctions de distance 
                 # 1. dist() :[ package stats]: n'accepte que les données numériques en entrée.
                 # 2. get_dist () [package factoextra]: n'accepte que les données numériques en entrée et 
                 # il prend en charge la distance basée sur la corrélation
                 # des mesures incluant les méthodes «pearson», «kendall» et «spearman».
                 # 3. daisy() [package de cluster]: capable de gérer d'autres types de variables (par exemple, nominaux,
                 # ordinal, (a) binaire symétrique). Dans ce cas, le coefficient de Gower
                 # être automatiquement utilisé comme métrique. C'est l'une des mesures les plus populaires de
                 # proximité pour les types de données mixtes.
                 # 
                 # 
                 # # calcul de distance ecludienne
                 # 
                    dist.eucl <- dist(df.scaled, method = "euclidean") 
                    # avec les options "euclidean", "maximum", "Manhattan", "canberra", "binaire", "minkowski".
                    # Pour faciliter la visualisation des informations de distance générées par la fonction dist (), 
                    # on peut reformater le vecteur de distance dans une matrice en utilisant la fonction as.matrix ().
                 
                     round(as.matrix(dist.eucl)[1:3, 1:3], 1) # on l'a arrondi à un chiffre après la virgule  
                    # Dans cette matrice, la valeur représente la distance entre les objets. Les valeurs sur le
                    # diagonale de la matrice représentent la distance entre les objets et eux-mêmes (qui sont zéro).
                     
                     # Dans cet ensemble de données, les colonnes sont des variables. Par conséquent, si nous voulons calculer par paire
                     # distances entre les variables, il faut commencer par transposer les données pour avoir des variables
                     # dans les lignes de l'ensemble de données avant d'utiliser la fonction dist (). 
                     # La fonction t () est utilisée pour transposer les données.
                    
                 
                 # # Calcul des distances basées sur la corrélation
                     # La méthode de corrélation peut être soit pearson, spearman ou kendall.
                   library("factoextra")
                   dist.cor <- get_dist(df.scaled, method = "pearson")
                   round(as.matrix(dist.cor)[1:3, 1:3], 1)
                 
                 # # Calcul des distances pour des données mixtes
                   # La fonction daisy() fournit une solution (mesure de Gower) en informatique pour 
                   # la matrice de distance, dans la situation où les données contiennent des valeurs non numériques colonnes.
                   # un exemple sur les données florales contenant une variables ordonnées facteur et numériques:

                  library(cluster)
                  data(flower)
                  head(flower, 3)
                  str(flower)
                 
                # matrice de Distance de donnee mixte (factor et numerique) mesure de Gower
                 dd <- daisy(flower)
                 round(as.matrix(dd)[1:3, 1:3], 2)
                 
                 
                # Visualisation des matrices de distance
                 library(factoextra)
                 fviz_dist(dist.eucl)
                 # Rouge: haute similarité (ie: faible dissimilarité) 
                 # Bleu: faible similarité
                 # Le niveau de couleur est proportionnel à la valeur de la dissemblance entre les observations:
                 #   rouge pur si dist (xi, xj) = 0 et bleu pur si dist (xi, xj) = 1. 
                 # les Objets appartenant au même cluster sont affichés dans l'ordre consécutif des grappes de couleur.
                 
              
                 
            #### V clustering
                 
                 # algorithme k-mean 
                 # K-means clustering (MacQueen, 1967) est le plus souvent utilisé pour partitionner un ensemble de données donné en k clusters,
                 # où k représente le nombre de groupes prédéfinis par l'analyste. Il
                 # classifie des objets dans plusieurs groupes de sorte à avoir une  haute similitude intra-classe (minimisation de la variance intra-classe),
                 # et une forte  dissemblables inter-classe (entre différentes classes) (maximisation de la variance inter-classe) (c'est-à-dire, une faible similarité inter-classe).
                 #   Dans le clustering k-means, chaque cluster est représenté par son centre (i.e, centroïde)
                 #   correspond à la moyenne des points attribués au cluster.
                      
                    # Il existe plusieurs algorithmes k-means disponibles. 
                    # L'algorithme standard est l'Algorithme de Hartigan-Wong (1979), qui définit la variation totale à l'intérieur des grappes
                    # la somme des distances  distances euclidiennes au carré entre les articles et les centroïdes correspondants :
                 
                 
                 # La première étape de l'utilisation du clustering k-means est d'indiquer le nombre de clusters (k) qui sera généré dans la solution finale.
                 #   L'algorithme commence par sélectionner aléatoirement k objets de l'ensemble de données pour servir de
                 # centres initiaux pour les clusters. Les objets sélectionnés sont également appelés cluster moyens ou centroïdes.
                 #   Ensuite, chacun des objets restants est affecté à son centroïde le plus proche, où le plus proche est
                 # défini à l'aide de la distance euclidienne entre l'objet et le cluster. Cette étape est appelée "étape d'attribution de cluster". 
                 #         Notez que, pour utiliser la distance de corrélation , les données sont entrées en tant que z-scores.
                 #   Après l'étape d'affectation, l'algorithme calcule la nouvelle valeur moyenne de chaque groupe.
                 #         Le terme cluster "mise à jour de centroïde" est utilisé pour concevoir cette étape. Maintenant que les centres
                 # ont été recalculés, chaque observation est vérifiée à nouveau pour voir si elle pourrait être plus proche
                 # à un autre cluster. Tous les objets sont réassignés à l'aide du cluster mis à jour.
                 #         Les étapes d'assignation de cluster et de mise à jour de centroïde sont répétées itérativement jusqu'à
                 # les affectations de cluster cessent de changer (c'est-à-dire jusqu'à ce que la convergence soit atteinte).
                 #         Autrement dit, les groupes formés dans l'itération actuelle sont les mêmes que ceux obtenus dans la précédente itération.
                 
                 # # L'algorithme K-means peut être résumé comme suit:
                 # 1. Spécifiez le nombre de grappes (K) à créer (par l'analyste)
                 # 2. Sélectionnez aléatoirement k objets de l'ensemble de données en tant que centres ou moyens de cluster initiaux
                 # 3. Assigne chaque observation à son centroïde le plus proche, en se basant sur l'Euclide-distance entre l'objet et le centroïde
                 # 4. Pour chacune des k grappes, mettez à jour le centroïde du cluster en calculant la nouvelle valeurs moyennes de tous les points de données du cluster. 
                 # 
                 #       Le centoïde d'un groupe Kth est un vecteur de longueur p contenant les moyennes de toutes les variables pour les observations dans le kième groupe; 
                 #       p est le nombre de variables.
                 # 
                 # 5. Minimiser itérativement le total dans la somme des carrés. Autrement dit, répétez les étapes 3
                 #   et 4 jusqu'à ce que les affectations de cluster cessent de changer quand le nombre maximal des itérations est atteinte. 
                 #       Par défaut, le logiciel R utilise 10 comme valeur par défaut pour le nombre maximum d'itérations, donc à nous de spécifier au besoin.
                   
                 
                 # exemple (mise en pratique du k-mean: 
                 # NB: les variables doivent êtres numériques car le k-mean calcule des moyennes ... 
                 data("USArrests") # on charge la donnée
                 df <- scale(USArrests) # on standarise les données pour les mettre à la même échelle
                 head(df, n = 3)
                 
                  # avec la fonction kmean() du package stats 
                    kmeans(x, centers, iter.max = 10, nstart = 1)
                    # . x: matrice numérique, trame de données (data.frame) numériques ou un vecteur numérique
                    # . centers: les valeurs possibles sont le nombre de grappes (k) ou un ensemble de grappes initiales (distinctes)
                    #            centres de cluster. Si un nombre, un ensemble aléatoire de lignes (distinctes) dans x est choisi comme
                    #            les centres initiaux. 
                    # . iter.max: nombre maximal d'itérations autorisées. La valeur par défaut est 10.
                    # . nstart: nombre de partitions de départ aléatoires lorsque les centres sont des nombres.
                    # Essayer nstart> 1 est souvent recommandé.
                    
                    
                # Pour créer un beau graphique des clusters générés avec la fonction kmeans (),
                # on va utiliser le paquet factoextra.
                
                 # install.packages("factoextra")
                   library(factoextra)
                 
                 
                # Estimons le nombre optimal de grappes 
                    
                    # une question ici est souvent de se demander comment choisir le k qu'on va préciser.
                    # Différentes méthodes seront présentées plus bas dans la section "évaluation et validation des clusters statistiques".

# Ici, nous fournissons une solution simple. L'idée est de calculer le clustering k-means en utilisant
# différentes valeurs de grappes k. Ensuite, le wss (dans la somme des carrés) est tiré selon
# le nombre de grappes. L'emplacement d'un virage (genou ou coude) dans l'intrigue est généralement
# considéré comme un indicateur du nombre approprié de grappes.

                    # La fonction R fviz_nbclust() [dans le paquet factoextra] fournit une solution pratique
                    # pour estimer le nombre optimal de grappes.

                    
                 library(factoextra)
                 fviz_nbclust(df, kmeans, method = "wss")
                 # on remarque des niveaux de coude en (2, 4, 8 et 9),
                 # faire juste 2 classe n'est pas très souvent utils dans les études pratiques alors on peut 
                 # penser au suivent (4). de plus, il faut remarquer qu'en ( 4), on peut observer deux grands changement structurel à gauche et à droite de la courbe des k-cluster.
                 fviz_nbclust(df, kmeans, method = "wss") +
                 geom_vline(xintercept = 4, linetype = 2)
                 
                 # Le graphique ci-dessus représente la variance au sein des grappes. Il diminue à mesure que k augmente,
                 # on peut observer un virage (ou "coude") à k = 4. Ce virage indique que les groupes supplémentaire au-delà du k=4 ont peu de valeur. 
                 
                 # Comme l'algorithme de classification k-means commence par k centroïdes choisis au hasard, il est toujours
                 # recommandé d'utiliser la fonction set.seed() afin de définir une graine pour le générateur de nombres aléatoires de R. 
                 
                 # effectuons une classification k-means avec k = 4:
                
                 set.seed(123)
                 km.res <- kmeans(df, 4, nstart =25)
                 
                 # Comme le résultat final du clustering k-means est sensible auX assignations de départ aléatoire,
                 # nous spécifions nstart = 25. Cela signifie que R va essayer 25 différents aléatoires pour 
                 # commencer les affectations, puis sélectionnez les meilleurs résultats correspondant à celui avec
                 # la plus basse variation du cluster. La valeur par défaut de nstart dans R est 1. Mais c'est
                 # fortement recommandé de calculer le clustering k-means avec une grande valeur de nstart
                 # tels que 25 ou 50, afin d'avoir un résultat plus stable.
                 
                 print(km.res)
                 # on voit :
                 # . les moyennes ou centres de la grappe: une matrice dont les rangées sont le numéro de grappe (1 à 4)
                 # et les colonnes sont des variables
                 # . le vecteur de clustering: un vecteur d'entiers (de 1: k) indiquant le cluster alloué à chaque point
                 
                 # Il est possible de calculer la moyenne de chaque variable par grappe en utilisant les données d'origine: (on a donc utilisé USArrests et non pas les données standardisé "df")
                 aggregate(USArrests, by=list(cluster=km.res$cluster), mean)
                 
                 # Si on souhaite ajouter les classifications de points aux données d'origine, on fait:
                 dd <- cbind(USArrests, cluster = km.res$cluster)
                 head(dd)
                 
                 # Accéder aux résultats de la fonction kmeans ()
                  #  La fonction kmeans () renvoie une liste de composants, incluant:
                  #    . cluster: un vecteur d'entiers (de 1: k) indiquant le cluster auquel chaque le point est attribué
                  #    . centers: une matrice de centres de cluster (moyens de cluster)
                  #    . totss: La somme totale des carrés (TSS), c'est-à-dire q (xi - ¯x)². TSS mesure la variance totale dans les données.
                  #    . withinss: Vecteur de la somme des carrés à l'intérieur d'un cluster, un composant par cluster
                  #    . tot.withinss: somme totale des carrés à l'intérieur des grappes, c'est-à-dire sum (withinss)
                  #    . betweenss: la somme des carrés entre les grappes, c'est-à-dire totss - tot.withinss
                  #    . size: le nombre d'observations dans chaque groupe
                  # Ces composants peuvent être consultés comme suit:  
                 
                 # Numéro de groupe (cluster) pour chacune des observations
                 km.res$cluster
                 head(km.res$cluster, 4)
                 
                 # size (nombre d'observations dans chaque groupe)
                 km.res$size
                 
                 # Cluster moyen
                 km.res$centers
                 
                 ### visualisation des classes
                 # c'est conseillé de tracer les résultats du cluster. Ceux-ci peuvent être utilisés pour évaluer le choix du nombre de grappes (k) 
                 # ainsi que la comparaison de deux analyses de grappes différentes.
                 # Maintenant, nous voulons visualiser les données dans un nuage de points avec la coloration de chaque point de données
                 # selon son affectation de groupe.
                 # Le problème est que les données contiennent plus de 2 variables et la question est de savoir
                 # quelles variables seront choisi pour le diagramme de dispersion xy.
                 # Une solution consiste à réduire le nombre de dimensions en appliquant une réduction de dimension
                 # algorithme, tel que l'analyse en composantes principales (ACP), qui fonctionne sur le
                 # quatre variables et sort deux nouvelles variables (qui représentent les variables d'origine) qu'on peu utiliser pour faire le graph.
                 # 
                 # En d'autres termes, si nous avons un ensemble de données multidimensionnel, une solution est d'effectuer
                 # l'Analyse en composantes principales (PCA) et pour tracer des points de données selon les coordonnées des deux premières composantes principales.
                                                                    
                 # La fonction fviz_cluster() [package factoextra] peut être utilisée pour visualiser facilement les cluster du k-means.
                 # Il prend les résultats de k-means et les données d'origine comme arguments. 
                 # Dans le résultat, les observations sont représentées par des points, en utilisant des composantes principales si le nombre de variables est supérieur à 2. 
                 # Il est également possible de dessiner une ellipse de concentration autour chaque cluster.

                 fviz_cluster(km.res, data = df,
                              palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                              ellipse.type = "euclid", # Concentration ellipse
                              star.plot = TRUE, # Ajouter les segments entre les points et les centroids de leurs classes d'affectation  
                              repel = TRUE, # Eviter le surplacage d'étiquette (lent)
                              ggtheme = theme_minimal()
                 )
                 
                 
          ### Avantages et inconvénients de la classification K-means
                 K-means clustering est un algorithme très simple et rapide. Il peut traiter efficacement avec de très
                 grands ensembles de données. 
                 
                 # Cependant, il existe certaines faiblesses, notamment:
                 #   1. Il suppose une connaissance préalable des données et oblige l'analyste à choisir
                 #      nombre approprié de grappe (k) à l'avance.
                 #   2. Les résultats finaux obtenus sont sensibles à la sélection aléatoire initiale des centres de cluster.
                 #      Ceci est un problème car pour chaque série d'exécution de
                 #      algorithme sur le même ensemble de données, l'algorithme peut choisir différents ensembles de centres initiaux.
                 #      Cela peut conduire à différents résultats de clustering sur différentes exécutions de l'algorithme.
                 # 3. Il est sensible aux valeurs aberrantes.
                 # 4. Si vous réorganisez vos données, il est très possible que vous obteniez une solution différente
                 # chaque fois que vous changez la commande de vos données.
                 # 
                 # Les solutions possibles à ces faiblesses:
                 #   1. Solution au problème 1: Calculer k-means pour une gamme de valeurs k, par exemple
                 # en variant k entre 2 et 10. Ensuite, choisissez le meilleur k en comparant le
                 # regrouper les résultats obtenus pour les différentes valeurs de k.
                 # 2. Solution au problème 2: Calculer l'algorithme K-means plusieurs fois avec différents
                 #    centres de cluster initiaux. La course avec la somme totale la plus faible à l'intérieur de la grappe
                 # carré est sélectionné comme solution finale de clustering.
                 # 3. Pour éviter les distorsions causées par des valeurs aberrantes excessives, il est possible d'utiliser l'algorithme PAM, qui est moins sensible aux valeurs aberrantes.

    ### alternative à l'algorithme k-mean:
                 # Une alternative robuste à k-means est PAM, qui est basée sur medoids. 
                 # Le clustering PAM peut être calculé en utilisant la fonction pam()[package de cluster]. 
                 # La fonction pamk() [fpc package] est un wrapper pour PAM qui affiche aussi
                 # le nombre de grappes suggéré en fonction de la largeur moyenne optimale de la silhouette.
                 ###??? c'est quoi un <<wrapper>>, je doit le demander à l'auteur
                
                 
    #### algorithme k-mediods
                 # L'algorithme k-medoids est une approche de clustering liée au clustering k-means
                 # pour partitionner un ensemble de données en k groupes ou clusters. Dans le clustering k-medoids,
                 # chaque cluster est représenté par l'un des points de données du cluster. Ces points sont nommés médocs de cluster 
                 # 
                 # Le terme médoïde fait référence à un objet au sein d'un groupe pour lequel la dissimilarité moyenne
                 # entre elle et tous les autres  membres de la grappe sont minimes. Cela correspond 
                 # au point le plus central du cluster. Ces objets (un par cluster) peuvent
                 # être considéré comme un exemple représentatif des membres de ce groupe qui
                 # peut être utile dans certaines situations. 
                 # Rappelons que, dans k-means clustering, le centre d'une
                 #  cluster est calculé comme la valeur moyenne de tous les points  du cluster.
                 # K-medoid est une alternative robuste au clustering k-means. Cela signifie que l'algorithme
                 # est moins sensible au bruit et aux valeurs aberrantes, comparé à k-means, car il utilise des médoïdes
                 # en tant que centres de cluster au lieu de moyens (utilisés dans k-means).
                 # L'algorithme k-medoids nécessite que l'utilisateur spécifie k, le nombre de grappes à
                 # généré (comme dans le clustering k-means). Une approche utile pour déterminer le meilleur
                 # nombre de clusters est la méthode silhouette qui sera montré plus bas.
                 # La méthode de clustering k-medoids la plus courante est l'algorithm  PAM  (Partitionnement
                 # Autour de Medoids, Kaufman & Rousseeuw, 1990).

                 
                 ## concept du PAM
                 # L'utilisation de moyennes implique que le clustering k-means est très sensible aux valeurs aberrantes. 
                 # Cela peut gravement influencer l'affectation des observations aux grappes (cluster). 
                 # Un algorithme plus robuste est fourni par l'algorithme PAM qui est moins sensibles aux valeurs abérantes.

                 # L'algorithme PAM est basé sur la recherche de k objets représentatifs ou médoïdes
                 # parmi les observations de l'ensemble de données.
                 # Après avoir trouvé un ensemble de k médioïdes représentatif, les clusters sont construits en assignant chaque observation
                 # au médoïde le plus proche.
                 # 
                 # Ensuite, chaque medoid sélectionné "m" et chaque point de données non-medoid sont échangés et la
                 # fonction objective est calculée. 
                 # La fonction objectif correspond à la somme des dissimilarités de tous les objets à leur médoid le plus proche.
                 # L'étape SWAP tente d'améliorer la qualité du clustering en échangeant objets sélectionnés (medoids) et objets non sélectionnés. 
                 # la fonction objectif peut être réduit en échangeant un objet sélectionné avec un objet non sélectionné, puis
                 # l'échange est effectué. 
                 # Ceci est continué jusqu'à ce que la fonction objectif ne puisse plus être diminué. 
                 # Le but est de trouver k objets représentatifs qui minimisent la somme des
                 # dissimilarités des observations à leur objet représentatif le plus proche.
                 # 
                 # deroulement de l'Algorithme PAM:
                 # 1. Sélectionnez k objets pour devenir les medoids, ou dans le cas où ces objets ont été fournis
                 #    utilisez-les comme médoids;
                 # 2. Calculer la matrice de dissimilarité si elle n'a pas été fournie;
                 # 3. Affectez chaque objet à son médoïde le plus proche;
                 # 4. Pour chaque recherche de cluster, si l'un des objets du cluster diminue
                 #    coefficient de dissimilarité moyenne; si c'est le cas, sélectionnez l'entité qui diminue le plus ce
                 #    coefficient  comme le medoid pour cette grappe;
                 # 5. Si au moins un médoid a changé, allez à (3), sinon terminez l'algorithme.

                 
                # Comme mentionné ci-dessus, l'algorithme PAM fonctionne avec une matrice de dissimilarité, et
                # calculer cette matrice l'algorithme peut utiliser deux mesures:
                #      1. Les distances euclidiennes, qui sont la somme des racines des différences;
                #      2. Et, la distance de Manhattan qui sont la somme des distances absolues.
                #     
                # NB: Notez qu'en pratique, vous devriez obtenir des résultats similaires la plupart du temps, en utilisant soit
                # distance euclidienne ou Manhattan. Si vos données contiennent des valeurs aberrantes, distance Manhattan
                # devrait donner des résultats plus robustes, alors que euclidien serait influencé par inhabituel
                # valeurs.


               # Mise en oeuvre de l'algorithme PAM.
                 data("USArrests") 
                 df <- scale(USArrests) 
                 head(df, n = 3) 
                 
                 # La fonction pam() [package de cluster] et pamk() [package fpc] peuvent être utilisées pour le calcul de PAM
                 
                 # La fonction pamk() ne nécessite pas qu'un utilisateur décide du nombre de grappes K.
   
                      # x<-df
                      # k<-4
                  # pam(x, k, metric = "euclidean", stand = FALSE)
                 # . x: les valeurs possibles comprennent:
                 #   - Matrice de données numériques ou trame de données numériques: chaque ligne correspond à un
                 #        observation, et chaque colonne correspond à une variable.
                 #   - Matrice de dissimilarité: dans ce cas, 
                 #        x est généralement la sortie de daisy() ou de dist()
                 # . k: le nombre de grappes
                 # . metric: les métriques de distance à utiliser. Les options disponibles sont "euclidienne" et "Manhattan".
                 # 
                 # . stand: valeur logique; si TRUE, les variables (colonnes) dans x sont standardisées avant
                 # calculer les dissimilarités. Ignoré en mettant (FALSE) si x est déjà une matrice de dissimilarité.
                 
                 
                 # Pour créer un beau graphique des clusters générés avec la fonction pam(), utilisera
                 # le paquet factoextra.
                 
                   # install.packages(c("cluster", "factoextra"))
                 
                 library(cluster)
                 library(factoextra)
                 
                 # Estimer le nombre optimal de grappes
                 #
                 # Pour estimer le nombre optimal de clusters, nous utiliserons la méthode de la silhouette moyenne.
                 # L'idée est de calculer l'algorithme PAM en utilisant différentes valeurs de clusters k. 
                 # la silhouette moyenne proche des grappes est dessinée en fonction du nombre de grappes. 
                 # La silhouette moyenne mesure la qualité d'un clustering. Une silhouette moyenne élevée
                 # indique un bon regroupement. Le nombre optimal de clusters k est celui qui
                 # maximiser la silhouette moyenne sur une gamme de valeurs possibles pour k (Kaufman et
                 # Rousseeuw [1990]).
                 # La fonction R fviz_nbclust() [package factoextra] fournit une solution pratique pour
                 # estimer le nombre optimal de grappes.

                 
                 library(cluster)
                 library(factoextra)
                 fviz_nbclust(df, pam, method = "silhouette")
                 # ou
                 fviz_nbclust(df, pam, method = "silhouette") +
                   theme_classic()
                 # D'après le graph, le nombre suggéré de grappes est 2. 
                 
                 # PAM exécution avec k=2
                 pam.res <- pam(df, 2)
                 print(pam.res)
                 # . les médoids de la grappe: une matrice, dont les rangées sont les médoïdes et les colonnes sont
                 # variables
                 # . le vecteur de clustering: un vecteur d'entiers (de 1: k) indiquant le cluster 
                 # dont chaque point est alloué
                 
                 # Si on souhaite ajouter les classifications de points aux données d'origine, on fait:
                 dd <- cbind(USArrests, cluster = pam.res$cluster)
                 head(dd, n = 3)
                 
                 # Accéder aux résultats de la fonction pam ()
                 # La fonction pam () retourne un objet de la classe pam dont les composants incluent:
                 #   . medoids: objets représentant des clusters
                 #   . clustering: un vecteur contenant le numéro de cluster de chaque objet
                 # 
                 # Ces composants peuvent être consultés comme suit:
                 
                 # Cluster medoids: New Mexico, Nebraska
                 pam.res$medoids
                 
                 # Cluster numbers
                 pam.res$clustering
                 head(pam.res$clustering)
                 
                 # visualisation 
                 # Si les données contient plus de 2 variables, l'algorithme de l'analyse en composantes principales (ACP)
                 # est utilisé pour réduire la dimensionnalité des données. Dans ce cas, les deux premiers principaux
                 # les dimensions sont utilisées pour tracer les données.
                 
                 fviz_cluster(pam.res,
                              palette = c("#00AFBB", "#FC4E07"), # couleur
                              ellipse.type = "t", # Concentration d'ellipse
                              repel = TRUE, # Eviter le surplacage d'étiquette (lent)
                              ggtheme = theme_classic()
                 )
                 
                 
                 # L'algorithme PAM nécessite que l'utilisateur connaisse les données et indique les
                 # nombre de grappes à produire. Cela peut être estimé en utilisant la fonction
                 # fviz_nbclust [dans le paquet R factoextra].
                 # La fonction pam() [package cluster] peut être utilisée pour calculer l'algorithme PAM.

                 # NB: Notons que, pour les grands ensembles de données, pam () peut avoir besoin de trop de mémoire ou trop
                 # temps de calcul. Dans ce cas, la fonction clara () est préférable. Cela ne devrait pas
                 # être un problème pour les ordinateurs modernes.
                 
                 
        #### CLARA 
                 # CLARA (Clustering Large Applications, Kaufman et Rousseeuw (1990)) est un
                 # extension aux méthodes k-medoids (chapitre 5) pour traiter des grandes quantités de données 
                 # (plus de plusieurs milliers d'observations) afin de réduire
                 # temps de calcul et problème de stockage RAM. Ceci est réalisé en utilisant l'échantillonnage approche.

                 # concepte de base de CLARA
                 #
                 # Au lieu de trouver des médoides pour l'ensemble des données, le CLARA considère un petit échantillon
                 # des données avec une taille fixe (sampsize) et applique l'algorithme PAM  à
                 # un ensemble optimal de médoides générer pour l'échantillon. La qualité des médoides résultants
                 # est mesurée par la dissimilarité moyenne entre chaque objet de l'ensemble de données
                 # et le médoïde de son groupe, défini comme la fonction de coût.
                 # CLARA répète les processus d'échantillonnage et de clustering un nombre de fois prédéfini
                 # afin de minimiser le biais d'échantillonnage. Les résultats finaux du clustering correspondent aux
                 # ensemble de medoids avec le coût minimal. 
                 
                 # L'algorithme CLARA est résumé comme suit:
                 #
                 # 1. Diviser aléatoirement les ensembles de données en plusieurs sous-ensembles de taille fixe (sampsize)
                 # 2. Calcule l'algorithme PAM sur chaque sous-ensemble et choisir le k correspondant
                 # objets représentatifs (médoïdes). Assigner chaque observation de l'ensemble des données
                 # réglé sur le médoïde le plus proche.
                 # 3. Calcule la moyenne (ou la somme) des dissimilarités des observations à
                 # leur medoid le plus proche. Ceci est utilisé comme une mesure de la qualité de la classification.
                 # 4. Conservez le sous-ensemble de données pour lequel la moyenne (ou la somme) est minimale. 
                 #   Une autre l'analyse est effectuée sur la partition finale.

                 # Notons que chaque sous-ensemble de données est obligé de contenir les médoides obtenus du meilleur
                 # sous-ensemble de données jusque-là. Des observations aléatoires sont ajoutées à cet ensemble jusqu'à
                 # ce que sampsize ait été atteint
                 
                 # Pour calculer l'algorithme CLARA dans R, les données doivent être standardisées
                 # 
                 # Ici, nous allons générer un ensemble de données aléatoires. Pour rendre le résultat reproductible, on commence
                 # en utilisant la fonction set.seed().

                 set.seed(1234)
                 # Générer 500 objets, divisés en 2 groupes.
                 # la commande rnorm() crée déjà des données normalisé donc standardisé, alors on aura plus besoin de standardiser la table df avec scale(df) dans la suite.
                 # c'est la table df qui contiendra les données créées ici.
                 df <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
                             cbind(rnorm(300,50,8), rnorm(300,50,8)))
                 # spécifier les nom de colonne 
                 colnames(df) <- c("x", "y")
                 
                 # spécifier les nom des lignes (S1, S2, S3, S4, .... S100, ...)
                 rownames(df) <- paste0("S", 1:nrow(df))
                 # apercu de la table
                 head(df, nrow = 6)
                 
                 # exécution de CLARA
                       # x<-df
                   # clara(x, k, metric = "euclidean", stand = FALSE,
                   #        samples = 5, pamLike = FALSE)
                 
                 # . x: une matrice de données numériques ou une trame de données, chaque ligne correspond à une observation,
                 # et chaque colonne correspond à une variable. Les valeurs manquantes (NAs) sont autorisées.
                 # . k: le nombre de grappes.
                 # . metric: les métriques de distance à utiliser. Les options disponibles sont "euclidienne" et
                 # "Manhattan". Les distances euclidiennes sont la somme des carrés des différences, et
                 # Les distances de Manhattan sont la somme des différences absolues. Notons que la distance de manhattan est moins sensible aux valeurs aberrantes.
                 # . stand: valeur logique; si TRUE, les variables (colonnes) dans x seront standardisées avant de
                 # calculer les dissimilarités. Notons qu'il est recommandé de standardiser
                 # les variables avant le regroupement.
                 # . samples: nombre d'échantillons à extraire de l'ensemble de données. La valeur par défaut est 5
                 # mais il est recommandé une valeur beaucoup plus grande.
                 # . pamLike: logique indiquant si le même algorithme dans la fonction pam() doit
                 # être utilisé alors Cela devrait toujours être TRUE.
                 
                 
                 # install.packages(c("cluster", "factoextra"))
                 
                 library(cluster)
                 library(factoextra)
                 
                 library(cluster)
                 library(factoextra)
                 
                 # recherche du nombre de clusteur optimal dans CLARA
                 fviz_nbclust(df, clara, method = "silhouette")+
                   theme_classic()
                 # ON RETIEN 2 clusters
                 
                 # exécution de CLARA
                 clara.res <- clara(df, 2, samples = 50, pamLike = TRUE)
                 print(clara.res)
                 
                 # La sortie de la fonction clara() inclut les composants suivants:
                 # . medoids: objets représentant des clusters
                 # . clustering: un vecteur contenant le numéro de cluster de chaque objet
                 # . sample: étiquettes ou numéros de cas des observations du meilleur échantillon, c'est-à-dire
                 # l'échantillon utilisé par l'algorithme de clara pour la partition finale.
                 
                 # Si on souhaite ajouter les classifications de points aux données d'origine, on fait:
                 dd <- cbind(df, cluster = clara.res$cluster)
                 head(dd, n = 4)
                 
                 # Medoids
                 clara.res$medoids
                 
                 # Clustering
                 head(clara.res$clustering, 10)
                 
                 # visualisation des groupes
                 fviz_cluster(clara.res,
                              palette = c("#00AFBB", "#FC4E07"), # couleur
                              ellipse.type = "t", # Concentration des ellipse
                              geom = "point", pointsize = 1,
                              ggtheme = theme_classic()
                 )
                 
  ### hierachique clustering
                 
                 # Le clustering hiérarchique [ou l'analyse hiérarchique de cluster (HCA)] est une approche alternative
                 #  de la mise en grappe de partitionnement  pour grouper des objets en fonction de
                 # leur similitude. Contrairement au clustering de partitionnement examiné plus haut, le clustering hiérarchique 
                 # n'exige pas de pré-spécifier le nombre de grappes à produire.
                 # 
                 # La classification hiérarchique peut être subdivisée en deux types:
                 # . clustering agglomératif dans lequel, chaque observation est initialement considérée comme
                 #   grappe de ses propres (feuille). Ensuite, les groupes les plus similaires sont successivement fusionnés
                 #   jusqu'à ce qu'il n'y ait qu'un seul gros cluster (racine).
                 # . Le clustering divise, inverse du clustering agglomératif, commence par la racine,
                 #   En effet, tous les objets sont inclus dans un cluster. Alors le plus hétérogène
                 #    sont divisés successivement en groupes jusqu'à ce que toutes les observations soient dans leur propre groupe.
                 #   
                 # Le résultat de la classification hiérarchique est une représentation arborescente des objets, qui
                 # est également connu sous le nom de dendrogramme.
                 #                             
                 # Le dendrogramme est une hiérarchie à plusieurs niveaux où les groupes à un niveau sont reliés
                 # pour former les groupes aux niveaux suivants. Cela permet de décider du niveau
                 # ou couper l'arbre pour générer des groupes appropriés d'objets de données.
           
                 
                 # 1 Clustering agglomératif
                 # Le clustering agglomératif est le type de clustering hiérarchique le plus courant
                 # permet de regrouper des objets dans des clusters en fonction de leur similarité. Il est également connu comme AGNES
                 # (Nidification agglomérative). L'algorithme commence par traiter chaque objet comme un singleton.
                 # Ensuite, les paires de clusters sont successivement fusionnées jusqu'à ce que tous les clusters
                 # été fusionné en un grand cluster contenant tous les objets. Le résultat est un arbre
                 # représentatif des objets, l'arbre est nommé dendrogramme.
                 # 
                 #  # algorithm 
                 # Le regroupement agglomératif fonctionne de manière ascendante. Autrement dit, chaque objet est
                 # initialement considéré comme un cluster à un seul élément (feuille). À chaque étape de l'algorithme,
                 # les deux groupes qui sont les plus similaires sont combinés dans un nouveau groupe plus grand
                 # (nouds). Cette procédure est répétée jusqu'à ce que tous les points soient membres d'un seul grand cluster (racine).
                 # 
                 # L'inverse du clustering agglomératif est le clustering de division, également appelé 
                 # DIANA (Division Analysis) et fonctionne de manière "top-down". Cela commence par 
                 # root, dans lequel tous les objets sont inclus dans un seul cluster. 
                 # A chaque étape de l'itération, le cluster le plus hétérogène est divisé en deux. Le processus est itéré jusqu'à ce que tout
                 # les objets sont dans leur propre groupe.
                 # Notons que le regroupement agglomératif permet d'identifier de petites grappes. 
                 # la division du regroupement est bon pour identifier les grands groupes. 
                 # 
                 # NB: nous voyons seulement le clustering hiérarchique agglomératif ici,
                 # le clustering de division sera étudié avec un autre livre.

                 # les tapes de la classification hiérarchique agglomérative
                 # 1. Préparer les données
                 # 2. Calcul des informations de (dis) similarité entre chaque paire d'objets dans les données.
                 # 3. Utilisation de la fonction de liaison pour regrouper les objets dans l'arborescence hiérarchique, sur la base 
                 # des informations de distance générées à l'étape 1. Objets / clusters qui sont proches
                 # la proximité est liée à l'aide de la fonction de liaison.
                 # 4. Déterminer où couper l'arbre hiérarchique en grappes. Cela crée une partition des données.
                 
                 # préparation des données 
                 # Les données doivent être une matrice numérique avec:
                 # . les rangées représentant les observations (individus);
                 # . et les colonnes représentant les variables.
                 # 
                 # 
                 # Notons qu'il est généralement recommandé de normaliser les variables dans l'ensemble de données avant
                 # effectuer une analyse ultérieure. La normalisation rend les variables comparables 
                 # lorsqu'elles sont mesurés à différentes échelles. La fonction R
                 # scale () peut être utilisé pour la standardisation.
                 
                 data("USArrests")
                 # Standardisation
                 df <- scale(USArrests)
                 head(df, nrow = 6)
                 
                 # Mesures de similarité
                 # Afin de décider quels objets / groupes doivent être combinés ou divisés, nous mesurons la similarité entre objets.
                 # Il existe de nombreuses méthodes pour calculer les informations de (dis) similarité, y compris Euclidienne
                 # et les distances de Manhattan. on peu utiliser la fonction 
                 # dist() pour calculer la distance entre chaque paire d'objets dans un ensemble de données. 
                 # Les resultats de ce calcul sont connu comme une matrice de distance ou de dissimilarité.
                 # 
                 # Par défaut, la fonction dist() calcule la distance euclidienne entre les objets;
                 # Cependant, il est possible d'indiquer d'autres métriques en utilisant la méthode de l'argument. 
                 
                 # calcule de la matrice de  dissimilarité
                 # df = données standardisée
                 res.dist <- dist(df, method = "euclidean")
                 # Notons que la fonction dist() calcule la distance entre les lignes d'une matrice de donnée
                 #  en utilisant la méthode de mesure de distance spécifiée.
                 
                 # Pour voir facilement l'information de distance entre les objets, nous reformatons les résultats de la
                 # fonction dist() dans une matrice en utilisant la fonction as.matrix(). Dans cette matrice, valeur en
                 # cellule formée par la rangée i, la colonne j, représente la distance entre l'objet i
                 # et l'objet j dans l'ensemble de données d'origine. 
                 # Par exemple, l'élément 1,1 représente la distance
                 # entre l'objet 1 et lui-même (qui est nul). L'élément 1,2 représente la distance
                 # entre l'objet 1 et l'objet 2, et ainsi de suite.
                 # 
                 # Le code ci-dessous affiche les 6 premières lignes et colonnes de la matrice de distance:
                 as.matrix(res.dist)[1:6, 1:6]
                 
                 # Lien (linkage)
                 # La fonction de liaison prend l'information de distance, retournée par la fonction dist(),
                 # et regroupe les paires d'objets en grappes en fonction de leur similarité. Ensuite, ces nouveaux
                 # groupes formés sont liés les uns aux autres pour créer des groupes plus gros. Ce processus est
                 # itérée jusqu'à ce que tous les objets de l'ensemble de données d'origine soient liés entre eux dans une hiérarchie de l'arbre.
                 
                 # Par exemple, étant donné une matrice de distance "res.dist" générée par la fonction dist(),
                 # la fonction  hclust() peut être utilisée pour créer l'arborescence hiérarchique.
                 # hclust() peut être utilisé comme suit:
                 res.hc <- hclust(d = res.dist, method = "ward.D2")
                 # . d: une structure de dissimilarité produite par la fonction dist().
                 # . méthode: méthode d'agglomération (linkage) à utiliser pour calculer la distance
                 # entre les groupes. Les valeurs autorisées sont l'une de "ward.D", "ward.D2", "single",
                 # "Complet", "moyen", "mcquitty", "médian" ou "centroïde".
                 # Il existe de nombreuses méthodes d'agglomération de clusters (c'est-à-dire des méthodes de liaison). 
                 # les méthodes de liaison les plus communes sont décrites ci-dessous:
                   # . Liaison maximale ou complète: La distance entre deux grappes est définie comme
                   #   la valeur maximale de toutes les distances par paires entre les éléments du cluster 1
                   #   et les éléments du groupe 2. Il tend à produire des groupes plus compacts.
                   # . Liaison minimale ou unique: la distance entre deux grappes est définie
                   #   comme la valeur minimale de toutes les distances par paires entre les éléments du
                   #   cluster 1 et les éléments du cluster 2. Il a tendance à produire de longues grappes «loose».
                   # . Liaison moyenne ou moyenne: La distance entre deux grappes est définie comme
                   #   distance moyenne entre les éléments du groupe 1 et les éléments du groupe 2.
                   # . Liaison centroïde: La distance entre deux grappes est définie comme la distance
                   #   entre le centroïde pour le cluster 1 (un vecteur moyen de longueur p variables) et
                   #   le centroïde pour le groupe 2.
                   # . Méthode de variance minimale de Ward: Elle minimise la variance totale intra-cluster.
                   #   A chaque étape, la paire de clusters avec une distance minimale entre les clusters
                   #   sont fusionnés.
                   # Notons qu'à chaque étape du processus de clustering, les deux clusters, qui ont les
                   # plus petite distance de liaison, sont regroupés ensemble.
                   # Le couplage complet et la méthode de Ward sont généralement préférés.
                     
                 # dendrogrammes
                 # Les dendrogrammes correspondent à la représentation graphique de l'arbre hiérarchique
                 # peuvent généré par la fonction R base hclust() comme précédament construit dans l'objet (res.hc)
                 # et on le visualise en faisant le plot
                 res.hc <- hclust(d = res.dist, method = "ward.D2")
                 plot(res.hc)
                 
                 # Ici, nous allons utiliser la fonction fviz_dend() [dans le paquet R factoextra] pour produire un beau dendrogramme.
                  
                 library("factoextra")
                 fviz_dend(res.hc, cex = 0.5) # cex: taille des label
                 
                 # Dans le dendrogramme affiché ci-dessus, chaque feuille correspond à un objet. Comme nous déménageons
                 # dans l'arbre, des objets similaires les uns aux autres sont combinés en branches, ce qui
                 # sont eux-mêmes fusionnés à une hauteur plus élevée.
                 # La hauteur de la fusion, indiquée sur l'axe vertical, indique la (dis) similarité / distance
                 # entre deux objets / groupes. Plus la hauteur de la fusion est élevée,  
                 # moins les objets sont similaire. Cette hauteur est connue comme la distance cophénétique entre les deux objets.
                 
                 # Notons que, les conclusions sur la proximité de deux objets peuvent être tirées uniquement sur la base de
                 # la hauteur où les branches contenant ces deux objets sont d'abord fusionnées. Nous ne pouvons pas utiliser
                 # la proximité de deux objets le long de l'axe horizontal comme critère de leur similarité.

                 
                 # Vérification de  l'arborescence du cluster
                 #
                 # Après avoir lié les objets dans un ensemble de données dans un arbre de cluster hiérarchique, 
                 # on peut vouloir évaluer les distances (c'est-à-dire, les hauteurs dans l'arbre reflètent les distances) d'origine avec précision.
                 # Une façon de mesurer la qualité de l'arborescence de cluster générée par la fonction hclust()
                 # sur nos données est de calculer la corrélation entre les distances cophénétiques et
                 # la distance des données d'origine générées par la fonction dist(). 
                 # Si le clustering est valide, la liaison des objets dans l'arbre de cluster devrait avoir une forte corrélation avec le
                 # distances entre les objets dans la matrice de distance d'origine.
                 # Plus la valeur du coefficient de corrélation est proche de 1, plus la
                 # solution du cluster reflète vos données. 
                 # Les valeurs supérieures à 0,75 sont jugées bonnes. 
                 # la Méthode de liaison "moyenne" ou en anglais "average" semble produire des valeurs élevées de cette statistique. 
                 # Ceci peut être une raison pour laquelle il est si populaire.
                 # La fonction de base R cophenetic() peut être utilisée pour calculer les distances cophénétiques
                 # de la classification hiérarchique.                 
                 
                 # calculons la distance cophenetic 
                 res.coph <- cophenetic(res.hc)
                 # vérification des clusters
                 # Correlation entre distance cophenetic  et distance d'origine
                 cor(res.dist, res.coph) # donne 0.6975266
                 # la corrélation entre les distances cophénique et les distance d'origine 
                 # donne 0.6975266 avec le cluster realisé avec la fonction hclust(,method="ward.D2") 
                 
                 # essayons avec la liaison "moyenne" (méthod="average") de hclust() pour voir si la corrélation sera amélioré:
                 res.hc2 <- hclust(res.dist, method = "average")
                 cor(res.dist, cophenetic(res.hc2)) # donne 0.7180382
                 # Le coefficient de corrélation donne 0.7180382 et est supérieur à celui de la méthode "wad2".
                 # cela montre que l'utilisation de la méthode de liaison "différente"average" crée un arbre
                 # qui représente légèrement mieux les distances d'origine.
                 # Ainsi, la nature des méthodes de liaison utilisé influence la bonne représentation des données par l'arbre généré.
                 
                 
                 comment Couper le dendrogramme en différents groupes
                 
                 L'un des problèmes de la classification hiérarchique est que cela ne nous dit pas combien
                 il existe des grappes, ou où couper le dendrogramme pour former des grappes.
                 nous pouvons couper l'arborescence hiérarchique à une hauteur donnée afin de partitionner nos données
                 en grappes. La fonction de base R cutree() peut être utilisée pour couper un arbre qui a été généré par
                 la fonction hclust(), en plusieurs groupes soit en spécifiant le nombre désiré de
                 groupes ou la hauteur de coupe. Il renvoie un vecteur contenant le numéro de cluster de chaque
                 observation.
                 
                 # exemple: coupons l'arbre en 4 groupes 
                 grp <- cutree(res.hc, k = 4)# coupe l'arbre en 4 groupe
                 head(grp, n =7) # afficher les 7 première observation et leurs groupe respectif
                 
                 # Nombre d'observation ou d'individus dans chacun des 4 groupes
                 table(grp)
                       
                 # afficher les nom des individus contenu dans le groupe numéro 1
                 rownames(df)[grp ==1]
                 # afficher les nom des individus contenu dans le groupe numéro 2
                 rownames(df)[grp ==2]
                 # afficher les nom des individus contenu dans le groupe numéro 3
                 rownames(df)[grp ==3]
                 # afficher les nom des individus contenu dans le groupe numéro 4
                 rownames(df)[grp ==4]
                 
                       
                       
                 # visualisation des 4 groupes de l'arbre
                 fviz_dend(res.hc, k = 4, # Cut in four groups
                                 cex = 0.5, # label size
                                 k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                                 color_labels_by_k = TRUE, # color labels by groups
                                 rect = TRUE # Add rectangle around groups
                       )
                 # changeons la couleur du premier grappe en "bleue" pour mieux le distinguer du deuxieme 
                 # car les couleurs précédemment choisir pour les groupes 1 et 2 sont trop proche 
                 # et ne permettent pas une bonne distinction visuel entre ces deux couleurs
                 fviz_dend(res.hc, k = 4, # Cut in four groups
                           cex = 0.5, # label size
                           k_colors = c("blue", "#00AFBB", "#E7B800", "#FC4E07"),
                           color_labels_by_k = TRUE, # color labels by groups
                           rect = TRUE # Add rectangle around groups
                 )     
                 
                 # Visualisons le nuage de points. 
                 # Les observations sont représentées par des points dans le graph en utilisant les composantes principales.
                 # Un cadre est dessiné autour de chaque groupe.
                   fviz_cluster(list(data = df, cluster = grp),
                                    palette = c("black", "#00AFBB", "#E7B800", "#FC4E07"),
                                    ellipse.type = "convex", # Concentration des ellipse
                                    repel = TRUE, # Eviter le surplacage (chevauchement) des étiquettes (cela rend l'exécution lente)
                                    show.clust.cent = FALSE, ggtheme = theme_minimal()
                              )
                       
                #  Package Cluster R
                   # Le package "cluster" de  R facilite l'analyse de cluster. Il fournit
                   # la fonction agnes() et diana() pour le calcul de clustering agglomératif et de division,
                   # respectivement. Ces fonctions effectuent toutes les étapes nécessaires pour nous. On a plus besoin
                   #  d'exécuter les fonctions scale(), dist() et hclust() séparément car ces fonctions agnes() et diana() le fond déjà en faisant  toutes les étapes. 
                   # 
                   # example d'exécution suit:
                   # 
                   # clustering agglomératif (agnes)
                   # classification hiérarchique agglomérative 
                   library("cluster")
                   res.agnes <- agnes(x = USArrests, # data matrix
                                          stand = TRUE, # Standardize the data
                                          metric = "euclidean", # metric for distance matrix
                                          method = "ward" # Linkage method
                       )
                       
                   # clustering  de division (diana)
                   res.diana <- diana(x = USArrests, # data matrix
                                          stand = TRUE, # standardize the data
                                          metric = "euclidean" # metric for distance matrix
                       )
                       
                   # visualisation 
                       fviz_dend(res.agnes, cex = 0.6, k = 4)
                       fviz_dend(res.diana, cex = 0.6, k = 4)
                       
                       # Application de la classification hiérarchique au gène
                       # analyse des données d'expression
                       # ....... niveau 7.6
                       
                      
    ##### comparaison de dendogramme
                       # pour comparer deux dendogramme, nous allons utiliser le package "dendextend"
                       # ce package contient plusieurs fonctions. ici, nous allons mettre l'acent sur les fonctions :
                       #   - tanglegram() pour visualiser et comparer deux dendogramme
                       #   - et cor.dendlist() pour calculer la matrice des corrélation entre deux dendogrammes.

                       # preparation des données
                       df <- scale(USArrests)
                       
                       # Pour rendre lisibles les graphiques, générés dans la suite, nous allons travailler avec un petit
                       # sous-ensemble aléatoire de l'ensemble de données. Par conséquent, nous allons utiliser la fonction sample() pour sélectionner aléatoirement
                       #  10 observations parmi les 50 observations contenues dans l'ensemble de données:
                       
                       set.seed(123)
                       ss <- sample(1:50, 10)
                       df <- df[ss,]
                       
                       # comparaison de dendogramme
                       # Nous commençons par créer une liste de deux dendrogrammes en calculant la classification hiérarchique
                       # (HC) en utilisant deux méthodes de liaison différentes ("average" et "ward.D2"). Ensuite nous
                       # transformer les résultats en dendrogrammes et créer une liste pour tenir les deux dendrogrammes.
                       
                       library(dendextend)
                       # calcul de la matrice de distance (euclidienne)
                       res.dist <- dist(df, method = "euclidean")
                       # calcul de 2 clustering hierarchique avec deux différentes méthodes (average et wad2)
                       hc1 <- hclust(res.dist, method = "average")
                       hc2 <- hclust(res.dist, method = "ward.D2")
                       # creation de 2 dendrogrammes
                       dend1 <- as.dendrogram (hc1)
                       dend2 <- as.dendrogram (hc2)
                       # Créer une liste pour contenir les 2 dendrogrammes ensemble
                       dend_list <- dendlist(dend1, dend2)
                       
                       # Pour comparer visuellement deux dendrogrammes, 
                       # nous utiliserons la fonction tanglegram() qui trace les deux dendrogrammes, côte à côte, avec leurs étiquettes connectées par des lignes.
                       # La qualité de l'alignement des deux arbres peut être mesurée en utilisant la fonction
                       # entanglement(). L'enchevêtrement est une mesure entre 1 (enchevêtrement complet) et 0 (non enchevêtrement). 
                       # Un coefficient d'intrication faible correspond à un bon alignement.

                       tanglegram(dend1, dend2)
                       # on remarque que certaines classes se regroupent de la même manière entre les deux dendogrammes
                       # et que d'autres sont legèrement regroupé différemments(les pointillés indiquent la présence de deux regroupement différents).  
                       
                       # comme je l'ai expliqué plus haut, l'auteur indique de noter que 
                       # les nouds "uniques", avec une combinaison d'étiquettes / éléments non présents dans le
                       # autre arbre, sont mis en évidence avec des lignes pointillées.
                       

                       
                       # on peut Personnalisé le tanglegram en utilisant de nombreuses autres options comme suit:
                       tanglegram(dend1, dend2,
                                  highlight_distinct_edges = FALSE, # Désactiver les lignes pointillées
                                  common_subtrees_color_lines = FALSE, # Désactiver les couleurs des lignes  de correspondance des étiquettes entre les deux dendogrammes
                                  common_subtrees_color_branches = TRUE, # colorer Branches communes 
                                  main = paste("entanglement =", round(entanglement(dend_list), 2))
                       )
                       
                       # Matrice de corrélation entre une liste de dendrogrammes
                       # La fonction cor.dendlist() est utilisée pour calculer la matrice de corrélation "Baker" ou "Cophenetic"
                       #  entre une liste d'arbres. La valeur peut aller de -1 à 1. Des Valeurs proche de 0
                       #  signifient que les deux arbres ne sont pas statistiquement similaires.

                       # matrice de  correlation de Cophenetique
                       cor.dendlist(dend_list, method = "cophenetic")
                       
                       # matrice de  correlation de Baker 
                       cor.dendlist(dend_list, method = "baker")
                       
                       # coefficient de  correlation de Cophenetic 
                       cor_cophenetic(dend1, dend2)
                       
                       # coefficient de  correlation de Baker
                       cor_bakers_gamma(dend1, dend2)
                       
                       
                       # dans les deux cas (cophenic et baker), les coéfficient sont très proche de 1
                       # alors les deux graphs sont statistiquement très similaires. on le remarque meme visuelement plus haut dans la visualisation du comparatif

                       
                       # Il est également possible de comparer simultanément plusieurs dendrogrammes. 
                       #  Un opérateur de chaînage "%>%" est utilisé pour exécuter plusieurs fonctions en même temps. 

                       # Créer plusieurs dendrogrammes en enchaînant
                       dend1 <- df %>% dist %>% hclust("complete") %>% as.dendrogram
                       dend2 <- df %>% dist %>% hclust("single") %>% as.dendrogram
                       dend3 <- df %>% dist %>% hclust("average") %>% as.dendrogram
                       dend4 <- df %>% dist %>% hclust("centroid") %>% as.dendrogram
                       
                       # calcul de la matrice de correlation 
                       dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                                             "Average" = dend3, "Centroid" = dend4)
                       cors <- cor.dendlist(dend_list)
                       # print
                       round(cors, 2)
                       
                       # visualiser les correlation comme graph
                       library(corrplot)
                       corrplot(cors, "pie", "lower")
                       
                       # on observe que les méthodes complete et average sont très liées
                       
                       
                # visualisation de dendogramme
                       # Comme décrit plus haut, un dendrogramme est une représentation arborescente de
                       # données créées en utilisant des méthodes de classification hiérarchiques. Dans cet article, nous
                       # allons visualiser et personnaliser les dendrogrammes. De plus, nous montrons
                       # comment enregistrer et zoomer un grand dendrogramme.

                       # Nous commençons par calculer la classification hiérarchique en utilisant les ensembles de données USArrests:

                       data(USArrests)
                       # Calculer les distances 
                       dd <- dist(scale(USArrests), method = "euclidean")
                       # classification hiérarchique
                       hc <- hclust(dd, method = "ward.D2")
                       
                       # Visualisation  
                       #
                       # Nous allons utiliser la fonction fviz_dend () [du package factoextra] pour créer facilement une belle
                       # dendrogramme en utilisant le tracé de base R ou ggplot2. Il fournit également une option pour
                       # dessiner des dendrogrammes circulaires et des arbres de type phylogénique.
                       # 
                       # install.packages(c("factoextra", "dendextend"))
                       
                       library(factoextra)
                       fviz_dend(hc, cex = 0.5)
                       
                       # nous pouvons utiliser les arguments main, sub, xlab, ylab pour modifier les titres de tracé comme suit
                       
                       fviz_dend(hc, cex = 0.5,
                                 main = "Dendrogram - ward.D2",
                                 xlab = "Objects", ylab = "Distance", sub = "")
                       
                       # Pour dessiner un dendrogramme horizontal:
                       fviz_dend(hc, cex = 0.5, horiz = TRUE)
                       
                       # Il est également possible de couper l'arbre à une hauteur donnée pour partitionner les données en plusieurs
                       # groupes comme décrit plus haut dans la Classification hiérarchique. 
                       # Dans ce cas, il est possible de colorier les branches par groupes et d'ajouter un rectangle autour de chaque groupe.
                       # 

                       fviz_dend(hc, k = 4, # couper en quatre groupes
                                 cex = 0.5, # taille des label
                                 k_colors = c("blue", "#00AFBB", "#E7B800", "#FC4E07"),
                                 color_labels_by_k = TRUE, # colorer les label par la couleur de son groupe 
                                 rect = TRUE, # ajouter des rectangles autour de chaque groupes
                                 rect_border = c("blue", "#00AFBB", "#E7B800", "#FC4E07"), # couleur des bords des rectangles
                                 rect_fill = TRUE)
                       
                       # Pour modifier le thème de tracé, utilisons l'argument ggtheme, 
                       # dont les valeurs autorisées incluent des thèmes officiels de ggplot2
                       # comme [theme_gray(), theme_bw(), theme_minimal(), theme_classic (),
                       # theme_void()] ou tout autre thème ggplot2 défini par l'utilisateur.

                       fviz_dend(hc, k = 4, 
                                 cex = 0.5, 
                                 k_colors = c("blue", "#00AFBB", "#E7B800", "#FC4E07"),
                                 color_labels_by_k = TRUE, 
                                 ggtheme = theme_gray() # Changer le theme en fond de couleur gris
                       )
                       
                       # Les valeurs autorisées pour k_color incluent les palettes de brasseurs du package "RColorBrewer" (par exemple "RdBu", "Blues", "Dark2", "Set2",. . . ; )
                       # et palettes de revues scientifiques du package "ggsci" (par exemple: "npg", "aaas", "lancette", "jco", "ucscgb", "uchicago", "simpsons" et "Rickandmorty").
                       # Changeons les couleurs du groupe en utilisant les palette de couleurs "jco" (journal d'oncologie clinique):

                       fviz_dend(hc, cex = 0.5, k = 4, 
                                 k_colors = "jco")
                       
                       # dessinons un dendrogramme horizontal avec un rectangle autour des clusters:
                       fviz_dend(hc, k = 4, cex = 0.4, horiz = TRUE, k_colors = "jco",
                                 rect = TRUE, rect_border = "jco", rect_fill = TRUE)
                       
                       # tracons un dendrogramme circulaire en utilisant l'option type = "circular".
                       fviz_dend(hc, cex = 0.5, k = 4,
                                 k_colors = "jco", type = "circular")
                       
                       # Pour tracer un arbre de type phylogénique, 
                       # on utilise type = "phylogenic" et repel = TRUE ( pour Eviter le surplacage (chevauchement) des étiquettes (cela rend l'exécution lente))
                       # Cette fonctionnalité (phylogénique) nécessite le package R "igraph". 

                       require("igraph")
                       fviz_dend(hc, k = 4, k_colors = "jco",
                                 type = "phylogenic", repel = TRUE)
                       
                       # La disposition par défaut pour les arbres phylogéniques est "layout.auto". Les valeurs autorisées sont l'une des suivantes:
                       # c("layout.auto", "layout_with_drl", "layout_as_tree", "layout.gem", "layout.mds", "Layout_with_lgl"). 
                       #
                       # faisons un exemple avec hylo.layout = "layout.gem":
                       require("igraph")
                       fviz_dend(hc, k = 4, 
                                 k_colors = "jco",
                                 type = "phylogenic", repel = TRUE,
                                 phylo_layout = "layout.gem")
                       
                # cas de dendogramme sur une grosse base de données
                       # Si nous calculons le clustering hiérarchique sur un grand ensemble de données, 
                       # Nous pouvons zoomer le dendrogramme ou  ne représenter qu'un sous-ensemble du dendrogramme.
                       # Alternativement, nous pouvons également tracer le dendrogramme sur une grande page sur un PDF, qui
                       # poura être zoomé sans perte de résolution.

                       #Zoom dans le dendrogramme
                       #
                       # Si nous voulons zoomer dans les premiers clusters, il est possible d'utiliser l'option xlim et ylim
                       # pour limiter la zone de traçage. 
                         
                       fviz_dend(hc, xlim = c(1, 20), ylim = c(1, 8)) # on fixe les marges de xlim et ylim en les observant sur le gros graph initial qui n'était pas encore zoomé
                       
                       # Tracer un sous-arbre de dendrogrammes
                       # 
                       # Pour tracer un sous-arbre, nous suivrons la procédure ci-dessous:
                       #  1. Créer le dendrogramme entier en utilisant fviz_dend() et enregistrez le résultat dans un
                       # objet, que nous nommons  dend_plot.
                       # 2. Utiliser la fonction de base R cut.dendrogram() pour couper le dendrogramme, à une valeur donnée.
                       # de hauteur (h), en plusieurs sous-arbres. Cela retourne une liste avec les composants $upper
                       # et $lower, le premier est une version tronquée de l'arbre d'origine, également de classe
                       # dendrogramme, ce dernier est une liste avec les branches obtenues à partir de la coupe de l'arbre,
                       # de chacun un dendrogramme.
                       # 3. Visualisez les sous-arbres en utilisant fviz_dend().
                       # 
                       # . Couper le dendrogramme et visualisez la version tronquée:

                       # Créer un graph de l'ensemble du dendrogramme,
                       
                       dend_plot <- fviz_dend(hc, k = 4, 
                                              cex = 0.5, 
                                              k_colors = "jco"
                       )
                       
                       # extraire les données de dendrogramme
                       dend_data <- attr(dend_plot, "dendrogram") 
                       
                       
                       # Couper le dendrogramme à la hauteur h = 10
                       dend_cuts <- cut(dend_data, h = 10)
                       # Visualisez la version tronquée contenant 2 branches (tracer le sous-arbre)
                       fviz_dend(dend_cuts$upper) 
                       
                       # Tracer le dendrogramme entier
                       print(dend_plot)
                       
                       
                       # tracer le sous-arbre 1
                       fviz_dend(dend_cuts$lower[[1]], main = "sous-arbre 1")
                       # tracer le sous-arbre 2
                       fviz_dend(dend_cuts$lower[[2]], main = "sous-arbre 2")
                       
                       
                       # nous pouvons également tracer des arbres circulaires comme suit (pour le sous-arbre 2):
                       fviz_dend(dend_cuts$lower[[2]], type = "circular")
                       
                       # enregistrer dans une page PDF avec une meilleur résolution de l'image
                       # Si nous avons un grand dendrogramme, nous pouvons l'enregistrer sur une grande page PDF, 
                       # qui peut être zoomée sans perdre de résolution.
                       getwd()
                       pdf("dendrogram.pdf", width=30, height=15) # ouvre un PDF
                       p <- fviz_dend(hc, k = 4, cex = 1, k_colors = "jco" ) # trace le graph et l'enregistre dans l'objet nommé "p"
                       print(p) # imprimer (afficher) l'objet "p" qui ici, contient notre graph dans le pdf ouvert
                       dev.off() # fermer le pdf
                       
                       
                       # manipulation de dendogramme
                       # Le package dendextend fournit des fonctions pour changer facilement l'apparence d'un
                       # dendrogramme et pour comparer les dendrogrammes.
                       # Dans cette section, nous allons utiliser l'opérateur de chaînage (%>%) pour simplifier notre code.
                       # L'opérateur de chaînage transforme x%>% f (y) en f (x, y) afin que vous puissiez l'utiliser pour réécrire plusieurs
                       # opérations telles qu'elles peuvent être lues de gauche à droite, de haut en bas. 
                       # . Code R standard pour créer un dendrogramme:
                         
                       data <- scale(USArrests)
                       dist.res <- dist(data)
                       hc <- hclust(dist.res, method = "ward.D2")
                       dend <- as.dendrogram(hc)
                       plot(dend)
                       
                       # créer un dendogramme avec des opérateurs de chaine
                       library(dendextend)
                       dend <- USArrests[1:5,] %>% # données
                         scale %>% # standardiser des données
                         dist %>% # calculer la matrice de distance,
                         hclust(method = "ward.D2") %>% #  clustering hierachique
                         as.dendrogram # convertir l'objet en dendrogramme.
                       plot(dend)
                       
                       
                       # Fonctions pour personnaliser les dendrogrammes: La fonction set() [du package dendextend]
                       # peut être utilisé pour changer les paramètres d'un dendrogramme. 

                       #>  set(object, what, value)
                       
                       
                       # 1. objet: un objet dendrogramme
                       # 2. what: un caractère indiquant quelle est la propriété de l'arbre qui devrait être
                       # définir / mis à jour
                       # 3. value: un vecteur avec la valeur à définir dans l'arbre (le type de la valeur dépend de celui du "what").
                       # Les valeurs possibles pour l'argument comprennent:
                       # 
                       # Valeur pour l'argument "what"           description
                       # labels                                  afficher les labels
                       # labels_colors and labels_cex            Définir la couleur et la taille des labels
                       # labels,                                 respectivement
                       # 
                       # leaves_pch, leaves_cex and              Définir le type, taille and couleur des points 
                       # leaves_col                              pour les feuilles, respectivement
                       # 
                       # nodes_pch, nodes_cex and                Définir le type, taille and couleur des points
                       # nodes_col                               pour les nouds, respectivement
                       # 
                       # hang_leaves                             accrocher les feuilles
                       # branches_k_color                        colorer les branches
                       # branches_col, branches_lwd ,            Définir la couleur, la largeur de ligne et  
                       # branches_lty                            le type de ligne des branches, 
                       #                                         respectivement
                       # 
                       # by_labels_branches_col,                 Définir la couleur, la largeur de ligne et
                       # by_labels_branches_lwd and              le type de ligne de branches avec
                       # by_labels_branches_lty                  leurs labels spécifique, respectivement
                       # 
                       # 
                       # clear_branches and                      effacer les Branches et feuilles ,
                       # clear_leaves                            respectivement
                       
                       
                       # exemple
                       library(dendextend)
                       # 1. Créer un dendrogramme personnalisé
                       mycols <- c("blue", "#00AFBB", "#E7B800", "#FC4E07")
                       dend <- as.dendrogram(hc) %>%
                         set("branches_lwd", 1) %>% # Largeur de ligne des branches
                         set("branches_k_color", mycols, k = 4) %>% # Colorer les branches par groupe
                         set("labels_colors", mycols, k = 4) %>% # Colorer les  labels par groupe
                         set("labels_cex", 0.5) # Changer la taille des labels
                       
                       # 2. afficher le graph
                       fviz_dend(dend)
                       
                       
 ### heatmap
                       # Une carte thermique  est une autre façon de visualiser la classification hiérarchique. 
                       # c'est également appelée une image de fausse couleur, où les valeurs de données sont transformées en échelle de couleur.
                       # Les cartes de chaleur nous permettent de visualiser simultanément des grappes d'échantillons et de caractéristiques. 
                       # La classification hiérarchique est faite à la fois des lignes et des colonnes de la matrice de données.
                       # Les colonnes / lignes de la matrice de données sont réorganisées en fonction de la hiérarchie
                       # résultat de regroupement, en mettant des observations similaires proches les uns des autres. Les blocs  'haut'
                       # et les valeurs "basses" sont adjacentes dans la matrice de données. Enfin, un schéma de couleurs est appliqué pour
                       # la visualisation et la matrice de données. Visualisation de la matrice de données de cette
                       # manière peut aider à trouver les variables qui semblent être caractéristiques de chaque échantillon de grappe.
                       # 
                       # 
                       # Il y a un nombre multiple de package R et de fonctions pour le dessin interactif et
                       # heatmaps statiques, y compris:
                       # . heatmap() [fonction de base R, package stats]: Dessine un heatmap simple
                       # . heatmap.2() [package gplots R]: Dessine un heatmap amélioré 
                       # . pheatmap() [pheatmap R package]: Dessine de jolis heatmaps et fournit plus de 
                       #   contrôle pour changer l'apparence des heatmaps.
                       # . d3heatmap () [package d3heatmap R]: Dessine un heatmap interactif / cliquable
                       # . Heatmap () [package ComplexHeatmap R / Bioconductor]: Dessine, annote et
                       #   arrange des heatmaps complexes (très utile pour l'analyse de données génomiques)                                      
                       # 
                       # Ici, nous commençons par décrire les 5 fonctions R pour dessiner des heatmaps. Ensuite, nous allons
                       # concentrer sur le package ComplexHeatmap, qui fournit une solution flexible pour organiser
                       # et annoter plusieurs heatmaps. Il permet également de visualiser l'association entre
                       # différentes données provenant de différentes sources.
                       # 
                       # Preparation de données
                       df <- scale(mtcars)
                       
                       # utilisation de hetmap()
                       heatmap(x, scale = "row")
                       # . x: une matrice numérique
                       # . scale: un caractère indiquant si les valeurs doivent être centrées et mises à l'échelle
                       # soit la direction de la ligne ou la direction de la colonne, ou aucune. Les valeurs autorisées sont 
                       # c ("row", "column", "none"). La valeur par défaut est "row".
                       
                       # graph par defaut
                       heatmap(df, scale = "none")
                       # Dans le graphique ci-dessus, les valeurs élevées sont en rouge et les valeurs faibles sont en jaune.
                       
                       # Il est possible de spécifier une palette de couleurs en utilisant l'argument col:
                       col<- colorRampPalette(c("red", "white", "blue"))(256)
                       
                       # ou d'utiliser la palette de couleur "RColorBrewer"
                       library("RColorBrewer")
                       col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
                       
                       # En outre, vous pouvez utiliser l'argument RowSideColors et ColSideColors pour annoter
                       # lignes et colonnes, respectivement.
                       
                       # 1. Un nom de palette de couleurs RColorBrewer est utilisé pour changer l'apparence
                       # 2. L'argument RowSideColors et ColSideColors sont utilisés pour annoter des lignes
                       # et colonnes respectivement. Les valeurs attendues pour ces options sont un vecteur
                       # contenant des noms de couleurs spécifiant les classes pour les lignes / colonnes.
                       
                       col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
                       heatmap(df, scale = "none", col = col,
                               RowSideColors = rep(c("blue", "pink"), each = 16),
                               ColSideColors = c(rep("purple", 5), rep("orange", 6)))
                       
                       # Cartes de chaleur améliorées  (hetmap.2())
                       # install.packages("gplots")
                       library("gplots")
                       heatmap.2(df, scale = "none", col = bluered(100),
                                 trace = "none", density.info = "none")
                       
                       # D'autres arguments peuvent être utilisés, notamment:
                       # . labRow, labCol
                       # . hclustfun: hclustfun = fonction (x) hclust (x, méthode = "ward")
                       # Dans le code R ci-dessus, la fonction bluered() [du package gplots] est utilisée pour générer
                       # un ensemble de couleurs variant en douceur. on peut également utiliser le générateur de couleurs suivant
                       # les fonctions:
                       # . colorpanel(n, low, mid, high)
                       # - n: nombre souhaité d'éléments de couleur à générer
                       # - low, mid, high: Couleurs à utiliser pour les valeurs les plus basses, moyennes et élevées.
                       #   mid peut être omis.
                       # . redgreen(n), greenred(n), bluered(n) and redblue(n)
                         
                       # Jolies cartes de chaleur: pheatmap ()
                       # install.packages("pheatmap")
                       library("pheatmap")
                       pheatmap(df, cutree_rows = 4)
                       
                       # Des arguments sont disponibles pour modifier la métrique de cluster par défaut ("euclidean") et
                       # méthode ("complète"). Il est également possible d'annoter des lignes et des colonnes à l'aide des variables de regroupement
                       # 
                       
                       # Cartes de chaleur interactives: d3heatmap ()
                       # install.packages("d3heatmap")
                       library("d3heatmap")
                       d3heatmap(scale(mtcars), colors = "RdYlBu",
                                 k_row = 4, # nombre de groupe dans les lignes
                                 k_col = 2 # nombre de groupe dans les colunnes
                       )
                       # on observent comment des individus et groupes d'individus (lignes) sont lié à des variables (colonnes) ou groupes de variable

                       # La fonction d3heamap() permet de:
                       #   . Placez la souris sur une cellule heatmap d'intérêt pour voir les noms de la ligne et colonne
                       #      ainsi que la valeur correspondante.
                       #   . Sélectionnez une zone pour le zoom. Après avoir zoomé, cliquez à nouveau sur le heatmap pour 
                       #     retourner à l'affichage précédent
                       
                       
    # Amélioration des cartes thermiques en utilisant dendextend
                       #
                       # Le package dendextend peut être utilisé pour améliorer les fonctions d'autres packages. le
                       # Nous allons commencer par définir l'ordre et le
                       # apparence pour les lignes et les colonnes en utilisant dendextend. Ces résultats sont utilisés dans d'autres
                       # fonctions d'autres packages.
                       
                       # L'ordre et l'apparence des lignes et des colonnes peuvent être définis comme suit :
                       library(dendextend)
                       # ordonné par ligne
                       Rowv <- mtcars %>% scale %>% dist %>% hclust %>% as.dendrogram %>%
                         set("branches_k_color", k = 3) %>% set("branches_lwd", 1.2) %>%
                         ladderize
                       # Ordonné par colonne: nous pouvons transposer les données
                       Colv <- mtcars %>% scale %>% t %>% dist %>% hclust %>% as.dendrogram %>%
                         set("branches_k_color", k = 2, value = c("orange", "blue")) %>%
                         set("branches_lwd", 1.2) %>%
                         ladderize
                       
                       # ensuite, on poura utiliser les argument "Rowv" et "Colv" créés ci-dessus dans les fonctions ci-dessous
                       #
                       # utilisé dans heatmap()
                       heatmap(scale(mtcars), Rowv = Rowv, Colv = Colv,
                               scale = "none")
                       #
                       # utilisé dans heatmap.2()
                       library(gplots)
                       # x11()
                       heatmap.2(scale(mtcars), scale = "none", col = bluered(100),
                                 Rowv = Rowv, Colv = Colv,
                                 trace = "none", density.info = "none")
                       #
                       # utilisé dans d3heatmap()
                       library("d3heatmap")
                       d3heatmap(scale(mtcars), colors = "RdBu",
                                 Rowv = Rowv, Colv = Colv)
                       
                       # ComplexHeatmap 
                       # ComplexHeatmap est un package R / bioconducteur, développé par Zuguang Gu, qui
                       # fournit une solution flexible pour organiser et annoter plusieurs heatmaps. Cela permet aussi
                       # de Visualiser l'association entre différentes données provenant de différentes sources.

                       # installation du package
                       # source("https://bioconductor.org/biocLite.R")
                       # biocLite("ComplexHeatmap")
                       
                       # heatmap simple
                       library(ComplexHeatmap)
                       # x11()
                       Heatmap(df,
                               name = "mtcars", #titre de la legend
                               column_title = "Variables", row_title = "Samples",
                               row_names_gp = gpar(fontsize = 7) # taille du libelé des lignes
                       )
                       
                       # Arguments supplémentaires:
                       # 1. show_row_names, show_column_names: indique s'il faut afficher les noms de lignes et de colonnes, respectivement. La valeur par défaut est TRUE
                       # 2. show_row_hclust, show_column_hclust: valeur logique; s'il faut montrer la rangée
                       # et les groupes de colonnes. La valeur par défaut est TRUE
                       # 3. clustering_distance_rows, clustering_distance_columns: statistique pour le clustering:
                       # "Euclidien", "maximum", "manhattan", "canberra", "binaire", "minkowski",
                       # "Pearson", "spearman", "kendall")
                       # 4. clustering_method_rows, clustering_method_columns: méthodes de clustering:
                       # "Ward.D", "ward.D2", "single", "complet", "moyen",. . . (voir? hclust).
                       
                       # Pour spécifier une couleur personnalisée, on utilisera la fonction colorRamp2():
                       
                       library(circlize)
                       # x11()
                       mycols <- colorRamp2(breaks = c(-2, 0, 2),
                                            colors = c("green", "white", "red"))
                       Heatmap(df, name = "mtcars", col = mycols)
                       
                       # on peut aussi utiliser les palettes de couleurs "RColorBrewer"
                       library("circlize")
                       library("RColorBrewer")
                       # x11()
                       Heatmap(df, name = "mtcars",
                               col = colorRamp2(c(-2, 0, 2), brewer.pal(n=3, name="RdBu")))
                       
                       # Nous pouvons également personnaliser l'apparence des dendogrammes en utilisant la fonction
                       # color_branches () [package dendextend]:
                       library(dendextend)
                       row_dend = hclust(dist(df)) # clustering des lignes
                       col_dend = hclust(dist(t(df))) # clustering des  colonnes
                       Heatmap(df, name = "mtcars",
                               row_names_gp = gpar(fontsize = 6.5),
                               cluster_rows = color_branches(row_dend, k = 4),
                               cluster_columns = color_branches(col_dend, k = 2)
                               )
                       
                       # Division de heatmap par lignes
                       # on peut diviser le heatmap en utilisant l'algorithme k-means ou une variable de regroupement.
                       # Il est important d'utiliser la fonction set.seed() lors de l'exécution de k-means afin que 
                       # les résultats obtenus puissentt être reproduits avec précision plus tard.
                       # . Pour diviser le dendrogramme en utilisant k-means, on fait:

                       # division en 2 groupes
                       set.seed(2)
                       x11()
                       Heatmap(df, name="mtcars", km=2) # km pour dire k-mean
                       
                       # Pour diviser par une variable de regroupement, uon tilise l'argument "split". Dans l'exemple suivant
                       # nous allons utiliser les niveaux de la variable facteur cyl [dans le jeu de données mtcars] pour diviser le
                       # heatmap par lignes. Rappelons que la colonne cyl correspond au nombre de cylindres.
                       
                       # divisé selon les modalité (factor) d'une variable (la variable "cyl")
                       Heatmap(df, name = "mtcars", split = mtcars$cyl,
                               row_names_gp = gpar(fontsize = 7))
                       
                       # Notons que, split peut être aussi une trame de données 
                       # dans laquelle on peut diviser les lignes de la carte de chaleur selon différentes combinaisons de niveaux
                       
                       # couper en combinant plusieurs variables
                       Heatmap(df, name ="mtcars",
                               split = data.frame(cyl = mtcars$cyl, am = mtcars$am))
                       
                       # Annotation Heatmap
                       # La classe HeatmapAnnotation est utilisée pour définir une annotation sur une ligne ou une colonne. 
                         
                         #> HeatmapAnnotation(df, name, col, show_legend)
                       
                       # . df: un data.frame avec des noms de colonne
                       # . name: le nom de l'annotation heatmap
                       # . col: une liste de couleurs qui contient le mappage des couleurs sur les colonnes de df
                       # Pour l'exemple ci-dessous, nous allons transposer nos données pour avoir les observations en colonnes
                       # et les variables en lignes.
                       
                       df <- t(df)
                       
                       # Annotation simple
                       # Un vecteur contenant des valeurs discrètes ou continues est utilisé pour annoter des lignes ou des colonnes.
                       # Nous utiliserons les variables qualitatives cyl (levels = "4", "5" et "8") et am (levels = "0")
                       # et "1"), et la variable continue mpg pour annoter les colonnes.
                       # Pour chacune de ces 3 variables, les couleurs personnalisées sont définies comme suit:
  
                       # Annotation data frame
                       annot_df <- data.frame(cyl = mtcars$cyl, am = mtcars$am,
                                              mpg = mtcars$mpg)
                       # Définir les couleurs pour chaque niveau de variables qualitatives
                       # Définir les couleur dégradé selon la variable continue (mpg)
                       col = list(cyl = c("4" = "green", "6" = "gray", "8" = "darkred"),
                                  am = c("0" = "yellow", "1" = "orange"),
                                  mpg = circlize::colorRamp2(c(17, 25),
                                                             c("lightblue", "purple")) )
                       # Créer les annotation du heatmap 
                       ha <- HeatmapAnnotation(annot_df, col = col)
                       # Combiner le heatmap et les annotations
                       Heatmap(df, name = "mtcars",
                               top_annotation = ha)
                       
                       # Il est possible de masquer la légende d'annotation en utilisant l'argument show_legend = FALSE
                       # comme suit:
                       ha <- HeatmapAnnotation(annot_df, col = col, show_legend = FALSE)
                       Heatmap(df, name = "mtcars", top_annotation = ha)
                       
                       # annotation complex:
                       # Dans cette section, nous verrons comment combiner heatmap et quelques graphiques de base pour montrer
                       # la distribution des données. Pour les graphiques d'annotation simples, les fonctions suivantes
                       # peut être utilisé: anno_points(), anno_barplot (), anno_boxplot (), anno_density () et
                       # anno_histogram ().
                       # Un exemple :

                       # Définir quelques graphiques pour afficher la distribution des colonnes
                       .hist = anno_histogram(df, gp = gpar(fill = "lightblue"))
                       .density = anno_density(df, type = "line", gp = gpar(col = "blue"))
                       ha_mix_top = HeatmapAnnotation(hist = .hist, density = .density)
                       # Définir quelques graphiques pour afficher la distribution des lignes
                       .violin = anno_density(df, type = "violin",
                                              gp = gpar(fill = "lightblue"), which = "row")
                       .boxplot = anno_boxplot(df, which = "row")
                       ha_mix_right = HeatmapAnnotation(violin = .violin, bxplt = .boxplot,
                                                        which = "row", width = unit(4, "cm"))
                       # Combiner les annotation avec le heatmap
                       Heatmap(df, name = "mtcars",
                               column_names_gp = gpar(fontsize = 8),
                               top_annotation = ha_mix_top,
                               top_annotation_height = unit(3.8, "cm")) + ha_mix_right
                       
                       
                       # combinaison de plusieurs heatmaps
                       # Plusieurs heatmaps peuvent être combinés comme suit:
                       
                       # Heatmap 1
                       ht1 = Heatmap(df, name = "ht1", km = 2,
                                     column_names_gp = gpar(fontsize = 9))
                       # Heatmap 2
                       ht2 = Heatmap(df, name = "ht2",
                                     col = circlize::colorRamp2(c(-2, 0, 2), c("green", "white", "red")),
                                     column_names_gp = gpar(fontsize = 9))
                       
                       # Combinaison de  2 heatmaps
                       ht1 + ht2
                       
                       # nous pouvons utiliser l'option width = unit (3, "cm")) pour contrôler la taille des heatmaps.
                       # Notons que lors de la combinaison de plusieurs heatmaps, le premier heatmap est considéré comme
                       # la carte de chaleur principale. Certains paramètres des cartes thermiques restantes sont ajustés automatiquement
                       # selon le réglage de la carte de chaleur principale. Cela comprend: l'élimination des grappes de lignes
                       # et les titres puis en ajoutant le fractionnement.

                       # La fonction draw () peut être utilisée pour personnaliser l'apparence de l'image finale:
                       draw(ht1 + ht2,
                            row_title = "Two heatmaps, row title",
                            row_title_gp = gpar(col = "red"),
                            column_title = "Two heatmaps, column title",
                            column_title_side = "bottom",
                            # Gap between heatmaps
                            gap = unit(0.5, "cm"))
                       
                       # Les légendes peuvent être supprimées en utilisant les arguments show_heatmap_legend = FALSE et
                       # show_annotation_legend = FAUX.
                       
                       
                       # Application à la matrice d'expression génique
                       # Dans les données d'expression génique, les lignes sont des gènes et les colonnes sont des échantillons. Plus d'information
                       # à propos des gènes peuvent être percus après la carte de chaleur d'expression tels que la longueur du gène et
                       # type de gènes.
                       
                       expr <- readRDS(paste0(system.file(package = "ComplexHeatmap"),
                                              "/extdata/gene_expression.rds"))
                       mat <- as.matrix(expr[, grep("cell", colnames(expr))])
                       type <- gsub("s\\d+_", "", colnames(mat))
                       ha = HeatmapAnnotation(df = data.frame(type = type))
                       Heatmap(mat, name = "expression", km = 5, top_annotation = ha,
                               top_annotation_height = unit(4, "mm"),
                               show_row_names = FALSE, show_column_names = FALSE) +
                         Heatmap(expr$length, name = "length", width = unit(5, "mm"),
                                 col = circlize::colorRamp2(c(0, 100000), c("white", "orange"))) +
                         Heatmap(expr$type, name = "type", width = unit(5, "mm")) +
                         Heatmap(expr$chr, name = "chr", width = unit(5, "mm"),
                                 col = circlize::rand_color(length(unique(expr$chr))))
                       
                       # Il est également possible de visualiser les altérations génomiques et d'intégrer différentes niveaux de molécules
                       # (expression génique, méthylation de l'ADN, ...). 
                       
                       
          #### IV : validation de cluster
                       # La validation de cluster consiste à mesurer la qualité des résultats de clustering.
                       # Avant d'appliquer un algorithme de clustering à un ensemble de données, la première chose à faire est de
                       # évaluer la tendance au regroupement. Autrement dit, si l'application de la classification est appropriée pour
                       # ces données. Si oui, alors combien y a-t-il de grappes? Ensuite, on pourra effectuer un clusturing hiérarchique
                       #  ou de partitionnement en cluster (avec un nombre prédéfini de clusters). Finalement,
                       # on purra utiliser un certain nombre de mesures, décrites dans cette partie, pour évaluer la qualité 
                       # des résultats du regroupement.
                       # Procédure :
                       # . 1:Évaluer la tendance au regroupement 
                       # . 2:Déterminer le nombre optimal de grappes 
                       # . 3:Statistiques de validation des grappes 
                       # . 4:Choisir les meilleurs algorithmes de clustering 
                       # . 5:Calcul de la valeur p pour la classification hiérarchique 
                       # 
                       # ### 1: Évaluer la tendance au regroupement
                       # Avant d'appliquer une méthode de clustering à des données, il est important d'évaluer si
                       # les ensembles de données contiennent des groupes significatifs (c'est-à-dire des structures non aléatoires) ou non. 
                       # Si oui, alors combien de grappes sont là. 
                       #                                                                 
                       # Ce processus est défini comme l'évaluation de la tendance à la concentration ou la faisabilité de l'analyse de regroupement.
                       # Un gros problème, dans l'analyse des clusters, est que les méthodes de clusters ramèneront les  clusters même
                       # si les données ne contiennent en réalité aucun cluster. En d'autres termes, si vous appliquez aveuglément une
                       # méthode de clustering sur un ensemble de données, il va diviser les données en grappes parce que c'est
                       # ce qu'il est censé faire.
                       # Dans cette partie, nous commençons par décrire pourquoi nous devrions évaluer la tendance au regroupement
                       # avant d'appliquer une méthode de clustering sur une donnée. Ensuite, nous fournissons des statistiques et
                       # méthodes visuelles pour évaluer la tendance à la classification.
                       #                            
                       # 
                       # # nous allons utiliser le package  "clustertend" pour évaluation les statistiques à la tendance au clustering 
                       # # install.packages(c("factoextra", "clustertend"))
                       # 
                       # Nous utiliserons deux ensembles de données:
                       # . le jeu de paramètres R intégré.
                       # . et un ensemble de données aléatoires généré à partir de l'ensemble de données iris.
                       # 
                       # base1: Ensembles de données de l'iris :
                       head(iris, 3)
                       # Nous commençons par exclure la colonne "Species" à la position 5
                       df <- iris[, -5]
                       
                       # base2: Données aléatoires générées à partir de l'ensemble de données iris
                       random_df <- apply(df, 2,
                                          function(x){runif(length(x), min(x), (max(x)))})
                       random_df <- as.data.frame(random_df)
                       
                       # Standardisationdes deux bases
                       df <- iris.scaled <- scale(df)
                       random_df <- scale(random_df)
                       
                       
                       # Inspection visuelle des données
                       # Nous commençons par visualiser les données pour évaluer si elles contiennent un sens pour un regroupement.
                       #  
                       # Comme les données contiennent plus de deux variables, nous devons réduire les dimensions
                       # afin de tracer un nuage de points. Cela peut être fait en utilisant l'analyse des composantes principales
                       # Algorithme (PCA) (fonction R: prcomp ()). Après avoir effectué PCA, nous utilisons la fonction
                       # fviz_pca_ind() [paquet R factoextra] pour visualiser la sortie.
                       # L'iris et les ensembles de données aléatoires peuvent être illustrés comme suit:
                         
                       library("factoextra")
                       # graph des iri standardisé (df)
                       # x11()
                       fviz_pca_ind(prcomp(df), title = "PCA - Iris data",
                                    habillage = iris$Species, palette = "jco",
                                    geom = "point", ggtheme = theme_classic(),
                                    legend = "bottom")
                       # graph de la base des données aléatoire standardisées (random_df)
                       fviz_pca_ind(prcomp(random_df), title = "PCA - Random data",
                                    geom = "point", ggtheme = theme_classic())
                       
                       
                       # On peut voir que l'ensemble de données de l'iris contient 3 véritables grappes. 
                       # Cependant, l'ensemble de données aléatoire généré ne contient aucun cluster significatif.
                       
                       # Pourquoi évaluer la tendance au regroupement?
                       #   Afin d'illustrer l'important de l'évaluation de la tendance des grappes, nous commençons par
                       # calculer le clustering k-means et le clustering hiérarchique sur
                       # les deux ensembles de données (les données réelles et aléatoires). La fonction fviz_cluster() et
                       # fviz_dend() [du package R factoextra] sera utilisé pour visualiser les résultats.
                       
                       library(factoextra)
                       set.seed(123)
                       # K-means sur les données iris (df)
                       km.res1 <- kmeans(df, 3)
                       fviz_cluster(list(data = df, cluster = km.res1$cluster),
                                    ellipse.type = "norm", geom = "point", stand = FALSE,
                                    palette = "jco", ggtheme = theme_classic())
                       
                       
                       
                       # K-means sur les données aléatoires (random_df)
                       km.res2 <- kmeans(random_df, 3)
                       fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
                                    ellipse.type = "norm", geom = "point", stand = FALSE,
                                    palette = "jco", ggtheme = theme_classic())
                       
                       # clustering Hierarchique  sur les données aléatoires (random_df)
                       fviz_dend(hclust(dist(random_df)), k = 3, k_colors = "jco",
                                 as.ggplot = TRUE, show_labels = FALSE)
                       
                       # On peut voir que l'algorithme k-means et le clustering hiérarchique imposent
                       # une classification sur l'ensemble de données aléatoires uniformément réparties même s'il n'y a pas
                       # grappes significatives présentes en elle. C'est pourquoi, en regroupement,  les méthodes d'évaluation de la tendance
                       # devrait être utilisé pour évaluer la validité de l'analyse de classification. Autrement dit, si un
                       # Un ensemble de données donné contient des groupes significatifs ou pas avant de faire le clustering.

                       
                       # Méthodes d'évaluation de la tendance au regroupement
                       # Dans cette section, nous décrirons deux méthodes pour évaluer la tendance à la classification: en (a1) on verra la
                       # statistique (statistique de Hopkins) et en (a2) on verra une méthode visuelle (évaluation visuelle de la grappe
                       # Tendance (VAT) algorithme).
                       
                       # a1) Méthodes statistiques
                       # La statistique Hopkins est utilisée pour évaluer la tendance à la classification d'un ensemble de données en
                       # mesurant la probabilité qu'un ensemble de données donné soit généré par une donnée de distribution uniforme.
                       #  En d'autres termes, il teste la répartition spatial l'aléatoire  des données.
                       # Par exemple, soit D soit un ensemble de données réel. La statistique Hopkins peut être calculée comme
                       # suivre:
                       
                       # 1. Echantillonner uniformément n points (p1, ..., pn) de D.
                       # 2. Pour chaque point pi ??? D, trouver son plus proche voisin pj; puis calculer la
                       # distance entre pi et pj et l'indique xi = dist (pi, pj)
                       # 3. Générer un ensemble de données simulé (randomD) tiré d'une distribution uniforme 
                       #  avec n points aléatoire (q1, ..., qn) et la même variation que l'ensemble réel de données d'origine D.
                       # 3. Pour chaque point qi ??? randomD ( ensemble aléatoire de D), trouve son voisin le plus proche qj dans D; puis
                       # Calculez la distance entre qi et qj et notez yi = dist (qi, qj)
                       # 4. Calculer la statistique de Hopkins (H) en tant que distance moyenne du plus proche voisin
                       # dans l'ensemble de données aléatoires divisé par la somme des distances moyennes du plus proche voisin 
                       #  dans le réel et à travers l'ensemble de données simulé.
                       # 
                       # Une valeur de H d'environ 0,5 signifie que la somme de yi et la somme des xi sont proches les uns des autres, et donc
                       # la donnée D est uniformément distribuée.
                       # Les hypothèses null et alternative sont définies comme suit:
                       # . Hypothèse nulle: l'ensemble de données D est distribué uniformément (c'est-à-dire sans grappes signification)
                       #                                                                        
                       # . Hypothèse alternative: l'ensemble de données D n'est pas réparti uniformément (c.-à-d. contient des grappes significatives)
                       # 
                       # Si la valeur de la statistique Hopkins est proche de zéro, alors nous pouvons rejeter l'hypothèse nulle
                       # et conclure que l'ensemble de données D est significativement une donnée regroupable.
                       # La fonction R hopkins() [du package clustertend] peut être utilisée pour évaluer statistiquement
                       # la tendance de clustering dans R. 

                         #>hopkins(data, n)
                       # . data: data.frame ou  matrice
                       # . n: le nombre de points à sélectionner à partir des données
                       
                       # install.packages("clustertend")
                       library(clustertend)
                       
                       # Calcul de la statistique Hopkins pour le jeu de données iris (base1)
                       set.seed(123)
                       hopkins(df, n = nrow(df)-1)
                       
                       
                       # Calcul de la statistique Hopkins pour le jeu de données généré aléatoirement de iris (base2)
                       set.seed(123)
                       hopkins(random_df, n = nrow(random_df)-1)
                       
                       # On peut voir que l'ensemble de données de l'iris est hautement regroupable (la valeur H = 0,18 est proche de 0 et loin en dessous du seuil 0.5 ).
                       # Cependant, l'ensemble de données random_df n'est pas regroupable (car sa valeur H = 0,48 est proche de 0.5).
                       
                       # a2) Méthodes visuelles
                       # L'algorithme de l'évaluation visuelle de l'approche de la tendance cluster (VAT) (Bezdek et Hathaway, 2002) est la suivante:
                       # Algorithme VAT
                       # 1. Calculez la matrice de dissimilarité (DM) entre les objets de l'ensemble de données en utilisant
                       # la mesure de la distance euclidienne 
                       # 2. Réorganisez le DM pour que les objets similaires soient proches les uns des autres.
                       # ce processus créer une matrice de dissimilarité ordonnée (ODM)
                       # 3. L'ODM est affiché comme une image de dissimilarité ordonnée (ODI), qui est la sortie visuelle du VAT
                       # 
                       # Pour l'évaluation visuelle de la tendance à la classification, nous commençons par calculer la matrice de dissimilarité
                       #  entre les observations en utilisant la fonction dist(). Ensuite, la fonction fviz_dist ()
                       # [package factoextra] est utilisé pour afficher la matrice de dissimilarité.
                       
                       fviz_dist(dist(df), show_labels = FALSE)+
                         labs(title = "Iris data")
                       fviz_dist(dist(random_df), show_labels = FALSE)+
                         labs(title = "Random data")
                       
                       # Rouge: haute similarité (ie: faible dissimilarité) | Bleu: faible similarité (ie: haute dissimilarité)
                       # Le niveau de couleur est proportionnel à la valeur de la dissemblance entre les observations:
                       #   rouge pur si dist(xi, xj) = 0 et bleu pur si dist(xi, xj) = 1. les Objets appartenant au
                       # même cluster sont affichés dans l'ordre consécutif.
                       # L'image de la matrice de dissimilarité confirme qu'il existe une structure de cluster dans l'ensemble de données iris (base1) (on remarque bien des zone de regroupement de bleu et de regroupement de rouge)
                       #  mais pas dans le fichier aléatoire (base2) (on ne remarque pas de zone de regroupement de bleu ou de regroupement de rouge, les deux sont dispersé un peut partou aleatoirement).
                       
                       # La VAT détecte la tendance au regroupement sous une forme visuelle en comptant le nombre de
                       # blocs foncés en forme de carré le long de la diagonale dans une image de VAT.
                       
                       # Résumé
                       # Dans cette partie, nous avons décrit comment évaluer la tendance à la classification en utilisant les Hopkins
                       # statistiques et une méthode visuelle. Après avoir montré qu'une donnée peut être regroupée, l'étape suivante
                       # est de déterminer le nombre de clusters optimaux dans les données. Cela sera décrit dans
                       # l'étape suivant.

    # Déterminer le nombre optimal de grappes
                       
                      #  La détermination du nombre optimal de clusters dans un ensemble de données est un
                      #  problème dans le clustering de partitionnement, tel que le clustering k-means, qui nécessite que
                      #  l'utilisateur  spécifie le nombre de grappes k à générer.
                      #  Malheureusement, il n'y a pas de réponse définitive à cette question. Le nombre optimal
                      #  des grappes est en quelque sorte subjective et dépend de la méthode utilisée pour mesurer
                      #  les similitudes et les paramètres utilisés pour le partitionnement.
                      #  
                      #  Une solution simple et populaire consiste à inspecter le dendrogramme produit en utilisant
                      #  clustering hiérarchique  pour voir s'il suggère un nombre particulier de clusters.
                      #  
                      #  Malheureusement, cette approche est également subjective.
                      #  Dans cette partie, nous allons décrire différentes méthodes pour déterminer le nombre optimal
                      #  des clusters pour k-means, k-medoids (PAM) et clustering hiérarchique.
                      #  Ces méthodes incluent des méthodes directes et des méthodes de tests statistiques:
                      #  
                      #  1. Méthodes directes: consiste à optimiser un critère, tel que le cluster intra
                      #  des sommes de carrés ou la silhouette moyenne. Les méthodes correspondantes sont nommées
                      #  méthodes de coude et de silhouette, respectivement.
                      #  
                      #  2. Méthodes d'essais statistiques: consiste à comparer les resultats à l'hypothèse nulle.
                      #  Un exemple est la statistique de l'écart.
                      #  
                      #  En plus des méthodes statistiques de coude, de silhouette et d'écart, il y a plus de trente
                      #  d'autres indices et méthodes qui ont été publiés pour identifier le nombre optimal de grappes. Nous allons fournir des codes R pour calculer ces 30 indices afin de décider
                      #  le meilleur nombre de grappes utilisant la "règle de la majorité".
                      #  Pour chacune de ces méthodes:
                      #  . Nous allons décrire l'idée de base et l'algorithme
                      #  . Nous fournirons des codes R faciles à utiliser avec de nombreux exemples pour déterminer le
                      #  nombre optimal de clusters et visualisation de la sortie.
                      #  
                      #  
                      #  
                      #  a1) elbow method (Méthode du coude)
                      #  Rappelons que, l'idée de base derrière les méthodes de partitionnement, tels que le clustering k-means
                      #  est de définir des clusters tels que la variation totale intra-cluster [ou total
                      #  La somme des carrés (WSS) intra-cluster soit minimisée. Le total WSS mesure la
                      #  compacité du clustering et nous voulons qu'il soit aussi petit que possible.
                      # 
                      #  La méthode Elbow (coud) considère le WSS total en fonction du nombre de clusters:
                      #  On devrait choisir un certain nombre de clusters de sorte que l'ajout d'un autre cluster n'améliore pas
                      #  beaucoup  le total WSS.
                      #  Le nombre optimal de clusters peut être défini comme suit:
                      #  1. Calculer l'algorithme de clustering (par exemple, k-means clustering) pour différentes valeurs de k.
                      #  Par exemple, en faisant varier k de 1 à 10 grappes.
                      #  2. Pour chaque k, calculer la somme totale du carré (wss).
                      #  3. Tracer la courbe de wss en fonction du nombre de grappes k.
                      #  4. L'emplacement d'un virage (genou ou coud) dans la parcelle est généralement considéré comme
                      #  indicateur du nombre approprié de grappes.
                      # 
                      #  Notez que la méthode du coude est parfois ambiguë. Une alternative est la 
                      #  méthode de silhouette moyenne (Kaufman et Rousseeuw [1990]) qui peut aussi être utilisée avec
                      #  toute approche de regroupement.
                      # 
                      #  a2) Méthode de silhouette moyenne
                      #  L'approche de la silhouette moyenne sera décrite de manière exhaustive plus bas dans la partie 
                      #  statistiques de validation des clusters. En bref, il mesure la qualité d'un regroupement.
                      #  C'est-à-dire qu'il détermine à quel point de chaque objet se trouve dans son groupe. Une moyenne élevée
                      #  de la largeur de la silhouette indique un bon regroupement.
                      #  La méthode de la silhouette moyenne calcule la silhouette moyenne des observations pour différentes
                      #  valeurs de k. Le nombre optimal de clusters k est celui qui maximise la 
                      #  silhouette moyenne sur une gamme de valeurs possibles pour k (Kaufman et Rousseeuw [1990]).
                      #  L'algorithme est similaire à la méthode elbow et peut être calculé comme suit:
                      #   1. Calculer l'algorithme de clustering (par exemple, k-means clustering) pour différentes valeurs de k.
                      #  Par exemple, en faisant varier k de 1 à 10 grappes.
                      #  2. Pour chaque k, calculez la silhouette moyenne des observations (avg.sil).
                      #  3. Tracer la courbe de avg.sil en fonction du nombre de grappes k.
                      #  4. L'emplacement du maximum est considéré comme le nombre approprié
                      #   des grappes.
                      # 
                      # 
                      # a3) Méthode statistique de l'écart
                      #  La statistique de l'écart a été publiée par R. Tibshirani, G. Walther et T. Hastie
                      #   (Université de Standford, 2001). L'approche peut être appliquée à n'importe quelle méthode de clustering.
                      #   La statistique d'écart compare le total dans la variation intra-cluster pour différentes valeurs
                      #  de k avec leurs valeurs attendues sous la distribution de référence nulle des données. 
                      #  l'estimation des groupes optimaux sera la valeur qui maximise la statistique d'écart 
                      #  (c.-à-d.  donne la plus grande statistique d'écart). Cela signifie que la structure de regroupement est loin de la distribution uniforme aléatoire des points.
                      #   L'algorithme fonctionne comme suit:
                      #   1. Regrouper les données observées, en faisant varier le nombre de grappes de k = 1, ..., kmax,
                      #   et calculer le total correspondant dans la variation intra-cluster Wk.
                      #   2. Générer des ensembles de données de référence B avec une distribution uniforme aléatoire. Cluster chaque
                      #   de ces ensembles de données de référence avec un nombre variable de groupes k = 1, ..., kmax, et
                      #   calculer le total correspondant dans la variation intra-cluster Wkb.
                      #   3. Calculer la statistique d'écart estimée en tant que déviation de la valeur Wk observée
                      #   sa valeur attendue Wkb sous l'hypothèse nulle: Gap (k) = ...
                      #                                                                   
                      #   Calculer également l'écart-type des statistiques.
                      #   4. Choisir le nombre de clusters comme la plus petite valeur de k telle que la statistique d'écart
                      #   est dans un écart-type de l'écart à k + 1.
                      #                                                                   
                      #   Notons que l'utilisation de B = 500 donne des résultats assez précis de sorte que le tracé de l'écart est fondamentalement
                      #   inchangé après une autre course.               
                      #                         
                      #  
                      # a4) Calculer le nombre de clusters en utilisant R
                      # Dans cette section, nous décrirons deux fonctions pour déterminer le nombre optimal de grappes:
                      # 1. Fonction fviz_nbclust() [du package R factoextra]: Elle peut être utilisée pour calculer
                      # trois méthodes différentes [coude, silhouette et statistique d'écart] pour toutes méthodes de partitionnement
                      # ou de regroupement [K-means, K-medoids (PAM), CLARA, HCUT]. 
                      # Notons que la fonction hcut() est disponible uniquement dans le package factoextra. Elle calcule le regroupement hiérarchique
                      # et coupe l'arbre en k grappes pré-spécifiées.
                      # 2. Fonction NbClust() [dans le package NbClust R] (Charrad et al., 2014): Elle fournit
                      # 30 indices pour déterminer le nombre de clusters et propose aux utilisateurs
                      # le meilleur schéma de regroupement à partir des différents résultats obtenus en variant toutes
                      # combinaisons de nombre de groupes, de mesures de distance et de méthodes de regroupement.
                      # Il peut calculer simultanément tous les indices et déterminer le nombre de
                      # clusters dans un appel de fonction unique.
                      # 
                      # Nous utiliserons les paquets R suivants:
                      # . factoextra pour déterminer les grappes numériques optimales pour un méthode declustering donné
                      #  et pour la visualisation de données.
                      # . NbClust pour calculer environ 30 méthodes à la fois, afin de trouver le meilleur
                      # nombre de grappes.

                       # installation des packages
                       # pkgs <- c("factoextra", "NbClust")
                       # install.packages(pkgs)
                       # 
                       # install.packages("NbClust")
                       library(factoextra)
                       library(NbClust)
                       
                       
                       # Preparation des données et Standardisation 
                       df <- scale(USArrests)
                       head(df)
                       
                       # Fonction fviz_nbclust (): Elbow, Silhouhette et Gap
                       # méthodes statistiques
                       
                        #>fviz_nbclust(x, FUNcluster, method = c("silhouette", "wss", "gap_stat"))
                       # . x: matrice numérique ou data.frame
                       # . FUNcluster: une fonction de partitionnement. Les valeurs autorisées incluent kmeans, pam,
                       # clara et hcut (pour la classification hiérarchique).
                       # . méthode: la méthode à utiliser pour déterminer le nombre optimal de grappes.
                       
                       # ci-dessous déterminons le nombre optimal de clusters pour le clustering k-means:
                       # Elbow (coud) method # ici on a spécifié manuellement intercept=4 en visualisant d'abord le graph sans le intercept 
                       fviz_nbclust(df, kmeans, method = "wss") +
                         geom_vline(xintercept = 4, linetype = 2)+
                         labs(subtitle = "Elbow method")
                       #  methode de Silhouette # ici, la méthode elle-même nous à suggéré l'intercept à 2 automatiquement
                       fviz_nbclust(df, kmeans, method = "silhouette")+
                         labs(subtitle = "Silhouette method")
                       
                       # Gap statistiques
                       # nboot = 50 pour que ca tourne vite sinon, pour un travail fin, on aurai choisi nboot=500.
                       # valeur recommandé: nboot= 500 pour les analyses.
                       # verbose = FALSE si on veut masquer la barre de progression.
                       set.seed(123)
                       fviz_nbclust(df, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
                         labs(subtitle = "Gap statistic method")
                       # le programme nous a suggéré automatiquement 4 classes
                       # - Méthode du coude: solution de 4 clusters proposée
                       # - Méthode de la silhouette: solution de 2 clusters proposée
                       # - Méthode statistique Gap: solution de 4 clusters proposée
                       
                       # D'après ces observations, il est possible de définir k = 4 comme le nombre optimal
                       # de grappes dans les données.
                       # L'inconvénient des méthodes de coude et de silhouette moyenne est qu'ils mesurent
                       # une caractéristique globale de clustering uniquement. Une méthode plus sophistiquée consiste à utiliser
                       # les statistique d'écart qui fournit une procédure statistique pour formaliser le coude / silhouette
                       # heuristique afin d'estimer le nombre optimal de grappes.
                       
                       # Fonction NbClust(): 30 indices pour choisir le meilleur
                       # nombre de grappes
                       # Le format simplifié de la fonction NbClust () est:
                         
                        #>NbClust(data = NULL, diss = NULL, distance = "euclidean",
                        #>      min.nc = 2, max.nc = 15, method = NULL)
                       
                       # . data: matrice de données
                       # . diss: matrice de dissimilarité à utiliser. Par défaut, diss = NULL, mais si c'est
                       # remplacé par une matrice de dissimilarité, la distance devrait être "NULL"
                       # . distance: la mesure de distance à utiliser pour calculer la matrice de dissimilarité.
                       # Les valeurs possibles incluent "euclidean", "manhattan" ou "NULL".
                       # . min.nc, max.nc: nombre minimal et maximal de grappes, respectivement
                       # . méthode: la méthode d'analyse de cluster à utiliser incluant "ward.D", "ward.D2",
                       # "Single", "complete", "average", "kmeans" et plus... .
                       # . Pour calculer NbClust() pour kmeans, utiliser method = "kmeans".
                       # . Pour calculer NbClust() pour le clustering hiérarchique, la méthode doit être l'une des
                       # c («ward.D», «ward.D2», «single», «complete», «average»).
                       
                       # ci-dessous calcule du NbClust() pour k-means:
                        library("NbClust")
                        nb <- NbClust(df, distance = "euclidean", min.nc = 2,
                                      max.nc = 10, method = "kmeans")
                       
                       # Le résultat de NbClust en utilisant la fonction fviz_nbclust() [package factoextra]:
                       library("factoextra")
                       fviz_nbclust(nb)
                       # interprètation des sortie et du graph:
                       # - 2 proposé 0 comme le meilleur nombre de grappes
                       # - 10 indices ont proposé 2 comme le meilleur nombre de grappes.
                       # - 2 ont proposé 3 comme le meilleur nombre de grappes.
                       # - 8 ont proposé 4 comme le meilleur nombre de grappes.
                       # Selon la règle de la majorité, le meilleur nombre de groupes est 2.
                       
                       # resumé:
                       # Dans cet article, nous avons décrit différentes méthodes pour choisir le nombre optimal de
                       # grappes dans un ensemble de données. Ces méthodes comprennent le coude, la silhouette et l'écart
                       # méthodes statistiques
                       # Nous avons démontré comment calculer ces méthodes en utilisant la fonction R fviz_nbclust ()
                       # [dans le package R factoextra]. En outre, nous avons décrit le paquet NbClust (), qui peut
                       # être utilisé pour calculer simultanément de nombreux autres indices et méthodes pour déterminer
                       # le nombre de grappes.
                       # Après avoir choisi le nombre de clusters k, l'étape suivante consiste à effectuer le partitionnement
                       # regroupement tel que décrit au: k-means clustering.
                       
  ### Statistiques de validation de cluster
                       # Le terme validation de grappe est utilisé pour concevoir la procédure d'évaluation de
                       # la qualité des résultats de l'algorithme de clustering. Ceci est important pour éviter de trouver des modèles dans
                       # une donnée aléatoire, ainsi que, dans la situation où vous voulez comparer deux  algorithmes de clusters.
                       # 
                       # Généralement, les statistiques de validation de classification peuvent être classées 
                       # en 3 classes (Theodoridis et Koutroubas, 2008; G. Brock et al., 2008, Charrad et al., 2014):
                       # 
                       # 1. Validation de cluster interne, qui utilise les informations internes du processus clustering
                       #  pour évaluer la qualité d'une structure de regroupement sans référence
                       # à l'information externe. Il peut également être utilisé pour estimer le nombre de clusters
                       # et l'algorithme de clustering approprié sans données externes.
                       # 
                       # 2. La validation de cluster externe, qui consiste à comparer les résultats d'une 
                       # analyse de cluster à un résultat connu à l'extérieur, comme une étiquette de classe provenant de l'extérieur.
                       #  Il determine la mesure dans laquelle les étiquettes de cluster correspondent
                       # aux étiquettes de classe. Puisque nous connaissons à l'avance le «vrai» numéro (nombre) de grappe, cette approche
                       # est principalement utilisé pour sélectionner le bon algorithme de clustering pour un ensemble de données spécifique.
                       # 
                       # 3. Validation de cluster relative, qui évalue la structure de clustering en faisant varier
                       # différentes valeurs de paramètres pour le même algorithme (par exemple,: variation du nombre
                       # des groupes k). Il est généralement utilisé pour déterminer le nombre optimal de clusters.
                       # 
                       # Dans cette partie, nous commençons par décrire les différentes méthodes de validation des clusters.
                       # Ensuite, nous allons montrer comment comparer la qualité des résultats de clustering obtenus
                       # avec différents algorithmes de clustering. 
                       # Enfin, nous allons réaliser un exemple pour la validation des résultats de la classification.
                       # 
                       # Dans tous les exemples présentés ici, nous appliquerons k-means, PAM et le clustering hiérarchique.
                       # Notons que les fonctions utilisées dans cet article peuvent être appliquées pour évaluer
                       # la validité de toutes les autres méthodes de classification.
                       
            ### Mesures internes pour la validation de cluster
                       # Dans cette section, nous décrivons les indices de validation de clustering les plus utilisés. Rappellons
                       # que le but de partitionner les algorithmes de clustering  est de diviser l'ensemble de données
                       # en groupes d'objets, tels que:
                       # . les objets d'un même cluster sont similaires autant que possible,
                       # . et les objets dans différents groupes sont très distincts
                       # Autrement dit, nous voulons que la distance moyenne au sein du cluster soit aussi faible que possible; et
                       # la distance moyenne entre les grappes doit être aussi grande que possible.
                       # Les mesures de validation internes reflètent souvent la compacité, la connectivité et
                       # la séparation des partitions de cluster.
                       # 1. Compacité ou cohésion de la grappe: Mesure dans quelle mesure les objets d'un même cluster sont proches.
                       #  Une variation intra-cluster plus faible est un indicateur d'une bonne
                       #  compacité (c'est-à-dire un bon regroupement). Les différents indices pour évaluer 
                       # la compacité des clusters sont basées sur des mesures de distance telles que le cluster au sein de
                       # la distances moyennes / médianes entre les observations.
                       # 2. Séparation: determine dans quelle mesure un cluster est bien séparé des autres clusters.
                       # Les indices utilisés comme mesures de séparation comprennent:
                       # - les distances entre les centres de cluster
                       # - les distances minimales par paires entre objets dans différents groupes
                       # 3. Connectivité: correspond à la mesure dans laquelle les éléments sont placés dans le même
                       # cluster comme leurs plus proches voisins dans l'espace de données. La connectivité a une valeur
                       # entre 0 et l'infini et devrait être minimisé.
                       # En règle générale, la plupart des indices utilisés pour la validation de la classification interne combinent la compacité
                       # et mesures de séparation.
                       # 
                       # Dans cette section, nous décrirons les deux indices couramment utilisés pour évaluer
                       # la qualité de clustering: la largeur de la silhouette et l'indice de Dunn. 
                       # La mesure interne peut également être utilisée pour déterminer le nombre optimal de grappes dans les données.
                       # 
                       # #a1) Coefficient de la silhouette
                       # L'analyse de la silhouette mesure la qualité du regroupement d'une observation et estime
                       # la distance moyenne entre les groupes. Le tracé de la silhouette affiche ou mesure 
                       # à quel point chaque point d'un cluster est proche de points dans les groupes voisins.
                       # Pour chaque observation i, la largeur de silhouette s_i est calculée comme suit:
                       # 1. Pour chaque observation i, calculer la dissimilarité moyenne a_i entre i et tous
                       # les autres points du cluster auquel appartient i.
                       # 2. Pour tous les autres groupes C, auxquels je n'appartiens pas, calculer la dissemblance moyenne
                       #  d(i, C) de i à toutes les observations de C. Le plus petit de ces d(i, C)
                       # est défini comme bi = minC(d(i, C)). La valeur de bi peut être vue comme la dissemblance
                       # entre i et son groupe "voisin", c'est-à-dire le plus proche auquel il n'e correspond'appartient pas.
                       # 3. Enfin, la largeur de la silhouette de l'observation i est définie par la formule:
                       #   Si = (bi - ai) / max (ai, bi).
                       # La largeur de la silhouette peut être interprétée comme suit:
                       #   - Les observations avec un grand S_i (presque egale à 1) sont très bien groupées.
                       #   - Un petit S_i (autour de 0) signifie que l'observation se situe entre deux groupes.
                       #   - Les observations avec un S_i négatif sont probablement placées c'est à dire sont dans le mauvais groupe.
                       # 
                       # # a2) Dunn index 
                       # L'indice de Dunn est une autre mesure de validation de regroupement interne qui peut être
                       # calculé comme suit:
                       # 1. Pour chaque cluster, calculez la distance entre chacun des objets du cluster
                       # et les objets dans les autres groupes
                       # 2. Utilisez le minimum de cette distance par paire comme séparation inter-cluster
                       # (min.separation)
                       # 3. Pour chaque cluster, calculez la distance entre les objets d'un même cluster.
                       # 4. Utilisez la distance intra-cluster maximale (c'est-à-dire le diamètre maximal) comme compacité intracluster
                       # 5. Calculez l'indice de Dunn (D) comme suit: D = min.separation / max.diameter
                       # Si l'ensemble de données contient des clusters compacts et bien séparés, le diamètre 
                       # des grappes devraient être petites et la distance entre les grappes devrait
                       # être grand. Ainsi, l'indice de Dunn devrait être maximisé.
                       # 
                       # 
                       # #a3) Mesures externes pour la validation de la mise en grappe
                       # L'objectif est de comparer les clusters identifiés (par k-means, pam ou clustering hiérarchique ) à une référence externe.
                       # Il est possible de quantifier l'accord entre les clusters de partitionnement et la référence externe
                       #  en utilisant soit l'indice de Rand corrigé et l'indice de variation de Meila VI, qui
                       # sont implémentés dans la fonction R cluster.stats() [package fpc].
                       # L'indice Rand corrigé varie de -1 (pas d'accord) à 1 (accord parfait).
                       # La Validation de clustering externe, peut être utilisé pour sélectionner un algorithme de clustering approprié pour
                       # un ensemble de données donné.
                       # 
                       # exemple:  Calcul des statistiques de validation des grappes 
                       # Installation des packages:
                       # install.packages(c("factoextra", "fpc", "NbClust"))
                       # install.packages("fpc")
                       library(factoextra)
                       library(fpc)
                       library(NbClust)
                       
                       # preparation des données
                       # on enlève la colonne "Species" à lat position 5
                       df <- iris[, -5]
                       
                       # Standardisation
                       df <- scale(df)
                       
                  ## Analyse de clustering
                       # Nous allons utiliser la fonction eclust() [clustering amélioré, dans factoextra] qui fournit
                       # plusieurs avantages:
                       # . Il simplifie le flux de travail de l'analyse de clustering
                       # . Il peut être utilisé pour calculer le clustering hiérarchique et le clusturing de partitionnement dans
                       # une appel de fonction en une ligne.
                       # . Comparé aux fonctions de partition standard (kmeans, pam, clara et fanny)
                       # qui nécessite que l'utilisateur spécifie le nombre optimal de clusters, la fonction
                       # eclust() calcule automatiquement la statistique d'écart pour estimer le bon nombre
                       # des grappes.
                       # . Il fournit des informations de silhouette pour toutes les méthodes de partitionnement et de regroupement hiérarchique
                       # 
                       # . Il dessine de beaux graphiques en utilisant ggplot2
                         #> eclust(x, FUNcluster = "kmeans", hc_metric = "euclidean", ...)
                       # . x: vecteur numérique, matrice de données ou data.frame
                       # . FUNcluster: une fonction de regroupement incluant "kmeans", "pam", "clara", "fanny",
                       # "Hclust", "agnes" et "diana". L'abréviation est autorisée.
                       # . hc_metric: chaîne de caractères spécifiant la métrique à utiliser pour le calcul de 
                       # dissimilarités entre les observations. Les valeurs autorisées sont celles acceptées par
                       # function dist() [y compris "euclidean", "manhattan", "maximum", "canberra",
                       # "Binaire", "minkowski"] et des mesures de distance basées sur la corrélation ["pearson",
                       # "Spearman" ou "kendall"]. lorsque FUNcluster est un clustering hiérarchique, Utilisé uniquement 
                       # les fonction telle que "hclust", "agnes" ou "diana".
                       # .. . . : autres arguments à passer à FUNcluster.
                       # 
                       # La fonction eclust () renvoie un objet de classe eclust contenant le résultat de la 
                       # fonction standard utilisée (par exemple, kmeans, pam, hclust, agnes, diana, etc.).
                       # 
                       # Il comprend également:
                       # . cluster: l'affectation de cluster d'observations après la coupe de l'arbre
                       # . nbclust: le nombre de grappes
                       # . silinfo: l'information de silhouette des observations
                       # . size: la taille des grappes
                       # . data: une matrice contenant les données d'origine ou les données standardisées (si stand = VRAI)
                       # . gap_stat: contenant des statistiques d'écart
                       # 
                       # Pour calculer un clustering de partitionnement, tel que k-means avec le clustering  k = 3, on fait:
                       # K-means clustering
                       km.res <- eclust(df, "kmeans", k = 3, nstart = 25, graph = FALSE)
                       # Visualisation k-means clusters
                       fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
                                    palette = "jco", ggtheme = theme_minimal())
                       
                       # pour calculer le clusturing hierachique, on fait:
                       # Hierarchical clustering
                       hc.res <- eclust(df, "hclust", k = 3, hc_metric = "euclidean", hc_method = "ward.D2", graph = FALSE)
                       # Visualisation du dendrogramme
                       fviz_dend(hc.res, show_labels = FALSE,
                                 palette = "jco", as.ggplot = TRUE)
                       
                       
                       # Tracé de la silhouette
                       # Rappelons que le coefficient de silhouette (Si) mesure la similarité d'un objet i avec
                       # les autres objets dans son propre groupe par rapport à ceux du groupe voisin. les Valeurs de (Si) varie de -1 à 1:
                       # 
                       # . Une valeur de Si proche de 1 indique que l'objet est bien groupé. c'est à dire
                       # que l'objet i est similaire aux autres objets de son groupe.
                       # . Une valeur de Si proche de -1 indique que l'objet est mal groupé, et que
                       # l'affectation à un autre groupe améliorerait probablement les résultats globaux.
                       # 
                       # Il est possible de dessiner des coefficients de silhouette d'observations en utilisant la fonction
                       # fviz_silhouette() [package factoextra], qui imprimera également un résumé de la silhouette
                       # de la sortie d'analyse. Pour éviter cela, vous pouvez utiliser l'option print.summary = FALSE.
                       fviz_silhouette(km.res, palette = "jco",
                                       ggtheme = theme_classic())
                       
                       # on peut aussi extraire les informations de la silhouette:
                       # informations de la silhouette
                       silinfo <- km.res$silinfo
                       names(silinfo)
                       # Largeurs de la silhouette de chaque observation  
                       head(silinfo$widths[, 1:3], 10)
                       # Largeur moyenne de la silhouette de chaque groupe  
                       silinfo$clus.avg.widths
                       # La moyenne totale (moyenne de toutes les largeurs de silhouettes individuelles)
                       silinfo$avg.width
                       # La taille de chaque cluster
                       km.res$size
                       
                       km.res$silinfo$widths
                       # On peut voir que plusieurs échantillons (2), dans le groupe 2, ont un coefficient de silhouette négatif.
                       # Cela signifie qu'ils ne sont pas dans le bon groupe. Nous pouvons trouver le nom de ces
                       # échantillons et déterminer les groupes qui sont plus proches (cluster voisin), comme suit:
                       
                       
                       #  largeur de la Silhouette observée
                       sil <- km.res$silinfo$widths[, 1:3]
                       # Objets avec silhouette négative
                       neg_sil_index <- which(sil[, 'sil_width']
                                              < 0)
                       sil[neg_sil_index, , drop = FALSE]
                       
                       
            ### Calcul de l'indice Dunn et d'autres statistiques de validation des grappes
                       # La fonction cluster.stats() [fpc package] et la fonction NbClust() [ NbClust package] peut être utilisé pour calculer l'indice Dunn et de nombreux autres indices.
                        #> cluster.stats(d = NULL, clustering, al.clustering = NUll]
                       # . d: un objet de distance entre les cas généré par la fonction dist ()
                       # . clustering: vecteur contenant le numéro de cluster de chaque observation
                       # . alt.clustering: vecteur tel que pour le clustering, indiquant un clustering alternatif
                       
                       # La fonction cluster.stats() renvoie une liste contenant de nombreux composants utiles pour
                       # analyser les caractéristiques intrinsèques d'un clustering:
                       # . cluster.number: nombre de clusters
                       # . cluster.size: vecteur contenant le nombre de points dans chaque cluster
                       # . average.distance, median.distance: vecteur contenant le cluster au sein de
                       # distances moyennes / médianes
                       # . average.between: distance moyenne entre les grappes. Nous voulons qu'il soit aussi grand
                       # comme possible
                       # . average.within: distance moyenne au sein des grappes. Nous voulons que ce soit aussi petit que
                       # possible
                       # . clus.avg.silwidths: vecteur des largeurs de silhouette moyennes du cluster. Rappelons que, le
                       # La largeur de la silhouette est également une estimation de la distance moyenne entre les grappes.
                       # Sa valeur est comprise entre 1 et -1 avec une valeur de 1 indiquant un très bon
                       # grappe.
                       # . within.cluster.ss: une généralisation de la somme des carrés à l'intérieur des clusters (kmeans
                       # fonction objective), qui est obtenue si d est une matrice de distance euclidienne.
                       # . dunn, dunn2: indice Dunn
                       # . corrected.rand, vi: Deux indices pour évaluer la similarité de deux clusters: 
                       # l'indice Rand corrigé et VI de Meila
                       
                       # Tous les éléments ci-dessus peuvent être utilisés pour évaluer la qualité interne du clustering.
                       # Dans les sections suivantes, nous allons calculer les statistiques de qualité de clustering pour k-means.
                       # Regarder le within.cluster.ss (dans la somme des carrés des grappes), le average.within
                       # (distance moyenne au sein des clusters) et le clus.avg.silwidths (vecteur de la largeur moyenne du cluster
                       #  de la silhouette).
                       library(fpc)
                       # Statistics pour le clustering k-means 
                       km_stats <- cluster.stats(dist(df), km.res$cluster)
                       # Dun index
                       km_stats$dunn
                       km_stats

            ### Validation de clustering externe
                       # Parmi les valeurs renvoyées par la fonction cluster.stats(), il existe deux index
                       # qui évaluent la similarité de deux groupes, à savoir l'indice Rand corrigé et Meila VI.
                       # Nous savons que les données de l'iris contiennent exactement 3 groupes d'espèces.
                       # Le clustering K-means correspond-il à la structure réelle des données?
                       # Nous pouvons utiliser la fonction cluster.stats() pour répondre à cette question.
                       # Commençons par calculer une tabulation croisée entre les clusters k-means et les  Étiquettes d'espèces de référence

                       table(iris$Species, km.res$cluster)
                       # On peut voir que:
                       # . Toutes les espèces de setosa (n = 50) ont été classées dans le groupe 1
                       # . Un grand nombre d'espèces de versicor (n = 39) a été classé dans le groupe 3.
                       # Certains d'entre eux (n = 11) ont été classés dans le groupe 2.
                       # . Un grand nombre d'espèces de virginica (n = 36) a été classé dans le groupe 2.
                       # Certains d'entre eux (n = 14) ont été classés dans le groupe 3.
                       
                       # Il est possible de quantifier l'accord entre les espèces et les clusters k-means en utilisant
                       # soit l'index Rand corrigé et le VI de Meila fourni comme suit:
                         
                       library("fpc")
                       # calcule des statistiques de  cluster 
                       species <- as.numeric(iris$Species)
                       clust_stats <- cluster.stats(d = dist(df),
                             species, km.res$cluster)
                       #  Rand index corrigé
                       clust_stats$corrected.rand

                       # VI
                       clust_stats$vi

                       # L'indice Rand corrigé fournit une mesure pour évaluer la similarité entre
                       # deux partitions, ajustées pour le hasard. Sa gamme est -1 (pas d'accord) à 1 ( accord parfait). 
                       # L'accord entre les types d'espèces et la solution de cluster est 0.62
                       #  en utilisant l'indice Rand et de  0.748 en utilisant le VI de Meila

                       # La même analyse peut être calculée à la fois pour le PAM et le clustering hiérarchique:
                       # Accord entre les espèces et les cluster de pam
                       pam.res <- eclust(df, "pam", k = 3, graph = FALSE)
                       table(iris$Species, pam.res$cluster)
                       cluster.stats(d = dist(iris.scaled),
                              species, pam.res$cluster)$vi
                       # Accord entre les espèces et les cluster du HC
                       res.hc <- eclust(df, "hclust", k = 3, graph = FALSE)
                       table(iris$Species, res.hc$cluster)
                       cluster.stats(d = dist(iris.scaled),
                                species, res.hc$cluster)$vi

                       # Validation de clustering externe, peut être utilisé pour sélectionner un algorithme de clustering approprié pour
                       # un ensemble de données donné.
                       
                       # Nous avons décrit comment valider les résultats de la classification en utilisant la méthode de la silhouette et la
                       # Dunn index. Cette tâche est facilitée en utilisant la combinaison de deux fonctions R: eclust()
                       # et fviz_silhouette dans le package factoextra. Nous avons également montré comment évaluer
                       # l'accord entre un résultat de clustering et une référence externe.
                       # Dans la section suivantes, nous allons montrer en (a1) comment  choisir l'algorithme de clustering approprié
                       # pour vos données et en (a2) on va calculer les valeurs p pour la classification hiérarchique.

      ####  choisir le meilleur algorithme de clusturing
                       # Choisir la meilleure méthode de classification pour une donnée  peut être une tâche difficile
                       # de l'analyste. Cette section décrit le package R clValid (G. Brock et al., 2008),
                       # qui peut être utilisé pour comparer simultanément plusieurs algorithmes de clustering en un seul
                       # appel de fonction pour identifier la meilleure approche de regroupement et le nombre optimal de grappes.
                       # nous allons Commencer par décrire les différentes mesures du package clValid pour comparer
                       # algorithmes de clustering. Ensuite, nous allons présenter la fonction * clValid * (). 
                       # puis nous ferons un exemple pour valider les résultats de la mise en cluster et comparer les algorithmes de clustering.

              ### Mesures pour comparer les algorithmes de clustering
                       # Le package clValid compare les algorithmes de clustering en utilisant deux mesures de validation de cluster :
                       # 
                       #   1. Mesures internes, qui utilisent des informations intrinsèques dans les données pour évaluer la qualité
                       # de la classification. Les mesures internes comprennent la connectivité, le coefficient de la silhouette
                       #  et l'indice de Dunn comme décrit plus haut au niveau (Validation de cluster Statistiques).
                       #  2. Mesures de stabilité, une version spéciale des mesures internes, qui évalue
                       #  la cohérence d'un résultat de clustering en le comparant avec les clusters obtenus
                       #  après chaque colonne est retirée, un à la fois.
                       #  Les mesures de stabilité des clusters comprennent:
                       #  . La proportion moyenne de non-chevauchement (APN)
                       #  . La distance moyenne (AD)
                       #  . La distance moyenne entre les moyennes (ADM)
                       #  . Le chiffre du mérite (FOM)
                       #  L'APN, AD et ADM sont tous basés sur le tableau de classification croisée du clustering d'origine
                       #   sur les données complètes avec le clustering basé sur la suppression d'une colonne.
                       #  . L'APN mesure la proportion moyenne d'observations non placées dans le
                       #  même cluster par un clustering basé sur les données complètes et le regroupement sur la base de données
                       #   avec une seule colonne supprimée.
                       #  . L'AD mesure la distance moyenne entre les observations placées dans le même
                       #    cluster dans les deux cas (ensemble de données complet et suppression d'une colonne).
                       #  . L'ADM mesure la distance moyenne entre les centres de cluster pour les observations
                       #    placé dans le même groupe dans les deux cas.
                       #  . Le FOM mesure la variance intra-cluster moyenne de la colonne supprimée,
                       #    où le regroupement est basé sur les colonnes restantes (non supprimées).
                       #    Les valeurs de APN, ADM et FOM vont de 0 à 1, avec une plus petite valeur correspondant
                       #    avec des résultats de regroupement hautement cohérents. AD a une valeur comprise entre 0 et
                       #    l'infini, et des valeurs plus petites sont également préférées.
                       #  Notez que le paquetage clValid fournit également des mesures de validation biologique,
                       #  évalue la capacité d'un algorithme de regroupement à produire des grappes biologiquement significatives.
                       #    Une application est une puce ou des données RNAseq où les observations correspondent aux gènes.

                ### Comparons les algorithmes de clustering 
                       # Nous allons utiliser la fonction clValid() [dans le package clValid]:
                         #> clValid(obj, nClust, clMethods = "hierarchical",
                         #>         validation = "stability", maxitems = 600,
                         #>        metric = "euclidean", method = "average")

                         # . obj: une matrice numérique ou une data.frame. Les lignes sont les éléments à regrouper et
                         # les colonnes sont des échantillons.
                         # . nClust: vecteur numérique spécifiant les nombres de clusters à évaluer.
                         # 
                         # . clMethods: la méthode de clustering à utiliser. Les options disponibles sont "hiérarchiques",
                         # "Kmeans", "diana", "fanny", "som", "modèle", "sota", "pam", "clara",
                         # et "agnes", avec plusieurs choix autorisés.
                         # . validation: le type de mesures de validation à utiliser. Les valeurs autorisées sont
                         # "Interne", "stabilité" et "biologique", avec plusieurs choix autorisés.
                         # . maxitems: le nombre maximum d'éléments (lignes dans la matrice) pouvant être
                         # groupé.
                         # . métrique: la métrique utilisée pour déterminer la matrice de distance. Les choix possibles sont
                         # "Euclidien", "corrélation" et "manhattan".
                         # . méthode: pour le clustering hiérarchique (hclust et agnes), l'agglomération
                         # méthode à utiliser. Les choix disponibles sont "ward", "single", "complete" et
                         # "moyenne".
                         # Par exemple, considérons le jeu de données iris, la fonction clValid () peut être utilisée comme suit.
                         # Nous commençons par les mesures internes du cluster, qui comprennent la connectivité, la largeur de la silhouette
                         # et Dunn index. Il est possible de calculer simultanément ces mesures internes pour
                         # plusieurs algorithmes de clustering en combinaison avec une série de numéros de cluster.
                         
                         # install.packages("clValid")                         
                         library(clValid)
                         # données Iris :
                         # - on suprime la colonne "species" et on standardise
                         df <- scale(iris[, -5])
                         # on calcul clValid
                         clmethods <- c("hierarchical","kmeans","pam")
                         intern <- clValid(df, nClust = 2:6,
                                           clMethods = clmethods, validation = "internal")
                         # Summary
                         summary(intern)

                         # On peut voir que la classification hiérarchique avec deux (2) clusters est la meilleure (car ont les plus faibles connectivités, les plus élevé Dunn et Silhouette )
                         # chaque cas (c'est-à-dire, pour la connectivité, les mesures Dunn et Silhouette). Quel que soit le
                         # algorithme de clustering, le nombre optimal de clusters semble être deux en utilisant les trois les mesures de clustering.
                         
                         
                         # Les mesures de stabilité peuvent être calculées comme suit:
                         # mesures de stabilité
                           clmethods <- c("hierarchical","kmeans","pam")
                           stab <- clValid(df, nClust = 2:6, clMethods = clmethods,
                                 validation = "stability")
                         # Afficher uniquement les scores optimaux
                           optimalScores(stab)
                           
                         # Pour les mesures APN et ADM, regroupement hiérarchique avec deux clusters
                         # donne le meilleur score. Pour les autres mesures (AD et FOM), le regroupement PAM avec six clusters a le meilleur score.
                           

                          # resumé: Ici, nous avons décrire comment comparer les algorithmes de clustering à l'aide du package clValid de R

          #### Calcule de la valeur de P-value (p) pour la Classification hiérarchique
                           Les grappes peuvent être trouvées dans un ensemble de données par hasard en raison d'un bruit de regroupement ou d'une erreur d'échantillonnage.
                           Cet article décrit le package p pvclust (Suzuki et al., 2004) qui utilise bootstrap
                           (techniques de rééchantillonnage) pour calculer la valeur p pour chaque grappe hiérarchique.
                           
                           # Algorithme
                           # 1. Généré des milliers d'échantillons bootstrap en échantillonnant au hasard des éléments sur les données
                           # 
                           # 2. Calculer le clustering hiérarchique sur chaque copie bootstrap
                           # 3. Pour chaque cluster:
                           #   . calculer la probabilité de bootstrap (BP) qui correspond à la
                           #     fréquence à laquelle le cluster est identifié dans les copies bootstrap.
                           #   . Calculer les valeurs de probabilité approximatives (AU) (valeurs p) en
                           #     rééchantillonnage bootstrap multi-échelles
                           #   Les clusters avec AU>= 95% sont considérés comme fortement supportés par les données.
                           
                           # install.packages("pvclust")

                            library(pvclust)
                           # preparation des données
                             data("lung")
                             head(lung[, 1:4])
                           # Dimension 
                             dim(lung)
                           # Nous n'utiliserons qu'un sous-ensemble de l'ensemble de données pour l'analyse de la mise en cluster. La fonction R
                           # sample() peut être utilisé pour extraire un sous-ensemble aléatoire de 30 échantillons:
                           set.seed(123)
                           ss <- sample(1:73, 30) # extraire 20 element aléatoirement
                           df <- lung[, ss]

                      # Calculer la valeur p pour la classification hiérarchique
                      # Description de la fonction pvclust ()
                        pvclust(data, method.hclust = "average",
                               method.dist = "correlation", nboot = 1000)
                      # Notons que, le temps de calcul peut être fortement diminué en utilisant le calcul parallèle
                      # version appelée parPvclust(). 
                        parPvclust(cl=NULL, data, method.hclust = "average",
                                   method.dist = "correlation", nboot = 1000,
                                   iseed = NULL)

                        # . données: matrice de données numériques ou data.frame.
                        # . method.hclust: la méthode d'agglomération utilisée dans la classification hiérarchique.
                        #   Les valeurs possibles sont "average", "ward", "single", "complete", "mcquitty",
                        #   "Médiane" ou "centroïde". La valeur par défaut est "average". 
                        # . method.dist: la mesure de distance à utiliser. Les valeurs possibles sont l'une des
                        #   "Correlation", "uncentered", "abscor" ou ceux qui sont autorisés pour argument de la méthode
                        #    dans la fonction dist(), par exemple "euclidean" et "manhattan".
                        # . nboot: nombre de réplications bootstrap. La valeur par défaut est 1000.
                        # . iseed: un intégrateur pour les graines aléatoires. 
                         
                        # La fonction pvclust () retourne un objet de classe pvclust contenant de nombreux éléments
                        # y compris hclust qui contient le résultat de la classification hiérarchique pour les données d'origine
                        # généré par la fonction hclust().
                                                            
                        # Utilisation de la fonction pvclust()
                        # pvclust () effectue un clustering sur les colonnes de l'ensemble de données, qui correspondent à
                        # échantillons dans notre cas. Si on veut effectuer le clustering sur les variables (ici,
                        # gènes), on devra transposer l'ensemble de données en utilisant la fonction t().
                        
                        # exemple: ci-dessous le calcule de pvclust() en utilisant 10 comme le nombre de réplications bootstrap :
                        library(pvclust)
                        set.seed(123)
                        res.pv <- pvclust(df, method.dist="cor",
                                          method.hclust="average", nboot = 10)


                      # graph par defaut
                        plot(res.pv, hang = -1, cex = 0.5)
                        pvrect(res.pv)

                      # Sur le dendrogramme, les valeurs des P-value de AU sont en (rouge, gauche), les valeurs BP (vert, droite),
                      # et les étiquettes de cluster (gris, bas). Les clusters avec AU> = 95% sont indiqués par le
                      # rectangles et sont considérés comme fortement supportés par les données.
                      
                      # Pour extraire les objets des grappes significatives, on utilise la fonction pvpick() comme suit:
                        clusters <- pvpick(res.pv)
                        clusters

                      # Le calcul parallèle peut être appliqué comme suit:

                      # Créer un cluster de sockage parallèle
                        library(parallel)
                        cl <- makeCluster(2, type = "PSOCK")
                      # version parallelle  de pvclust
                        res.pv <- parPvclust(cl, df, nboot=1000)
                        stopCluster(cl)


    ##### V  clustering avancé    
                        # Contenu:
                        # . Classification hiérarchique k-means
                        # . Clustering flou (Fuzzy clustering)
                        # . Clustering basé sur un modèle 
                        # . DBSCAN: clustering basé sur la densité 

                ### Clustering hiérarchique K-Means
                        # K-means  représente l'un des algorithmes de regroupement les plus populaires. cependant,
                        # il a quelques limitations: il oblige l'utilisateur à spécifier le nombre de grappes à l'avance
                        # et sélectionne les centroïdes initiaux de manière aléatoire. La solution finale de clustering k-means est très
                        # sensible à cette sélection aléatoire initiale des centres de cluster. Le résultat pourrait être
                        # (légèrement) différent chaque fois que l'on calcule les k-means.
                        # Dans cette partie, nous allons décrire une méthode hybride, appelée k-means hiérarchique
                        # clustering (hkmeans), pour améliorer les résultats de k-means.
                        
                  ### Algorithme
                        # L'algorithme est résumé comme suit:
                        # 1. Calculer la classification hiérarchique et couper l'arbre en k-clusters
                        # 2. Calculer le centre (c'est-à-dire la moyenne) de chaque groupe
                        # 3. Calculer k-means en utilisant l'ensemble des centres de cluster (définis à l'étape 2) comme
                        #    centres de cluster initiaux.

                        #  Notons que l'algorithme k-means améliorera le partitionnement initial généré à
                        #  l'étape 2 de l'algorithme. Par conséquent, le partitionnement initial peut être légèrement différent du
                        #  partitionnement final obtenu à l'étape 4.
  
                        #  La fonction R hkmeans() [de factoextra], fournit une solution facile pour calculer le
                        #  clustering k-means hiérarchique. Le format du résultat est similaire à celui fourni
                        #  par la fonction standard kmeans().
  
                        #  exemple:   
                        # preparation de donnée et standardisation 
                        df <- scale(USArrests)

                        # calculer le clustering  k-means hierarchique
                          library(factoextra)
                          res.hk <-hkmeans(df, 4)

                          # Éléments retournés par hkmeans()
                          names(res.hk)
                          res.hk
                          # Visualisation de l'arbre
                          fviz_dend(res.hk, cex = 0.6, palette = "jco",
                                    rect = TRUE, rect_border = "jco", rect_fill = TRUE)
                          
                          # Visualisation des clusters finaux du hkmeans  
                          fviz_cluster(res.hk, palette = "jco", repel = TRUE,
                                       ggtheme = theme_classic())
                          
                          # Nous avons décrit la classification hiérarchique hybride k-means pour améliorer les résultats k-means.
                          

            ###  Clustering flou (fluzz)
                          # Le clustering flou est considéré comme un clustering doux, dans lequel chaque élément a une
                          # probabilité d'appartenance à chaque groupe. En d'autres termes, chaque élément a un ensemble de
                          # les coefficients d'appartenance correspondant au degré d'être dans un cluster donné.
                          # Ceci est différent des clustering k-means et k-medoid , où chaque objet est affecté
                          # exactement à un cluster. les clustering K-means et k-medoids  sont connus comme durs ou
                          # clustering non-flou.
                          # Dans le clustering flou, les points proches du centre d'un cluster peuvent être dans le cluster
                          # ayant le degré plus élevé pour ses points que dans le bord d'un cluster. Le degré, auquel un élément
                          # appartient à un cluster donné, est une valeur numérique variant de 0 à 1.
                          # L'algorithme FCM (flou c-means) est l'un des clusters fuzzy les plus utilisés.
                          #  Le centroïde d'un cluster est calculé comme la moyenne de tous les points, pondérés
                          # par leur degré d'appartenance au cluster:
                          #   Dans cette partie, nous allons décrire comment calculer le clustering flou.
                          # 
                          # Nous utiliserons les paquets R suivants: 
                          #   1) cluster pour le calcul de clustering flou et 
                          #   2) factoextra pour visualiser les grappes.
                          # 
                          # Calcul du clustering flou
                          # La fonction fanny() [package R du cluster] peut être utilisée pour calculer le clustering flou.
                          # FANNY est synonyme de clustering d'analyse floue. 
                            
                              ##>  fanny(x, k, metric = "euclidean", stand = FALSE)

                          # . x: Une matrice de données ou une data.frame ou une matrice de dissimilarité
                          # . k: le nombre de grappes à générer
                          # . metric: métrique pour calculer les dissimilarités entre les observations
                          # . stand: Si TRUE, les variables sont standardisées avant de calculer les dissimilarités
                          # 
                          # La fonction fanny () renvoie un objet incluant les composants suivants:
                          #   . membership: matrice contenant le degré d'appartenance de chaque observation
                          # à un cluster donné. Les noms de colonne sont les clusters et les lignes sont des observations
                          #   . coeff: coefficient de partition de Dunn F(k) de la classification, où k est le nombre
                          # de grappes. F(k) est la somme de tous les coefficients d'appartenance au carré, divisée par
                          # le nombre d'observations. Sa valeur est comprise entre 1 / k et 1.
                          # la forme du coefficient est également donnée. Il est défini comme (F (k) - 1 / k) / (1 - 1 / k), et
                          # varie entre 0 et 1. Une faible valeur du coefficient de Dunn indique un clustering très flou
                          # , alors qu'une valeur proche de 1 indique un clustering quasi-net.
                          # . clustering: le vecteur de clustering contenant le groupement le plus proche des
                          # observations
                          # 
                          # exemple:

                          library(cluster)
                          df <- scale(USArrests) # Standardisation
                          res.fanny <- fanny(df, 2) # calcul un clustering flou (fuzzy)  avec k=2
                          
                          # Les différents composants peuvent être extraits en faisant:
                          head(res.fanny$membership, 3) # coefficient des degré d'appartenance de chaque observation  à un cluster donné 
                          res.fanny$coeff # Dunns coefficient de partitionnement 
                          head(res.fanny$clustering) # Observation des groupes

                          # visualisons les groupes d'observation
                          library(factoextra)
                          fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
                                       palette = "jco", ggtheme = theme_minimal(),
                                       legend = "right")

                          # Pour évaluer les qualités des résultats de la classification, 
                          # tracons le coefficient des silhouette:
                          fviz_silhouette(res.fanny, palette = "jco",
                                          ggtheme = theme_minimal())

                          # Résumé
                          # Le regroupement flou est une alternative au clustering k-means, où chaque point 
                          # a un coefficient d'adhésion à chaque groupe. Ici, nous avons démontré comment calculer et
                          # Visualisez le clustering flou en utilisant la combinaison des packages R et cluster.

                          
              ### Clustering basé sur un modèle
                          # Les méthodes traditionnelles de classification, telles que la classification hiérarchique  et
                          # le clustering k-means  , sont heuristiques et ne sont pas basés sur des modèles formels.
                          # De plus, l'algorithme de k-means est généralement initialisé de manière aléatoire, donc des exécutions différentes
                          # de k-means donnera souvent des résultats différents. De plus, k-means nécessite que l'utilisateur
                          # spécifiez le nombre optimal de clusters.
                          # Une alternative est le clustering basé sur un modèle, qui considère que les données proviennent 
                          # d'une distribution qui est un mélange de deux ou de plusieurs grappes (Chris Fraley et Adrian E.Raftery, 2002 et 2012). 
                          # Contrairement à k-means, le clustering basé sur un modèle utilise une 
                          # affectation, où chaque point de données a une probabilité d'appartenance à chaque cluster.
                          # Dans cette , nous illustrons le clustering basé sur un modèle en utilisant le package R mclust.
                          
                    # Concept de clustering basé sur un modèle
                          # Dans un clustering basé sur un modèle, les données sont considérées comme provenant d'un mélange de densité.
                          # Chaque composante (c'est-à-dire la grappe) k est modélisée par la distribution normale ou gaussienne
                          #                   qui est caractérisé par les paramètres:
                          #                   . ??k: vecteur moyen,
                          #                   . qk: matrice de covariance,
                          #                   . Une probabilité associée dans le mélange. Chaque point a une probabilité 
                          #                   d'appartenant à chaque cluster.
                          # Exemple: 
                          library("MASS")
                          data("geyser")
                          # graph
                          library("ggpubr")
                          x11()
                          ggscatter(geyser, x = "duration", y = "waiting")+ # dans nos études , ici, on poura prendre les deux premiers axes factoriel
                            geom_density2d() # Ajouter  2D densité

                         #  Le graphique ci-dessus suggère au moins 3 groupes dans le mélange. La forme de chacun des 
                         #  3 groupes semblent être approximativement elliptiques suggérant trois distributions normales bivariées.
                         #                      Comme les 3 ellipses semblent être similaires en termes de volume, de forme et
                         #                     d'orientation, nous pourrions anticiper que les trois composants de ce mélange pourraient avoir
                         #                     des matrices de covariance homogènes.
                         #                     
                         #  Estimation des paramètres du modèle
                         #                     Les paramètres du modèle peuvent être estimés à l'aide de la fonction Experance-Maximisation (EM)
                         #                     de l'algorithme initialisé par classification hiérarchique basée sur un modèle. Chaque cluster k est centré
                         #                     aux moyennes ??k, avec une densité accrue pour les points proches de la moyenne.
                         #                     Les caractéristiques géométriques (forme, volume, orientation) de chaque groupe sont déterminées par
                         #                     la matrice de covariance qk.
                         #  Différentes paramétrisations possibles de qk sont disponibles dans le paquet R mclust.
                         #                     Les options de modèle disponibles, dans le package mclust, sont représentées par des identificateurs comprenant:
                         #                     EII, VII, EEI, VEI, EVI, VVI, EEE, EEV, VEV et VVV.
                         #                     Le premier identificateur fait référence au volume, le second à la forme et le troisième à l'orientation.
                         #                     E signifie "égal", V pour "variable" et I pour "axes de coordonnées".
                         #                     Par exemple:
                         #                     . EVI désigne un modèle dans lequel les volumes de tous les groupes sont égaux (E), le
                         #                     les formes des grappes peuvent varier (V), et l'orientation est l'identité (I) ou Coordonner les axes.
                         #                     . EEE signifie que les grappes ont le même volume, la même forme et la même orientation
                         #                     espace p-dimensionnel.
                         #                     . VEI signifie que les grappes ont un volume variable, la même forme et l'orientation
                         #                     égal aux axes de coordonnées.
                         #  Choisir le meilleur modèle
                         #                     Le package Mclust utilise le maximum de vraisemblance pour s'adapter à tous ces modèles, avec différents
                         #                     paramètres de la matrice de covariance, pour une gamme de k composantes.
                         #                     Le meilleur modèle est sélectionné en utilisant le critère d'information bayésien ou BIC. Un grand
                         #                      score du BIC indique des preuves solides pour le modèle correspondant.
                         # Calcul du clustering basé sur un modèle dans R
                                             # install.packages ("mclust")
                                             # Notons que le clustering basé sur un modèle peut être appliqué sur des données univariées ou multivariées.
                          library("mclust")
                          data("diabetes")
                          head(diabetes, 3)

                          # . classe: le diagnostic: normal, chimiquement diabétique, et ouvertement diabétique. Exclu de l'analyse de cluster.
                          # . glucose: réponse du glucose plasmatique au glucose oral
                          # . insuline: réponse de l'insuline plasmatique au glucose oral
                          # . sspg: glucose plasmatique à l'état d'équilibre (mesure de la résistance à l'insuline)
                          # Le clustering basé sur un modèle peut être calculé en utilisant la fonction Mclust () comme suit:
                          library(mclust)
                          df <- scale(diabetes[, -1]) # Standardisation
                          mc <- Mclust(df) # Model-based-clustering
                          summary(mc) 

                          # Pour ces données, on peut voir que le clustering basé sur un modèle a sélectionné un modèle avec trois
                          # composants (c'est-à-dire des groupes). Le nom de modèle sélectionné optimal est le modèle VVV. 
                          #                    Les trois composants sont ellipsoïdaux avec [un volume, une forme et une orientation variables].
                          #                    Le résumé contient également le tableau de regroupement spécifiant le nombre d'observations
                          #                    dans chaque groupe.
                          #                    Vous pouvez accéder aux résultats comme suit:


                          mc$modelName #   model Optimal sélectionné ==> "VVV"
                          mc$G #  numbre Optimal de cluster => 3
                          head(mc$z, 30) # Probabilité d'appartenir à un cluster donné
                          head(mc$classification, 30) # Affectation de cluster à chaque observation

                          # Visualisation de la mise en cluster basée sur un modèle
                          # Les résultats de clustering basés sur un modèle peuvent être dessinés à l'aide de la fonction de base plot.Mclust() [dans
                          #                    package mclust]. Ici, nous allons utiliser la fonction fviz_mclust () [dans le package factoextra] pour
                          #                    créer de beaux graphs basées sur ggplot2.
                          #                    Dans la situation où les données contiennent plus de deux variables, fviz_mclust () utilise
                          #                    une analyse en composantes principales pour réduire la dimensionnalité des données. 
                          #                    Les deux premières composantes principaux sont utilisés pour produire un diagramme de dispersion des données. cependant,
                          #                    si on veut tracer les données en utilisant seulement deux variables d'intérêt, disons ici
                          #                    c ("insuline", "sspg"), on peut le spécifier que dans la fonction fviz_mclust() en utilisant 
                          #                    l'argument choose.vars = c ("insuline", "sspg").
                          library(factoextra)
                          # le BIC est utilisé pour choisir  le nombre de  clusters
                          fviz_mclust(mc, "BIC", palette = "jco")
                          # Classification: graph
                          fviz_mclust(mc, "classification", geom = "point",
                                      pointsize = 1.5, palette = "jco")
                          # Classification incertaine (avec incertitude)
                          fviz_mclust(mc, "uncertainty", palette = "jco")

                          # Notons que, dans la courbe d'incertitude, les symboles plus grands indiquent des observations plus incertaines. 




        ### DBSCAN: basé sur la densité
            # DBSCAN (clustering spatial basé sur la densité et application avec bruit),
            #                                  c'est un algorithme de clusering basé sur la densité, introduit dans Ester et al. 1996, qui peut
            #                                  être utilisé pour identifier des groupes de n'importe quelle forme dans un ensemble de données contenant du bruit et des valeurs aberrantes.
            #                                  L'idée de base derrière l'approche de regroupement basée sur la densité est dérivée d'une 
            #                                  méthode humaine de clustering intuitive. Par exemple, en regardant la figure ci-dessous (voir livre), on peut
            #                                  facilement identifier quatre groupes avec plusieurs points de bruit, en raison des différences
            #                                  dans la densité de points.
            #                                  Les grappes sont des régions denses dans l'espace de données, séparées par des régions de plus faible densité de
            #                                  points. L'algorithme DBSCAN est basé sur cette notion intuitive de "clusters" et
            #                                  "bruit". L'idée clé est que pour chaque point d'un cluster, le voisinage d'un
            #                                   rayon doit contenir au moins un nombre minimum de points.
            #                                  Dans cette, nous allons décrire l'algorithme DBSCAN et montrer comment
            #                                  calculer DBSCAN en utilisant le paquet * fpc * R.
            #                         Pourquoi DBSCAN?
            #                                  Les méthodes de partitionnement (K-means, PAM clustering) et le clustering hiérarchique sont
            #                                  approprié pour trouver des amas de forme sphérique ou des amas convexes. En d'autres termes, ils
            #                                  ne fonctionne bien que pour des grappes compactes et bien séparées. De plus, ils sont aussi
            #                                  gravement affectés par la présence de bruit et de valeurs aberrantes dans les données.
            #                                  Malheureusement, les données de la vie réelle peuvent contenir: i) des groupes de formes arbitraires tels que ceux
            #                                  montré dans la figure ci-dessous (grappes ovales, linéaires et en forme de "S") (voir livre); ii) de nombreuses valeurs aberrantes et
            #                                  bruit.
            #                                  La figure ci-dessous montre un ensemble de données contenant des grappes non convexes et des valeurs aberrantes / bruits.
            #                                  Le jeu de données simulées multishapes [in factoextra package] est utilisé.
            #                                  Le graphique ci-dessus contient 5 groupes et valeurs aberrantes, notamment:
            #                                  . 2 grappes ovales
            #                                  . 2 groupes linéaires
            #                                  . 1 cluster compact
            #                                  Compte tenu de ces données, l'algorithme k-means a des difficultés pour identifier ces groupes avec
            #                                  des formes arbitraires. Pour illustrer cette situation, le code R suivant calcule l'algorithme de  k-means
            #                                   sur l'ensemble de données multishapes. La fonction fviz_cluster () [package factoextra]
            #                                  est utilisé pour visualiser les clusters.
                                             library(factoextra)
                          data("multishapes")
                          df <- multishapes[, 1:2]
                          set.seed(123)
                          km.res <- kmeans(df, 5, nstart = 25)
                          fviz_cluster(km.res, df, geom = "point",
                                       ellipse= FALSE, show.clust.cent = FALSE,
                                       palette = "jco", ggtheme = theme_classic())

                          # Nous savons qu'il y a 5 cinq groupes dans les données, mais on peut voir que la méthode k-means
                          # identifier de manière inexacte les 5 groupes.
            # Algorithme
                      # L'objectif est d'identifier les régions denses, qui peuvent être mesurées par le nombre d'objets
                      #                        près d'un point donné.
                      #                        Deux paramètres importants sont requis pour DBSCAN: epsilon ("eps") et le 
                      #                        nombre de points minimum ("MinPts"). Le paramètre eps définit le rayon du voisinage autour
                      #                        un point x. Ça s'appelle les 'epsilon-neighborhood de x'. Le paramètre MinPts est le
                      #                        nombre minimum de voisins dans le rayon "eps".
                      #                        Tout point x de l'ensemble de données, avec un nombre voisin supérieur ou égal à MinPts, est
                      #                        marqué comme un point de base. Nous disons que x est le point frontière, si le nombre de ses voisins est
                      #                        moins que MinPts, mais il appartient au voisinage d'un point central z. Finalement,
                      #                        si un point n'est ni un noyau ni un point de frontière, alors il est appelé un point de bruit ou un
                      #                        aberrant.
                      #                        La figure ci-dessous montre les différents types de points (noyau, frontière et points aberrants)
                      #                        en utilisant MinPts = 6. Ici x est un point central parce que les voisins '(x) = 6, y est un point limite parce que les voisins' (y) <MinPts,  appartient au '-neighborhood du
                      #                        point de base x. Enfin, z est un point de bruit.
                      #                 Nous commençons par définir 3 termes, requis pour comprendre l'algorithme DBSCAN:
                      #                        . Densité directe atteignable: Un point "A" est directement densité d'un autre
                      #                        le point "B" si: i) "A" est dans le voisinage de "B" et ii) "B" est un point central.
                      #                        . Densité atteignable: Un point "A" est la densité accessible à partir de "B" s'il y a un ensemble
                      #                        des points de base menant de "B" à "A.
                      #                        . Densité connectée: Deux points "A" et "B" sont des densités connectées s'il y a
                      #                        le point central "C", tel que "A" et "B" sont des densités atteignables à partir de "C".
                      #                        Un cluster basé sur la densité est défini comme un groupe de points connectés en densité.
                      #                 L'algorithme de clustering basé sur la densité (DBSCAN) fonctionne comme suit:
                      #                        1. Pour chaque point xi, calculez la distance entre xi et les autres points. Trouve
                      #                        tous les points voisins à la distance eps du point de départ (xi). Chaque point, avec un
                      #                        nombre de voisins supérieur ou égal à MinPts, est marqué comme point de base ou visité.
                      #                        2. Pour chaque point principal, s'il n'est pas déjà affecté à un cluster, créez un
                      #                        nouveau cluster. Trouver récursivement tous ses points de densité connectés et les affecter à la
                      #                        même cluster que le point de base.
                      #                        3. Itérer à travers les points non visités restants dans l'ensemble de données.
                      #                        Les points qui n'appartiennent à aucun cluster sont traités comme des valeurs aberrantes ou
                      #                        bruit.
                      #                Avantages
                      #                        1. Contrairement à K-means, DBSCAN n'exige pas que l'utilisateur spécifie le nombre de
                      #                        grappes à générer
                      #                        2. DBSCAN peut trouver n'importe quelle forme de cluster. Le cluster n'a pas besoin d'être circulaire.
                      #                        3. DBSCAN peut identifier les valeurs aberrantes
                      #                Estimation des paramètres
                      #                        . MinPts: plus l'ensemble de données est grand, plus grand doit être la valeur de minPts  choisie.
                      #                        les minPts doivent être  au moins supérieur à 3.
                      #                        . epsilon (eps): sa valeur  peut alors être choisie en utilisant un graphique de distance k, en traçant
                      #                        la distance au k = minPts plus proche voisin. Les bonnes valeurs de epsilon sont là où
                      #                        le graph montre un fort virage.
                      #                Calculer DBSCAN
                      #                        Ici, nous utiliserons le paquet R fpc pour calculer DBSCAN. Il est également possible d'utiliser le
                      #                        package dbscan, qui fournit une ré-implémentation plus rapide de l'algorithme DBSCAN
                      #                        par rapport au paquet fpc.
                      #                        Nous utiliserons également le paquet factoextra pour visualiser les clusters.
                                            # install.packages("fpc")
                                            # install.packages("dbscan")
                                            # install.packages("factoextra")
                          data("multishapes", package = "factoextra")
                          df <- multishapes[, 1:2]
                          # calcul du DBSCAN en utilisant le package  fpc 
                          library("fpc")
                          set.seed(123)
                          db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
                          # graph DBSCAN 
                          library("factoextra")
                          fviz_cluster(db, data = df, stand = FALSE,
                                       ellipse = FALSE, show.clust.cent = FALSE,
                                       geom = "point",palette = "jco", ggtheme = theme_classic())

                          # Notez que  Les points noirs correspondent aux valeurs aberrantes et donc ne sont classé dans auccune des 5 classe. 
                          # on peut jouer avec eps et MinPts pour changer les configurations de cluster.
                          # On peut voir que DBSCAN fonctionne mieux pour ces ensembles de données et peut identifier
                          # l'ensemble correct de clusters comparé aux algorithmes k-means.
                          # Le résultat de la fonction fpc :: dbscan () peut être affiché comme suit:
                          print(db)

                          # Dans le tableau ci-dessus, les noms de colonnes sont le numéro de cluster. Le cluster 0 correspond aux valeurs aberrantes
                          # (points noirs dans le diagramme DBSCAN). La fonction print.dbscan() affiche la statistique de
                          # nombre de points appartenant aux grappes qui sont des graines et des points de bordure.


                          # Appartenance au cluster. Les observations de bruit / aberrantes sont codées comme 0
                          # Un sous-ensemble aléatoire est affiché
                          db$cluster[sample(1:1089, 20)]

                          # L'algorithme DBSCAN exige que les utilisateurs spécifient les valeurs eps optimales et le paramètre
                          # MinPts. Dans le code R ci-dessus, nous avons utilisé eps = 0.15 et MinPts = 5. Une limitation de
                          # DBSCAN est qu'il est sensible au choix de epsilon, en particulier si les clusters ont des
                          # densités. Si epsilon est trop petit, les groupes clairsemés seront définis comme du bruit. Si epsilon est trop grand,
                          # les groupes plus denses peuvent être fusionnés ensemble. Cela implique que, s'il y a des clusters avec
                          # différentes densités locales, alors une seule valeur peut ne pas suffire.
                          
                  # Méthode de détermination des eps optimaux
                  #         valeur
                  #         La méthode proposée ici consiste à calculer les distances de k-plus proches  points
                  #         d'une matrice.
                  #         L'idée est de calculer, la moyenne des distances de chaque point à ses k voisins les plus proche.
                  #          La valeur de k sera spécifiée par l'utilisateur et correspond à MinPts.
                  #         Ensuite, ces distances k sont tracées dans un ordre croissant. Le but est de déterminer
                  #         le "genou", qui correspond au paramètre optimal eps.
                  #         Un genou correspond à un seuil où un changement brusque se produit le long de la distance k sur la  courbe.
                  #         La fonction kNNdistplot () [dans le package dbscan] peut être utilisée pour tracer la distance k terrain:    
                            dbscan::kNNdistplot(df, k = 5)
                            abline(h = 0.15, lty = 2)

                          # On peut voir que la valeur optimale d'eps est autour d'une distance de 0,15.

                   # Prédictions de cluster avec l'algorithme DBSCAN
                   # La fonction predict.dbscan (objet, data, newdata) [dans le package fpc] peut être utilisée pour
                   # prédire les groupes pour les points dans newdata. 
