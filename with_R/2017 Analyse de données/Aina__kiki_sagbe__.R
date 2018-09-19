############################ Projet d'analyse de donnees ############################
---
  title: "projet 2017"
author: "Aina"
date: "3 avril 2017"
output: word_document
---



############################ 1. Analyse Descriptive ############################ 

setwd("f:/analyse de donn�e Personelle/PROJET")
don1<-load("Census94_salaires.Rdata")
View(don1)



install.packages("ggplot2")
install.packages("MASS")
install.packages("ROCR")
install.packages("rpart")
install.packages("randomForest")
install.packages("adabag")
install.packages("class")
install.packages("knncat")
install.packages("mvtnorm")
install.packages("FactoMineR")
library(mvtnorm)
library(ggplot2)
library(MASS)
library(ROCR)
library(rpart)
library(randomForest)
library(adabag)
library(class)
library(knncat)
library(FactoMineR)


setwd("f:/analyse de donn�e Personelle/PROJET")

load("Census94_salaires.Rdata")

donnee_a_predir_1<-Dapp
summary(donnee_a_predir_1)


###### am�nagement de la base
# supression des ligne contenant des donn�es manquantes
w = list()
na.indic = NULL
for (k in 1:ncol(donnee_a_predir_1)){
  w[[k]] = which(donnee_a_predir_1[,k]=="?")
  donnee_a_predir_1[w[[k]],k] = NA
  na.indic=c(na.indic,w[[k]])
}
na.indic = unique(na.indic)
donnee_a_predir_1 = donnee_a_predir_1[-na.indic,]
summary(donnee_a_predir_1)

## Recodage de donnee_a_predir_1$marital_status
donnee_a_predir_1$marital_status <- as.character(donnee_a_predir_1$marital_status)
donnee_a_predir_1$marital_status[donnee_a_predir_1$marital_status == "Divorced"] <- "Separated"
donnee_a_predir_1$marital_status[donnee_a_predir_1$marital_status == "Married-AF-spouse"] <- "Married"
donnee_a_predir_1$marital_status[donnee_a_predir_1$marital_status == "Married-civ-spouse"] <- "Married"
donnee_a_predir_1$marital_status[donnee_a_predir_1$marital_status == "Married-spouse-absent"] <- "Separated"
donnee_a_predir_1$marital_status[donnee_a_predir_1$marital_status == "Widowed"] <- "Separated"
donnee_a_predir_1$marital_status <- as.factor(donnee_a_predir_1$marital_status)


## Recodage de Dapp$age
donnee_a_predir_1$age<-as.numeric(donnee_a_predir_1$age)
for (i in 1:nrow(donnee_a_predir_1)) {
  if((donnee_a_predir_1[i,1]>=15 )&&( donnee_a_predir_1[i,1]<25)) {donnee_a_predir_1[i,1]="15-25"}
  else
    if ((donnee_a_predir_1[i,1]>=25 )&&( donnee_a_predir_1[i,1]<35)) {donnee_a_predir_1[i,1]="25-35"}
  else
    if ((donnee_a_predir_1[i,1]>=35 )&&( donnee_a_predir_1[i,1]<45)) {donnee_a_predir_1[i,1]="35-45"}
  else
    if ((donnee_a_predir_1[i,1]>=45 )&&( donnee_a_predir_1[i,1]<55)) {donnee_a_predir_1[i,1]="45-55"}
  else
    if ((donnee_a_predir_1[i,1]>=55 )&&( donnee_a_predir_1[i,1]<65)) {donnee_a_predir_1[i,1]="55-65"}
  
  else (donnee_a_predir_1[i,1]="age>65")}


donnee_a_predir_1$age <- as.factor(donnee_a_predir_1$age)

summary(donnee_a_predir_1$age)


donnee_a_predir_2<-donnee_a_predir_1
# GAIN ET PERTE
donnee_a_predir_2$capital_gain <- ifelse(donnee_a_predir_2$capital_gain == 0,"pas_gain","gain_oui")
donnee_a_predir_2$capital_loss <- ifelse(donnee_a_predir_2$capital_loss == 0,"pas_perte","perte_oui")

#heure de travail
View(donnee_a_predir_2)
summary(donnee_a_predir_2$hr_per_week)

donnee_a_predir_2$hr_per_week<-as.numeric(donnee_a_predir_2$hr_per_week)
for (i in 1:nrow(donnee_a_predir_2)) {
  if(donnee_a_predir_2[i,11]<30) {donnee_a_predir_2[i,11]="h<30"}
  else
    if ((donnee_a_predir_2[i,11]>=30 )&&( donnee_a_predir_2[i,11]<=50)) {donnee_a_predir_2[i,11]="30<=h<=50"}
  else (donnee_a_predir_2[i,11]="h>50")}


donnee_a_predir_2$hr_per_week<-as.factor(donnee_a_predir_2$hr_per_week)

summary(donnee_a_predir_2$hr_per_week)
#View(donnee_a_predir_2)

summary(donnee_a_predir_2)
colnames(donnee_a_predir_2)

donnee_a_predir_2$age = as.factor(donnee_a_predir_2$age)
donnee_a_predir_2$workclass = as.factor(donnee_a_predir_2$workclass)
donnee_a_predir_2$education = as.factor(donnee_a_predir_2$education)
donnee_a_predir_2$marital_status = as.factor(donnee_a_predir_2$marital_status)
donnee_a_predir_2$occupation = as.factor(donnee_a_predir_2$occupation)
donnee_a_predir_2$relationship = as.factor(donnee_a_predir_2$relationship)
donnee_a_predir_2$race = as.factor(donnee_a_predir_2$race)
donnee_a_predir_2$sex = as.factor(donnee_a_predir_2$sex)
donnee_a_predir_2$capital_gain = as.factor(donnee_a_predir_2$capital_gain)
donnee_a_predir_2$capital_loss = as.factor(donnee_a_predir_2$capital_loss)
donnee_a_predir_2$hr_per_week = as.factor(donnee_a_predir_2$hr_per_week)
donnee_a_predir_2$native_country = as.factor(donnee_a_predir_2$native_country)
donnee_a_predir_2$income = as.factor(donnee_a_predir_2$income)



##### Etude des données :
summary(donnee_a_predir_2)



# 1. ACM sur les variables catégorielles regroupées + variables income en supplémentaire

res.mca3 <- MCA(donnee_a_predir_2, ncp=20,quali.sup=c(13), graph=TRUE)


# data: le tableau de donnée utilisé
# ncp: le nombre de dimensions gardé dans les résultats
# quanti.sup: vecteur des index des variables continues illustratives
# quali.sup: vecteur des index des variables qualitatives illustratives
# graph: TRUE ou FALSE selon que l'on veut ou non afficher les graphes 
summary(res.mca3)
res.mca3$eig
res.mca3$var
res.mca3$ind
dimdesc(res.mca3) # Obtenir une description des dimensions
barplot(res.mca3$eig[,2])

plot.MCA(res.mca3, invisible=c("ind"), cex=0.7) # cex : taille des caractères
plot.MCA(res.mca3, invisible=c("var"), cex=0.7)

plot(res.mca3)
plot(res.mca3, autoLab="y", cex=0.7,select="contrib 10", unselect="transparency")

### Interprétation des axes factoriels (avec toutes les var)
# Dim 1 et Dim2 (Rapp corr ou Coeff corr exprimé en%)
# Rapp corr -> utilisé pour les var quali
# Coeff corr -> utilisé pour les var quanti

# rouge : var quali actives (càd avec modalités)
# vert : var quali supp
# bleu : var quanti supp

# Sert à visualiser quelles sont les variables les plus liées a chacune des dimensions

####################### Interpretation du summary res.mca ##########################

# Dans tableau des valeurs propres "eigenvalues":
# % of var : % d'inertie associée à chaque dimension
# La 1ere dimension explique ...% de l'inertie totale, la 2ème dimension explique ...% de l'inertie totale

# Dans tableau des individus "Individuals":
# 1 ligne = 1 individu
# Pour le 1er individu, la colonne "Dim 1" nous donne sa coordonnée dans la dimension 1
# Pour le 1er individu, la colonne"ctr" nous donne sa contribution à la construction de la 1ere dimension 
# l'individu 1 contribue à ...% à la construction de la dimension 1
# Pour le 1er individu, la colonne "cos2"  nous donne la qualité de la représentation sur l'axe 1 de l'individu
# 0 : l'individu est très mal projetté sur l'axe 1
# 1 : l'individu est très bien projetté sur l'axe 1
# idem pour les autres dimensions
# Par défaut, R calcule l'ACM sur 3 dimensions
# ncp : permet de préciser le nombre d'axes factoriels que l'on veut calculer
# Par défaut, R affiche les résultats de l'ACM pour les 10 premiers individus
# nbelements = 50 : pour afficher les résultats de l'ACM des 50 premiers individus
# nbelements = Inf : pour afficher les résultats de l'ACM de tout l'éch

# Dans tableau des variables qualitatitives actives, càd des modalités des variables qualitatives "Categories"
# 1 ligne = 1 modalité
# Pour la 1ère modalité, Même interprétation de "Dim 1", "ctr", "co2"
# Pour la 1ère modalité, la colonne "v.test" nous donne une valeur test comprise entre -2 et +2 (le plus souvent)
# Si abs(v.test) > 2, alors la coordonée est significativement différente de 0 sur l'axe 1
# Pour la 1ère modalité "...", abs(v.test)>2 donc la coordonnée de cette modalité est significativement positive sur l'axe 1
# Par défaut, R n'affiche que les résulatst de l'ACM que pour 10 premières modalités

# Dans le tableau des variables qualitatives "Categorical variables"
# On a qu'une colonne pour chaque dim et chaque variable : rpz le Rapp corr sur l'axe de la var
# Pour la 1ère variable (qui a plusieurs modalités), la colonne "Dim 1" nous donne le rapport de corrélation sur l'axe 1 de la variable "..."
# Pour la 1ère variable, Rapp corr = ..., globalement la variable est liée (ou non) à la dimension 1
# idem pour les autres dimensions

# Dans le tableau des variables qualitatives supplémentaires "Supplementary categories"
# Idem que pcdt : "Dim 1", "cos2", "v.test"...
# ...Sauf, qu'il n'y a pas les contributions "ctr" car ces variables supplémentaires ne participent pas à la construction des axes

# Dans le tableau des variables qualitatives supplémentaires "Supplementary categorical variables"
# On a qu'une colonne pour chaque dim et chaque variable : rpz le Rapp corr sur l'axe de la var
# Pour la 1ère variable (qui a plusieurs modalités), la colonne "Dim 1" nous donne le rapport de corrélation sur l'axe 1 de la variable "..."
# Pour la 1ère variable, Rapp corr = ..., globalement la variable est liée (ou non) à la dimension 1
# idem pour les autres dimensions

# Dans le tableau des variables quantitatives supplémentaires "Supplementary continuous variables"
# On a qu'une colonne pour chaque dim et chaque variable : rpz le coeff corr sur l'axe de la var
# Pour la 1ère variable, la colonne "Dim 1" nous donne le coefficient de corrélation de la 1ere var avec l'axe 1
# Pour la 1ère variable, coeff de corr = ...entre la variable et la dimension 1
# idem pour les autres dimensions

############################ 2. Classification non supervisée ############################ 

# 2.1 ACM sur les variables catégorielles regroupées + variables income en supplémentaire
res.mca3 <- MCA(donnee_a_predir_2, ncp=20, graph=TRUE)
# data: le tableau de donnée utilisé
# ncp: le nombre de dimensions gardé dans les résultats
# quanti.sup: vecteur des index des variables continues illustratives
# quali.sup: vecteur des index des variables qualitatives illustratives
# graph: TRUE ou FALSE selon que l'on veut ou non afficher les graphes 
summary(res.mca3)
res.mca3$eig
res.mca3$var
res.mca3$ind
dimdesc(res.mca3) # Obtenir une description des dimensions
barplot(res.mca3$eig[,2])

plot.MCA(res.mca3, invisible=c("ind"), cex=0.7) # cex : taille des caractères
plot.MCA(res.mca3, invisible=c("var"), cex=0.7)

plot(res.mca3)
plot(res.mca3, autoLab="y", cex=0.7,select="contrib 10", unselect="transparency")

# 2.2 Construire une nouvelle base de données incluant les composantes de l'ACM
# Selection des variables quantitatives

composante_ACM <- res.mca3$ind$coord
newbase<-cbind(donnee_a_predir_2[,"income"],composante_ACM)

nrow(composante_ACM)

# V1 : income
# income recodee en 1 et 2 pour <=50K et >50K

# 2.3 Obtenir une partition en 100 classes de cette nouvelle base par la méthode des k-moyennes
nbcl = 100
newbase1 <- unique(newbase)
centers.init = newbase1[sample(1:length(newbase1[,1]),nbcl),] 
K = kmeans(newbase1, centers.init)
str(K)

# 2.4 Faire une CAH des centres des classes obtenues au point precedent
centres_k=K$centers



d = dist(centres_k,"euclidean")
cah.ward <- hclust(d,method="ward.D2")


## Pour afficher le barplot et choisir le nombre de classes
plot(cah.ward,hang=-1)
plot(rev(cah.ward$height),type="b")
barplot(cah.ward$height)


classif<-as.hclust(cah.ward)
plot(rev(classif$height),type="h",ylab="hauteurs")


gpe.ward = cutree(cah.ward,k=9)

############################################################################
##############   




# ncp: Nombre_de_dimensions_gardées_apres_ACH
hcpc = HCPC(res.mca3,nb.clust = -1)# Le r�sultat est lourd et ne s'affiche pas car la base initiale est lourde, l'id�ale serait de tirer un �chantillon.


# On obtient 2 petites classes avec chacune 1 indidividu et 1 grosse classe
hcpc$call
hcpc$data.clust
# On obtient ici un tableau des 100 individus fictifs
# On peut lire dans ce tableau les coord de chacun des indiv pour chacune des dimensions conserv?es et la classe qui leur est attribu?e


###############################################
####### Consolidation par une kmeans #########
##############################################

nbcl = 9

split(rownames(K$centers),cutree(cah.ward,k=nbcl))

c=split(1:nrow(K$centers),cutree(cah.ward,k=9))
str(c)
m=matrix(NA,9,ncol(K$centers))
classe<-K$centers
for (k in 1:9){
  if (length(c[[k]])>5) {m[k,]=apply(classe[c[[k]],],2,mean)}
}
mtmp = apply(m,1,sum)
m=m[!is.na(mtmp),]
consol<-kmeans(newbase1,m)

######### Interpretation des classes #########
summary(hcpc$data.clust)

#classe 1 : 929 individus
#classe 2 : 25 individus
#classe 3 : 1077 individus


#### Variable income ####
table(donnee_a_predir_2$income[consol$cluster==1])/length(donnee_a_predir_2$income[consol$cluster==1])
table(donnee_a_predir_2$income[consol$cluster==2])/length(donnee_a_predir_2$income[consol$cluster==2])
table(donnee_a_predir_2$income[consol$cluster==3])/length(donnee_a_predir_2$income[consol$cluster==3])


#### il n'y a aucune classe ou les forts revenus sont sur representes.


##### On calcule la distance euclidienne des nouveaux individus aux centres des classes
# et on ajoute les individus aux classes les plus proches

plot(newbase1,col=consol$cluster,pch=20)





# On doit :
# Réaliser plusieurs ACM différentes, en ne conservant que celle qui propose les meilleurs résultats

############## refaire resum? ACM r?alis?es ici

# Partitionné en 100 classes la base de données Dapp2 (celle où l'ACM est la meilleure : res.mca3) par la méthode des k-means
# On a ainsi réalisé une classification avec l’algorithme des K-means
# On considère ces 100 centres de classes comme des individus fictifs

# On doit réaliser une ACP
# On récupère ces centres de classe et on les utilise pour réaliser la CAH 
# On doit ensuite regarder le nombre optimal de classes (graphique)
# On doit ensuite créer une matrice/variable "moyenne" qui calcule les moyennes de chacune des variables (continues) pour chaque classe
# On consolide avec les K-means

# On peut ensuite caractériser les différentes classes 
# en utilisant les moyennes obtenues pour chaque variable dans chaque classe
# on pourra voir s'il existe des classes dans lesquelles les forts revenus sont en forte proportion

hcpc$data.clust
# Jeu de données des 100 centres de classe + 1 dernière variable qui donne le N° de classe de chacun des individus
hcpc$desc.var
# Description des classes par les variables quali et quanti
# Quand il n'y a pas de résultats c'est que les var ne permettent pas de décrire les classes

# Pour var quali, il y a un test du Chi-deux qui est effectué 
# Ce test pour sert à savoir si ela var quali est liée au découpage en classe construit, 
# Si p.value <5%, alors la var quali est bien liée à la var en classe créée
# Ensuite, on peut regarder chaque modalité de cette var quali et classe par classe
# La classe ... est liée à la modalité ... de la var quali ... 
# La colonne "Cla/Mod" nous indique le pourcentage de var "quali+modalité" qui sont dans la classe k 
# 80 : 80% des villes du sud sont dans la classe 1 (var quali = ville, modalité = sud, classe=1)
# La colonne "Mod/Cla" nous indique le pourcentage de var "quali qui sont dans la classe k" qui sont des "modalité"
# 100 : 100% des villes de la classe 1 sont des villes du sud

# Pour var quanti, se sont des Rapp de corr qui sont calculés
# Ils se trouvent dans la colonne "Eta2"
# Ils nous donnent les liaisons entre les var quanti et les classes
# En dessous, on nous propose un tableau un peu plus détaillé avec ces calculs pour chacune des classes
# Permet d'avoir plus de détail et de comparer les classes entre elles
# On doit regarder principalement :
# la colonne "v.test" qui doit être >2 en valeur absolue pour que la var soit significative
# la colonne "Mean in category" qui nous indique la moyenne de chaque var dans la classe étudiée

hcpc$desc.axes
# Description par les axes factoriels
# "Eta2" nous donne l'intensité de la liaison 
# Eta2 proche de 100 : liaison forte, Eta2 proche de 0 : liaison faible
# abs(v.test)>2 : les coordonées des individus sont significatives
# signe v.test - : les coordonées des individus sont significativement plus faibles que les autres
# signe v.test + : les coordonées des individus sont significativement plus élevés que les autres

hcpc$desc.ind
# Description par les individus
# "$para" nous donne les informations de l'individu le plus proche du centre de la classe étudiée
# "$dist" nous donne les informations de l'individu le plus éloigné du centre des autres classes étudiée

############################ 3. Modélisation ############################ 

# Construction d'un ensemble d'apprentissage (2/3 de l'ech total)



# Analyse discriminante lin?aire et quadratique appliqu?e sur les composantes d'une analyse en facteurs
newbase1 = as.data.frame(newbase1)
names(newbase1)[1]<-"income"

n = nrow(newbase1)
set.seed(123)
appri = sample(1:n,round(2*n/3))
testi =setdiff(1:n, appri)
y_obs = newbase1$income[testi]


############################################################################
##################### MODELE 1 ############################################
# 1. Analyse discriminante linéaire :
par(mfrow=c(2,1))

lda=lda(income~.,data=newbase1) 
y_lda=predict(lda, newbase1)$class
tab=table(y_obs, y_lda[testi])
# Calcul de l'erreur d'apprentissage pour lda
err_lda = sum(tab-diag(diag(tab)))/length(testi)
p_lda = predict(lda, newbase1)$posterior[,2]
boxplot(p_lda[testi]~newbase1$income[testi], main="lda")


##########################################################################
##################### MODELE 2 ##########################################

# 2. Analyse  discriminante quadratique

qda=qda(income~.,data=newbase1) 
y_qda=predict(qda, newbase1)$class
tab=table(y_obs, y_qda[testi])
# Calcul de l'erreur d'apprentissage pour lda
err_qda = sum(tab-diag(diag(tab)))/length(testi)
p_qda = predict(qda, newbase1)$posterior[,2]
boxplot(p_qda[testi]~newbase1$income[testi], main="qda")


##############################################################################
#################### MODELE 3 ################################################

# A partir de la regression logistique on travaille de nouveau avec la base de d?part : Dapp

n = nrow(donnee_a_predir_2)
set.seed(123)
appri1 = sample(1:n,round(2*n/3))
testi1 =setdiff(1:n, appri1)
y_obs = donnee_a_predir_2$income[testi1]
#length(p_glm)
# 3. Régression logistique
glm=glm(income~., family="binomial", data=donnee_a_predir_2, subset=appri1)

p_glm = predict(glm,donnee_a_predir_2,typ="respons")
tab=table(y_obs,p_glm[testi1]>0.5) #risque de bayes
# Calcul de l'erreur d'apprentissage pour glm
err_glm = sum(tab-diag(diag(tab)))/length(testi1)
boxplot(err_glm)

###############################################################################
######################### MODELE 4 ###########################################


# 4. Arbre de decision
rp=rpart(income~.,data=donnee_a_predir_2, subset=appri1)
tab=  table(y_obs,predict(rp,donnee_a_predir_2,typ="class")[testi1])
# Calcul de l'erreur d'apprentissage pour l'arbre de décision
err_rp = sum(tab-diag(diag(tab)))/length(testi1)
boxplot(err_rp/length(testi1)) # A REVOIR



########################################################################
##################### MODELE 5 #########################################
# 5. Méthode des plus proches voisins :
newbase2 <- na.omit(newbase1)

n = nrow(newbase2)
set.seed(123)
appri2 = sample(1:n,round(2*n/3))
testi2 =setdiff(1:n, appri2)
y_obs = newbase2$income[testi2]

sum(is.na(newbase2))
sum(is.na(appri2))
sum(is.na(testi2))

knn=knn(newbase2[appri2,2:21], newbase2[testi2,2:21], newbase2$income[appri2],k=5)
err_knn = (sum(tab)-sum(diag(tab)))/length(testi2)
boxplot(err_knn/length(testi2)) # A REVOIR



# 6. Forets aleatoires :
X = donnee_a_predir_2[appri1,1:12] ; y = donnee_a_predir_2[appri1,13]
m.rf = randomForest(X,y,mtry=2,ntree=100)
tab = table(donnee_a_predir_2[testi1,13],predict(m.rf,donnee_a_predir_2)[testi1])
err_foret_aleatoire = (sum(tab)-sum(diag(tab)))/length(testi1)
boxplot(err_foret_aleatoire/length(testi1)) # A REVOIR

### Créer un tableau regroupant les erreurs (exprim?es en %) des méthodes testées
err = data.frame(cbind(err_lda,err_qda, err_glm,err_rp,err_knn,err_foret_aleatoire))
colnames(err)=c("lda", "qda", "glm", "tree","knn", "foret_aleatoire")

####################################################################
######### CONCLUSION ##############################################

# la regression logistique semble etre le meilleur modele
# En effet, l'erreur est de : 0.1596601, c'est la plus petite erreur parmi toutes les m?thodes ?tudi?es

# Risque de Bayes

############################ 4. Prediction ############################ 

##### 4.1. Prediction des niveaux de salaires #####
# Pour la pr?diction on utilise la base de donn?e Dtest qui ne contient pas la variable "income"
# En effet, le but de ce projet est de pr?dire les salaires des individus de Dtest gr?ce au meilleur mod?le de Dapp
# Le meilleur mod?le est la regression logistique glm
# Avant tout, il nous faut recoder le jeu de donn?es Dtest de la m?me fa?on que Dapp

###### Regroupement  et dicrétisation de certaines variables :
# Packages pour le recodage des variables
# install.packages("questionr")
# library(questionr)
# Mais on peut aussi le faire manuellement
#####
load("Census94_salaires_test.Rdata")

donnee_a_pred<-Dtest
summary(donnee_a_pred)


###### am�nagement de la base
# supression des ligne contenant des donn�es manquantes
w = list()
na.indic = NULL
for (k in 1:ncol(donnee_a_pred)){
  w[[k]] = which(donnee_a_pred[,k]=="?")
  donnee_a_pred[w[[k]],k] = NA
  na.indic=c(na.indic,w[[k]])
}
na.indic = unique(na.indic)
donnee_a_pred = donnee_a_pred[-na.indic,]
summary(donnee_a_pred)
don_conserver<-donnee_a_pred
## Recodage de donnee_a_pred$marital_status
donnee_a_pred$marital_status <- as.character(donnee_a_pred$marital_status)
donnee_a_pred$marital_status[donnee_a_pred$marital_status == "Divorced"] <- "Separated"
donnee_a_pred$marital_status[donnee_a_pred$marital_status == "Married-AF-spouse"] <- "Married"
donnee_a_pred$marital_status[donnee_a_pred$marital_status == "Married-civ-spouse"] <- "Married"
donnee_a_pred$marital_status[donnee_a_pred$marital_status == "Married-spouse-absent"] <- "Separated"
donnee_a_pred$marital_status[donnee_a_pred$marital_status == "Widowed"] <- "Separated"
donnee_a_pred$marital_status <- as.factor(donnee_a_pred$marital_status)


## Recodage de Dapp$age
donnee_a_pred$age<-as.numeric(donnee_a_pred$age)
for (i in 1:nrow(donnee_a_pred)) {
  if((donnee_a_pred[i,1]>=15 )&&( donnee_a_pred[i,1]<25)) {donnee_a_pred[i,1]="15-25"}
  else
    if ((donnee_a_pred[i,1]>=25 )&&( donnee_a_pred[i,1]<35)) {donnee_a_pred[i,1]="25-35"}
  else
    if ((donnee_a_pred[i,1]>=35 )&&( donnee_a_pred[i,1]<45)) {donnee_a_pred[i,1]="35-45"}
  else
    if ((donnee_a_pred[i,1]>=45 )&&( donnee_a_pred[i,1]<55)) {donnee_a_pred[i,1]="45-55"}
  else
    if ((donnee_a_pred[i,1]>=55 )&&( donnee_a_pred[i,1]<65)) {donnee_a_pred[i,1]="55-65"}
  
  else (donnee_a_pred[i,1]="age>65")}


donnee_a_pred$age <- as.factor(donnee_a_pred$age)

summary(donnee_a_pred$age)


donnee_a_pred_2<-donnee_a_pred
# GAIN ET PERTE
donnee_a_pred_2$capital_gain <- ifelse(donnee_a_pred_2$capital_gain == 0,"pas_gain","gain_oui")
donnee_a_pred_2$capital_loss <- ifelse(donnee_a_pred_2$capital_loss == 0,"pas_perte","perte_oui")

#heure de travail

summary(donnee_a_pred_2$hr_per_week)

donnee_a_pred_2$hr_per_week<-as.numeric(donnee_a_pred_2$hr_per_week)
for (i in 1:nrow(donnee_a_pred_2)) {
  if(donnee_a_pred_2[i,11]<30) {donnee_a_pred_2[i,11]="h<30"}
  else
    if ((donnee_a_pred_2[i,11]>=30 )&&( donnee_a_pred_2[i,11]<=50)) {donnee_a_pred_2[i,11]="30<=h<=50"}
  else (donnee_a_pred_2[i,11]="h>50")}


donnee_a_pred_2$hr_per_week<-as.factor(donnee_a_pred_2$hr_per_week)

summary(donnee_a_pred_2$hr_per_week)
#View(donnee_a_pred_2)

summary(donnee_a_pred_2)
colnames(donnee_a_pred_2)

donnee_a_pred_2$age = as.factor(donnee_a_pred_2$age)
donnee_a_pred_2$workclass = as.factor(donnee_a_pred_2$workclass)
donnee_a_pred_2$education = as.factor(donnee_a_pred_2$education)
donnee_a_pred_2$marital_status = as.factor(donnee_a_pred_2$marital_status)
donnee_a_pred_2$occupation = as.factor(donnee_a_pred_2$occupation)
donnee_a_pred_2$relationship = as.factor(donnee_a_pred_2$relationship)
donnee_a_pred_2$race = as.factor(donnee_a_pred_2$race)
donnee_a_pred_2$sex = as.factor(donnee_a_pred_2$sex)
donnee_a_pred_2$capital_gain = as.factor(donnee_a_pred_2$capital_gain)
donnee_a_pred_2$capital_loss = as.factor(donnee_a_pred_2$capital_loss)
donnee_a_pred_2$hr_per_week = as.factor(donnee_a_pred_2$hr_per_week)
donnee_a_pred_2$native_country = as.factor(donnee_a_pred_2$native_country)
donnee_a_pred_2$income = as.factor(donnee_a_pred_2$income)

predi<-predict(glm,newdata=donnee_a_pred_2,type="response")
Dtest2<-as.data.frame(cbind(donnee_a_pred_2,predi))


names(Dtest2)[13]<-"pred_income"
colnames(Dtest2)
####### Pour choisir le seuil s optimal pour dissocier les 2 categoreies
# <50k et >50k

boxp<-boxplot(p_glm[testi1]~donnee_a_predir_2$income[testi1])
summary(boxp)

str(boxp)
summary(boxp$names)
t<-table(donnee_a_predir_2$income[testi1],p_glm[testi1])
str(t)
# on coupe le seuil autour de 0,40

#####################################################################
View(Dtest2)
for (i in 1:nrow(Dtest2)) {
  if(Dtest2[i,13]<0.40) {Dtest2[i,13]="<=50"}
  else (Dtest2[i,13]=">50")}


table(Dtest2$pred_income)

#### 7345 obs. <=50k et 2693 obs. >50k #####

Dtest3<-as.data.frame(Dtest2$pred_income)
names(Dtest3)[1]<-"income"
save(Dtest3, file = "Aina_kiki_sagbe_.RData")

############################################################




