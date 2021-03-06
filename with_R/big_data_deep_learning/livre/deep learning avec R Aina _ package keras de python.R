### Aina KIKI-SAGBE ###
### Pr�sentation du deep learning avec R  ###
####  et utilisation du package "keras" charg� depuis python #####


# lien fort de ce documen :
# https://www.datacamp.com/community/tutorials/keras-r-deep-learning

# lien utiles: 
#   https://www.datacamp.com/community/tutorials/keras-r-deep-learning
#   https://keras.rstudio.com/
#   https://www.datacamp.com/courses/
#   https://www.datacamp.com/community/tutorials/deep-learning-python
#   https://www.datacamp.com/community/blog/keras-cheat-sheet
#   https://www.manning.com/books/deep-learning-with-r
#   http://www.r-tutor.com/deep-learning/introduction
#   https://www.r-bloggers.com/deep-learning-in-r-2/
  
# installation du package keras
devtools::install_github("rstudio/keras")
# ou
install.packages("keras")

# Chargment du package keras 
library(keras)
install_keras()
# Installation du package TensorFlow
#install_tensorflow()
install.packages("tensorflow")
#??TensorFlow()
library(tensorflow)


# chargement de donn�es 

# exemple1, on va prendre des donn�es issues du package keras
# chargement des donn�es MNIST 
mnist <- dataset_mnist()
# chargement des donn�es CIFAR10 
cifar10 <- dataset_cifar10()
# chargement des donn�es IMDB 
imdb <- dataset_imdb()


# exemple2, cr�ation de donn�es factice
data <- matrix(rexp(1000*784), nrow = 1000, ncol = 784)
# cr�ation de valeurs cible pour mes donn�es factice
labels <- matrix(round(runif(1000*10, min = 0, max = 9)), nrow = 1000, ncol = 10)

# exemple3, importation de donn�es CSV
# importer des donn�es "iris"
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) 
iris_save<-iris
head(iris) # t�te 
tail(iris) # pied
str(iris) # structure https://vimeo.com/130411487
dim(iris) # dimension
summary(iris)
#### on va poursuivre l'exemple avec les donn�es "iris"
# on renomme les colonnes
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

# visualisation en deuxdimentionet colorage selon 
# les trois modalit� de la variable "cible"="species" 
# qui contien: Iris-setosa,   Iris-versicolor et Iris-virginica 
# et le unclass() convertie les libel� qualitatives et classe num�rique 1,2 et 3
# comme on a trois modalit� afin que les couleurs puissent �tre facilement affect�es
# selon la modalit�
plot(iris$Petal.Length, 
     iris$Petal.Width, 
     pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], 
     xlab="Petal Length", 
     ylab="Petal Width")

# corr�lation entre variables num�riques
cor(iris[,1:4])
# 
M <- cor(iris[,1:4])
# ??corrplot()
library(corrplot)
corrplot(M, method="circle")


# v�rifionc s'il a lieu de normaliser nos donn�e:
summary(iris)
# je remarque que mes variables quantitatives ne sont pas 
# trop dispers�e et  ne sont pas trop volatil et �loign�es 
# les une des autres aussi bien � l_int�rieur des variables 
# que � l_ext�rieur (entre les variavles)
# donc, il n'y a pas besoins de les normaliser.

# cependant, nous allons quand m�me voir l'impacte que 
# la normalisation pourrait avoir sur nos donn�es et donc 
# sur nos r�sultats

# normalisation methode 1:
# cr�ation de la fonction  "normalize()" 
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# application de la fonction "normalize" � chacune des colonnes
# allant de 1 � 4 de la table "iris"
iris_norm <- as.data.frame(lapply(iris[1:4],normalize))
head(iris)

# comparons les histogrames des deux bases ( normalis�e et non-normalis�e)
hist(iris$Sepal.Length)
hist(iris_norm$Sepal.Length)

hist(iris$Sepal.Width)
hist(iris_norm$Sepal.Width)
hist(iris$Petal.Length)
hist(iris_norm$Petal.Length)
hist(iris$Petal.Width)
hist(iris_norm$Petal.Width)

# normalisation methode 2 :
# ici, on va utiliser keras et donc convertir "iris" 
# en matrice car keras travail avec les matrices et non 
# les data_frame

iris[,5] <- as.numeric(iris[,5]) -1

# convertissons en matrix
iris <- as.matrix(iris)
# enlever les label de dimension (pour enlever les nom de colonnes)
dimnames(iris) <- NULL

# utilisation de la fonction "Normalize" cr��e plus haut pour normaliser 
iris <- normalize(iris[,1:4])
head(iris)
summary(iris)


### comme il n'y a pas besoin de normaliser ici,
# on peut continuer avec nos donn�es originales
iris<-iris_save

### echantillon test et d'apprentissage
# indice de scission 
# set.seed()
# ind prend la valeur "1" avec la probas 0.67 
# et prend la valeur "2" avec la proba 0.33
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# (variables explicatives) diviser les donn�es iris
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

# (variable cible � expliquer)  diviser l'attribute de class
iris.trainingtarget <- iris[ind==1, 5]
iris.testtarget <- iris[ind==2,5] 
head(iris.trainingtarget)

## petite verification avant de commencer les mod�les
# pour voir si on a bien les bol�en true false ?
# une valeur cible de l'echantillon d_apprentissage
iris.trainLabels <- to_categorical(iris.trainingtarget)

# une valeur cible de l'echantillon test
iris.testLabels <- to_categorical(iris.testtarget)

# visualisation de iris.testLabels 
print(iris.testLabels)

### debut des mod�les
# initialisation sequentiel du model keras
model <- keras_model_sequential() 

# Methode de Perseptron Multi_couche MLP d_ou 
# la connectivit� des couches
# ajout d_une couche du mod�l
# on a plusieurs types de fonctions d'activation d'une couche � l'autre. 
# ici, on a utilis� la  fonction d_activation "RELU"
model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 3, activation = 'softmax')
# input_shape = c(4) car on a 4 variables explicatives 
# et donc 4 colonnes dans la base d_entrainement 
# contenant uniquement les variables explicatives


# consultons les sommaires et output du mod�le ex�cut�:
summary(model)
# definir la configuration du model
get_config(model)
# definir la couche de configuration
get_layer(model, index = 1)
# Liste des couches du mod�le
model$layers
# Liste des tenseurs d'entr�e
model$inputs
# Liste les tenseurs de sortie
model$outputs


# maintenant que nous connaissons faire l'architechture du mod�le, 
# nous allons maintenant compiler et adapter le mod�le � nos donn�es 

# Compilation de model: 
# on pr�cise la fonction de perte avec l_option "loss", 
# la pr�cision de l_apprentissage avec l_option "metrics" 
# et l_optimisation avec l_option "optimizer" 
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# NB: les choix des options d�pendent de ce qu'on veut faire.
# exemple: pour la regression avec neurone, 
# l_option de la fonction de perte "loss=MSE" 
# minimisation des erreurs quadratiques  
# tandisque pour la classification multi-classe comme ici,
# on mat l_option "loss=categorical_crossentropy" 
# mais si on avait une classification binaire, 
# on utiliserait "loss=binary_crossentropy"
# ... etc   ...


### adaptation de mod�le, 
# on va former 200 mod�les ou it�rations sur tous 
# les �chantillons "train et cible_train" par lot de 5 
# echantillons 
 
# Mise en place du model 
model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 200, 
  batch_size = 5, 
  validation_split = 0.2
  )
# pour voir la barre de progression, on sp�cifie dans 
# le fit(), l_option "verbose =1" 


# on stock le mod�le dans l'objet "history" 
history <- model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 200,
  batch_size = 5, 
  validation_split = 0.2
)

# grapf (trac�) de "history"
plot(history)
# ce graph est difficile � interpr�ter:
# il faut savoir que : le losset accindiquent la perte 
# et la pr�cision du mod�le pour les donn�es de formation,
# alors que la val_losset val_accsont les m�mes mesures,
# la perte et la pr�cision, pour les donn�es de test ou 
# de validation

# le graph etant difficile � interpr�ter, on va le scinder en 2:
# un pour les perte et un autre pour la pr�cision

# Tracer la perte de mod�le des donn�es d'entra�nement
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")
# Tracer la perte de mod�le des donn�es de test
lines(history$metrics$val_loss, col="green")
# ajout de legend
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
# on observe les trac� des pertes sur le graph


# on fait de m�me pour la pr�cision:
# Tracer la pr�cision du mod�le des donn�es d'entra�nement
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")
# Tracer la pr�cision du mod�le des donn�es de test
lines(history$metrics$val_acc, col="green")
# ajout de Legend
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
# comprendre la lecture du graph:
# Certaines choses � garder � l'esprit ici sont les suivantes:
# 
# Si la pr�cision de vos donn�es d'entra�nement continue 
# de s'am�liorer alors que la pr�cision de vos donn�es de 
# validation s'aggrave, vous �tes probablement en train de sur-apprendre: 
# votre mod�le commence simplement � m�moriser les donn�es 
# au lieu d'en tirer des le�ons.
# 
# Si la tendance d'exactitude sur les deux ensembles de 
# donn�es augmente  toujours pour les derni�res �poques,
# vous pouvez clairement voir que le mod�le n'a pas encore 
# sur-appris de l'ensemble de donn�es d'entra�nement.


### prediction
# Maintenant que votre mod�le est cr��, compil� et a �t� 
# adapt� aux donn�es, il est temps d'utiliser r�ellement 
# votre mod�le pour pr�dire les �tiquettes pour votre ensemble
# de test

# Predire les classes pour les donn�es de test
classes <- model %>% predict_classes(iris.test, batch_size = 128)
#  matrice de confusion
table(iris.testtarget, classes)


### evaluation du mod�l
# en plus de la matice de confusion, il convien de l'evaluer d'avantage 
# Evaluer les r�sultat de test
score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)
# affichier les score
print(score)

# Ensuite, apr�s impression du score, vous r�cup�rerez 
# la valeur de la perte et la valeur m�trique (dans ce cas 'accuracy') 
# en arri�re.


# R�glage fin de votre mod�le

# il y a d�j� deux d�cisions importantes que vous voudrez 
# probablement r�gler: combien de couches  vous allez 
# utiliser et combien � unit�s cach�es � vous a choisi pour 
# chaque couche.

# Aussi, En plus de jouer avec le nombre d'�poques ou 
# de la taille des lots, il y a d' autres fa�ons dont vous 
# pouvez modifier votre mod�le dans l'espoir qu'il 
# fonctionnera mieux: en ajoutant des couches, en augmentant 
# le nombre d'unit�s cach�es et en passant votre propre 
# optimisation param�tres � la compile()fonction. 

# Ajout de couches
# initialiser le model s�quentiel
model <- keras_model_sequential() 

# ajouter des couches � modeler
# dans notre exemple, on a ajouter une deuxiemme 
# couche (sous-couche) � l'ancienne exiatante 
# "layer_dense(units = 5, activation = 'relu') %>%"
# ce qui fait un degr� de profondeur=2
model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 5, activation = 'relu') %>% 
  layer_dense(units = 3, activation = 'softmax')

# on compile le mod�le
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# on lance le mod�le sur nos donn�es d'pprentissage
model %>% fit(
  iris.training, iris.trainLabels, 
  epochs = 200, batch_size = 5, 
  validation_split = 0.2
)


# on evalue le mod�le sur nos donn�es test
score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)
# on affiche les score
print(score)


# on peu �galement visualiser les param�tres de perte et
# la pr�cision de ce nouveau mod�le

# sauvegarder l'historique du mod�le dans un objet "history" 
history <- model %>% fit(
  iris.training, iris.trainLabels, 
  epochs = 200, batch_size = 5,
  validation_split = 0.2
)

# graph des perte du mod�le
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")
lines(history$metrics$val_loss, col="green")
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# graph pr�cision du mod�le
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")
lines(history$metrics$val_acc, col="green")
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))



### on peut aussi ajouter des unit� cach� ou noeud cach�
# par exemple, on desir ajouter 20 unit� cach� sur la premi�re couche,
# alors, unitspasse de 8 � (8+20=28)

model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 3, activation = 'softmax')
# ensuite, on �value le mod�le et on trace aussi ses coubres.



### on peut aussi changer l'algorithme d'optimisation,
# par exemple, aulieu d'utiliser l'algorithme 'adam', 
# on peut utiliser celui du descent du gradient
# definition de l'option de descente de gradient 
# avec la fonction: optimizer_sgd(lr=...)
sgd <- optimizer_sgd(lr = 0.01)

# Use the optimizer to compile the model
model %>% compile(optimizer=sgd, 
                  loss='categorical_crossentropy', 
                  metrics='accuracy')
# ensuite, on �value le mod�le et on trace aussi ses coubres.


# En plus d'utiliser un autre optimiseur, vous pouvez 
# �galement essayer d'utiliser un plus petit taux 
# d'apprentissage pour former votre r�seau. Ceci est 
# l'une des plus courantes techniques de r�glage fin;
# 
# Une pratique courante consiste � rendre le taux 
# d'apprentissage initial 10 fois plus petit que celui
# que vous avez utilis� pour former le mod�le avant.



### Enregistrement, chargement ou exportation de votre mod�le 
# avec la package keras

# sauvegarder pour pouvoir etre utilis� putard sans avoir � re-apprendre
save_model_hdf5(model, "my_model.h5")

# charger pour l'utiliser directement (gain de temps)
model <- load_model_hdf5("my_model.h5")


# vous pouvez �galement enregistrer et charger les poids 
# du mod�le avec les save_model_weights_hdf5()et 
# load_model_weights_hdf5()fonctions:
save_model_weights_hdf5("my_model_weights.h5")
model %>% load_model_weights_hdf5("my_model_weights.h5")


# vous pouvez �galement exporter la configuration de votre 
# mod�le JSON ou YAML. Ici, les fonctions model_to_json()
# et model_to_yaml()vous aider. Pour charger les configurations
# de nouveau dans votre espace de travail, vous pouvez 
# simplement utiliser model_from_json()et model_from yaml()
# fonctions:
json_string <- model_to_json(model)
model <- model_from_json(json_string)

yaml_string <- model_to_yaml(model)
model <- model_from_yaml(yaml_string)










