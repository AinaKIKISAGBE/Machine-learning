### Aina KIKI-SAGBE ###
### Présentation du deep learning avec R  ###
####  et utilisation du package "keras" chargé depuis python #####


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


# chargement de données 

# exemple1, on va prendre des données issues du package keras
# chargement des données MNIST 
mnist <- dataset_mnist()
# chargement des données CIFAR10 
cifar10 <- dataset_cifar10()
# chargement des données IMDB 
imdb <- dataset_imdb()


# exemple2, création de données factice
data <- matrix(rexp(1000*784), nrow = 1000, ncol = 784)
# création de valeurs cible pour mes données factice
labels <- matrix(round(runif(1000*10, min = 0, max = 9)), nrow = 1000, ncol = 10)

# exemple3, importation de données CSV
# importer des données "iris"
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) 
iris_save<-iris
head(iris) # tête 
tail(iris) # pied
str(iris) # structure https://vimeo.com/130411487
dim(iris) # dimension
summary(iris)
#### on va poursuivre l'exemple avec les données "iris"
# on renomme les colonnes
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

# visualisation en deuxdimentionet colorage selon 
# les trois modalité de la variable "cible"="species" 
# qui contien: Iris-setosa,   Iris-versicolor et Iris-virginica 
# et le unclass() convertie les libelé qualitatives et classe numérique 1,2 et 3
# comme on a trois modalité afin que les couleurs puissent être facilement affectées
# selon la modalité
plot(iris$Petal.Length, 
     iris$Petal.Width, 
     pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], 
     xlab="Petal Length", 
     ylab="Petal Width")

# corrélation entre variables numériques
cor(iris[,1:4])
# 
M <- cor(iris[,1:4])
# ??corrplot()
library(corrplot)
corrplot(M, method="circle")


# vérifionc s'il a lieu de normaliser nos donnée:
summary(iris)
# je remarque que mes variables quantitatives ne sont pas 
# trop dispersée et  ne sont pas trop volatil et éloignées 
# les une des autres aussi bien à l_intérieur des variables 
# que à l_extérieur (entre les variavles)
# donc, il n'y a pas besoins de les normaliser.

# cependant, nous allons quand même voir l'impacte que 
# la normalisation pourrait avoir sur nos données et donc 
# sur nos résultats

# normalisation methode 1:
# création de la fonction  "normalize()" 
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# application de la fonction "normalize" à chacune des colonnes
# allant de 1 à 4 de la table "iris"
iris_norm <- as.data.frame(lapply(iris[1:4],normalize))
head(iris)

# comparons les histogrames des deux bases ( normalisée et non-normalisée)
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

# utilisation de la fonction "Normalize" créée plus haut pour normaliser 
iris <- normalize(iris[,1:4])
head(iris)
summary(iris)


### comme il n'y a pas besoin de normaliser ici,
# on peut continuer avec nos données originales
iris<-iris_save

### echantillon test et d'apprentissage
# indice de scission 
# set.seed()
# ind prend la valeur "1" avec la probas 0.67 
# et prend la valeur "2" avec la proba 0.33
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# (variables explicatives) diviser les données iris
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

# (variable cible à expliquer)  diviser l'attribute de class
iris.trainingtarget <- iris[ind==1, 5]
iris.testtarget <- iris[ind==2,5] 
head(iris.trainingtarget)

## petite verification avant de commencer les modèles
# pour voir si on a bien les boléen true false ?
# une valeur cible de l'echantillon d_apprentissage
iris.trainLabels <- to_categorical(iris.trainingtarget)

# une valeur cible de l'echantillon test
iris.testLabels <- to_categorical(iris.testtarget)

# visualisation de iris.testLabels 
print(iris.testLabels)

### debut des modèles
# initialisation sequentiel du model keras
model <- keras_model_sequential() 

# Methode de Perseptron Multi_couche MLP d_ou 
# la connectivité des couches
# ajout d_une couche du modèl
# on a plusieurs types de fonctions d'activation d'une couche à l'autre. 
# ici, on a utilisé la  fonction d_activation "RELU"
model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 3, activation = 'softmax')
# input_shape = c(4) car on a 4 variables explicatives 
# et donc 4 colonnes dans la base d_entrainement 
# contenant uniquement les variables explicatives


# consultons les sommaires et output du modèle exécuté:
summary(model)
# definir la configuration du model
get_config(model)
# definir la couche de configuration
get_layer(model, index = 1)
# Liste des couches du modèle
model$layers
# Liste des tenseurs d'entrée
model$inputs
# Liste les tenseurs de sortie
model$outputs


# maintenant que nous connaissons faire l'architechture du modèle, 
# nous allons maintenant compiler et adapter le modèle à nos données 

# Compilation de model: 
# on précise la fonction de perte avec l_option "loss", 
# la précision de l_apprentissage avec l_option "metrics" 
# et l_optimisation avec l_option "optimizer" 
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# NB: les choix des options dépendent de ce qu'on veut faire.
# exemple: pour la regression avec neurone, 
# l_option de la fonction de perte "loss=MSE" 
# minimisation des erreurs quadratiques  
# tandisque pour la classification multi-classe comme ici,
# on mat l_option "loss=categorical_crossentropy" 
# mais si on avait une classification binaire, 
# on utiliserait "loss=binary_crossentropy"
# ... etc   ...


### adaptation de modèle, 
# on va former 200 modèles ou itérations sur tous 
# les échantillons "train et cible_train" par lot de 5 
# echantillons 
 
# Mise en place du model 
model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 200, 
  batch_size = 5, 
  validation_split = 0.2
  )
# pour voir la barre de progression, on spécifie dans 
# le fit(), l_option "verbose =1" 


# on stock le modèle dans l'objet "history" 
history <- model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 200,
  batch_size = 5, 
  validation_split = 0.2
)

# grapf (tracé) de "history"
plot(history)
# ce graph est difficile à interpréter:
# il faut savoir que : le losset accindiquent la perte 
# et la précision du modèle pour les données de formation,
# alors que la val_losset val_accsont les mêmes mesures,
# la perte et la précision, pour les données de test ou 
# de validation

# le graph etant difficile à interpréter, on va le scinder en 2:
# un pour les perte et un autre pour la précision

# Tracer la perte de modèle des données d'entraînement
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")
# Tracer la perte de modèle des données de test
lines(history$metrics$val_loss, col="green")
# ajout de legend
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
# on observe les tracé des pertes sur le graph


# on fait de même pour la précision:
# Tracer la précision du modèle des données d'entraînement
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")
# Tracer la précision du modèle des données de test
lines(history$metrics$val_acc, col="green")
# ajout de Legend
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
# comprendre la lecture du graph:
# Certaines choses à garder à l'esprit ici sont les suivantes:
# 
# Si la précision de vos données d'entraînement continue 
# de s'améliorer alors que la précision de vos données de 
# validation s'aggrave, vous êtes probablement en train de sur-apprendre: 
# votre modèle commence simplement à mémoriser les données 
# au lieu d'en tirer des leçons.
# 
# Si la tendance d'exactitude sur les deux ensembles de 
# données augmente  toujours pour les dernières époques,
# vous pouvez clairement voir que le modèle n'a pas encore 
# sur-appris de l'ensemble de données d'entraînement.


### prediction
# Maintenant que votre modèle est créé, compilé et a été 
# adapté aux données, il est temps d'utiliser réellement 
# votre modèle pour prédire les étiquettes pour votre ensemble
# de test

# Predire les classes pour les données de test
classes <- model %>% predict_classes(iris.test, batch_size = 128)
#  matrice de confusion
table(iris.testtarget, classes)


### evaluation du modèl
# en plus de la matice de confusion, il convien de l'evaluer d'avantage 
# Evaluer les résultat de test
score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)
# affichier les score
print(score)

# Ensuite, après impression du score, vous récupérerez 
# la valeur de la perte et la valeur métrique (dans ce cas 'accuracy') 
# en arrière.


# Réglage fin de votre modèle

# il y a déjà deux décisions importantes que vous voudrez 
# probablement régler: combien de couches  vous allez 
# utiliser et combien « unités cachées » vous a choisi pour 
# chaque couche.

# Aussi, En plus de jouer avec le nombre d'époques ou 
# de la taille des lots, il y a d' autres façons dont vous 
# pouvez modifier votre modèle dans l'espoir qu'il 
# fonctionnera mieux: en ajoutant des couches, en augmentant 
# le nombre d'unités cachées et en passant votre propre 
# optimisation paramètres à la compile()fonction. 

# Ajout de couches
# initialiser le model séquentiel
model <- keras_model_sequential() 

# ajouter des couches à modeler
# dans notre exemple, on a ajouter une deuxiemme 
# couche (sous-couche) à l'ancienne exiatante 
# "layer_dense(units = 5, activation = 'relu') %>%"
# ce qui fait un degré de profondeur=2
model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 5, activation = 'relu') %>% 
  layer_dense(units = 3, activation = 'softmax')

# on compile le modèle
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# on lance le modèle sur nos données d'pprentissage
model %>% fit(
  iris.training, iris.trainLabels, 
  epochs = 200, batch_size = 5, 
  validation_split = 0.2
)


# on evalue le modèle sur nos données test
score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)
# on affiche les score
print(score)


# on peu également visualiser les paramètres de perte et
# la précision de ce nouveau modèle

# sauvegarder l'historique du modèle dans un objet "history" 
history <- model %>% fit(
  iris.training, iris.trainLabels, 
  epochs = 200, batch_size = 5,
  validation_split = 0.2
)

# graph des perte du modèle
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")
lines(history$metrics$val_loss, col="green")
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# graph précision du modèle
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")
lines(history$metrics$val_acc, col="green")
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))



### on peut aussi ajouter des unité caché ou noeud caché
# par exemple, on desir ajouter 20 unité caché sur la première couche,
# alors, unitspasse de 8 à (8+20=28)

model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 3, activation = 'softmax')
# ensuite, on évalue le modèle et on trace aussi ses coubres.



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
# ensuite, on évalue le modèle et on trace aussi ses coubres.


# En plus d'utiliser un autre optimiseur, vous pouvez 
# également essayer d'utiliser un plus petit taux 
# d'apprentissage pour former votre réseau. Ceci est 
# l'une des plus courantes techniques de réglage fin;
# 
# Une pratique courante consiste à rendre le taux 
# d'apprentissage initial 10 fois plus petit que celui
# que vous avez utilisé pour former le modèle avant.



### Enregistrement, chargement ou exportation de votre modèle 
# avec la package keras

# sauvegarder pour pouvoir etre utilisé putard sans avoir à re-apprendre
save_model_hdf5(model, "my_model.h5")

# charger pour l'utiliser directement (gain de temps)
model <- load_model_hdf5("my_model.h5")


# vous pouvez également enregistrer et charger les poids 
# du modèle avec les save_model_weights_hdf5()et 
# load_model_weights_hdf5()fonctions:
save_model_weights_hdf5("my_model_weights.h5")
model %>% load_model_weights_hdf5("my_model_weights.h5")


# vous pouvez également exporter la configuration de votre 
# modèle JSON ou YAML. Ici, les fonctions model_to_json()
# et model_to_yaml()vous aider. Pour charger les configurations
# de nouveau dans votre espace de travail, vous pouvez 
# simplement utiliser model_from_json()et model_from yaml()
# fonctions:
json_string <- model_to_json(model)
model <- model_from_json(json_string)

yaml_string <- model_to_yaml(model)
model <- model_from_yaml(yaml_string)










