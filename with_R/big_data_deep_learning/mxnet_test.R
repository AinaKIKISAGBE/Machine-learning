setwd("H:/Documents/big data/test_mxnet_sortie")

# ---------------------------------------------------------------------------------------------------------
# Réseaux de neurones à convolution : MNIST avec MXNet 
# ---------------------------------------------------------------------------------------------------------

# Installation mxnet : https://mxnet.incubator.apache.org/install/index.html
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
#install.packages("mxnet")
library(mxnet)

ancien_suivi <- read.csv("ressource_suivi_mxnet.csv", header = T, sep="\t",dec=",", na=" ")
ancien_suivi$nb_drop_out<-as.character(ancien_suivi$nb_drop_out)
ancien_suivi$nb_learning_rate<-as.character(ancien_suivi$nb_learning_rate)
ancien_suivi$nb_momentum<-as.character(ancien_suivi$nb_momentum)
ancien_suivi$nb_size<-as.character(ancien_suivi$nb_size)
ancien_suivi$nb_hidden_node_1<-as.character(ancien_suivi$nb_hidden_node_1)

ancien_suivi$list_suivi<-paste(ancien_suivi$nb_drop_out,
                               ancien_suivi$nb_learning_rate,
                               ancien_suivi$nb_momentum,
                               ancien_suivi$nb_size,
                               ancien_suivi$nb_hidden_node_1,sep="_")

list_suivi_deja_fait<-ancien_suivi$list_suivi

train <- read.csv("mnist_train.csv", header=TRUE)
dim(train)
test <- read.csv("mnist_test.csv", header=TRUE)
dim(test)

# affichage des 30 premiers chiffres
par(mfrow=c(5,6))
par(mar=c(2,2,2,2))
for (i in (1:30)) {
  im <- matrix(data=as.numeric((train[i,-1]>0)), nrow=28, ncol=28)
  rim  <- matrix(NA,28,28)
  for (j in 1:28) {rim[,j] <- im[,29-j]}
  image(1:28, 1:28, rim, col=gray((0:255)/255), xlab=" ", ylab=" ", axes=F)
  title(main = train[i,1])
}

# préparation des données pour mxnet
train.x <- train[,-1]
train.y <- train[,1]
train.x <- t(train.x/255)
test.x  <- test[,-1]
test.x <- t(test.x/255) # 784 lignes et 10000 colonnes, valeurs entre 0 et 1

list_drop_out<- seq(0.1,0.9,by=0.1)
list_learning_rate<- seq(0.01,0.09,by=0.01)
list_momentum<- seq(0.1,0.9,by=0.1)
list_size<- seq(50,500,by=50)
list_hidden_node_1<-seq(50,500,by=50)
# nb_drop_out<- 0.5
# nb_learning_rate<- 0.07
# nb_momentum<- 0.9
# nb_size<- 100

M1=matrix(c("nb_drop_out","nb_learning_rate","nb_momentum","nb_size","nb_hidden_node_1","erreur"),1,6)
write.table(M1, "ressource_suivi_mxnet.csv", row.names=F,col.names = F, sep="\t",dec=",", na=" ",append = TRUE)

for (nb_drop_out in list_drop_out) {
  for (nb_learning_rate in list_learning_rate) {
    for (nb_momentum in list_momentum) {
      for (nb_size in list_size) {
        for (nb_hidden_node_1 in list_hidden_node_1) {
          nb_hidden_node_2<-nb_hidden_node_1/2
 
                     donnee_a_verifier<-paste(nb_drop_out,
                                         nb_learning_rate,
                                         nb_momentum,
                                         nb_size,
                                         nb_hidden_node_1,sep="_")
                  
           if(donnee_a_verifier %in% list_suivi_deja_fait)  { print("deja fait !!!")
           } else {   
                     
# 1er réseau : perceptron à 1 couche cachée
mx.set.seed(0)
# model <- mx.mlp(train.x, train.y, hidden_node=100, out_node=10, out_activation="softmax", num.round=10,
#                 array.batch.size=100, learning.rate=0.07, momentum=0.9, eval.metric = mx.metric.accuracy)
# # 2e réseau : perceptron à 2 couches cachées
# mx.set.seed(0)
# model <- mx.mlp(train.x, train.y, hidden_node=c(100,50), out_node=10, out_activation="softmax", num.round=10,
#                 array.batch.size=100, learning.rate=0.07, momentum=0.9, eval.metric = mx.metric.accuracy)
# avec dropout
model <- mx.mlp(train.x, train.y, hidden_node=c(nb_hidden_node_1,nb_hidden_node_2), out_node=10, out_activation="softmax", num.round=5, dropout=nb_drop_out,
                array.batch.size=nb_size, learning.rate=nb_learning_rate, momentum=nb_momentum, eval.metric = mx.metric.accuracy)

preds <- predict(model, test.x)
dim(preds) # 1 ligne par valeur à prédire - 1 colonne par observation
#preds[,1:20]
# après transposition, on a une ligne par observation, et une colonne par valeur de 0 à 9 (la colonne 1 correspond à 0...)
pred.label <- max.col(t(preds)) - 1 
erreur<-(nrow(test)-sum(diag(table(test$label,pred.label))))/nrow(test) # taux d'erreur
#table(test$label,pred.label) # matrice de confusion

M=matrix(c(nb_drop_out,nb_learning_rate,nb_momentum,nb_size,nb_hidden_node_1,erreur),1,6)

# M2<-rbind(M1,M)
t_sorti<-as.data.frame(M)
write.table(t_sorti, "ressource_suivi_mxnet.csv", row.names=F,col.names = F, sep="\t",dec=",", na=" ",append = TRUE)
           }
        }
      }
    }
  }
}
    
    

