setwd("H:/Documents/big data/test_Gradient_Boosting_sortie")


# extra-trees

# extra-tree

# Installation mxnet : https://mxnet.incubator.apache.org/install/index.html
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
#install.packages("e1071")
library(mxnet)
library(e1071)


# prédiction par Extra-Trees
install.packages('extraTrees')
options( java.parameters = "-Xmx4g" ) # 2g defines 2GB of heap mtry - 1 Gb n'est pas suffisant pour le MNIST
library(extraTrees)

# prédiction par Gradient Boosting
#library(devtools)
#install_github('dmlc/xgboost', subdir='R-package')
#detach(package:xgboost)
install.packages('xgboost')
library(xgboost)
library(Matrix)



M1=matrix(c("nb_num_class","nb_max_depth","nb_nrounds","nb_eta","nb_colsample_bytree","nb_nthread","erreur"),1,7)

write.table(M1, "ressource_suivi_Gradient_Boosting.csv", row.names=F,col.names = F, sep="\t",dec=",", na=" ",append = TRUE)


ancien_suivi <- read.csv("ressource_suivi_Gradient_Boosting.csv", header = T, sep="\t",dec=",", na=" ")
ancien_suivi$nb_numRandomCuts<-as.character(ancien_suivi$nb_numRandomCuts)
ancien_suivi$nb_nodemtry<-as.character(ancien_suivi$nb_nodemtry)
ancien_suivi$nb_numThreads<-as.character(ancien_suivi$nb_numThreads)
ancien_suivi$nb_mtry<-as.character(ancien_suivi$nb_mtry)

ancien_suivi$nb_ntree<-as.character(ancien_suivi$nb_ntree)

ancien_suivi$list_suivi<-paste(ancien_suivi$nb_numRandomCuts,
ancien_suivi$nb_nodemtry,
ancien_suivi$nb_numThreads, 
ancien_suivi$nb_mtry,
ancien_suivi$nb_ntree,sep="_")
                               
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

 list_num_class<- seq(10,100,by=10)
 list_max_depth<- seq(5,50,by=5)
 list_nrounds<- seq(10,100,by=10)
 list_eta<- seq(0.05,0.95,by=0.05)
 list_colsample_bytree<-seq(0.05,0.95,by=0.05)
 list_nthread<- seq(1,10,by=1)

# nb_numRandomCuts<- 0.5
# nb_nodemtry<- 0.07
# nb_numThreads<- 0.9
# nb_mtry<- 100
nb_nrounds<-5

 for (nb_num_class in list_num_class) {
   for (nb_max_depth in list_max_depth) {
 #    for (nb_nrounds in list_nrounds) {
       for (nb_eta in list_eta) {
        for (nb_colsample_bytree in list_colsample_bytree) {
         for (nb_nthread in list_nthread) {
   
        #   nb_hidden_node_2<-nb_hidden_node_1/2
 
                     donnee_a_verifier<-paste(nb_num_class ,
					 nb_max_depth,
					 nb_nrounds ,
					 nb_eta ,
					 nb_colsample_bytree ,
                     nb_nthread,
					 sep="_")


                                         
           if(donnee_a_verifier %in% list_suivi_deja_fait)  { print("deja fait !!!")
           } else {   
                     
# 1er réseau : perceptron à 1 couche cachée
mx.set.seed(0)
# model <- mx.mlp(train.x, train.y, hidden_node=100, out_node=10, out_activation="softmax", num.round=10,
#                 array.batch.mtry=100, learning.rate=0.07, numThreads=0.9, eval.metric = mx.metric.accuracy)
# # 2e réseau : perceptron à 2 couches cachées
# mx.set.seed(0)
# model <- mx.mlp(train.x, train.y, hidden_node=c(100,50), out_node=10, out_activation="softmax", num.round=10,
#                 array.batch.mtry=100, learning.rate=0.07, numThreads=0.9, eval.metric = mx.metric.accuracy)
# avec dropout


# transformation data frames en matrices sparses
train.mx <- sparse.model.matrix(label~., train)
test.mx  <- sparse.model.matrix(label~., test)
# transformation matrices sparses en matrices Xgb
dtrain   <- xgb.DMatrix(train.mx, label=train$label)
dtest    <- xgb.DMatrix(test.mx, label=test$label)
# modélisation
set.seed(235)
system.time(train.gdbt <- xgb.train(params=list(objective="multi:softmax", num_class=nb_num_class, eval_metric="mlogloss", max_depth=nb_max_depth), data=dtrain, nrounds=5, watchlist=list(eval=dtest, train=dtrain)))
#system.time(train.gdbt <- xgb.train(params=list(objective="multi:softmax", num_class=10, eval_metric="mlogloss", eta=0.2, max_depth=5, subsample=1, colsample_bytree=0.5), data=dtrain, nrounds=100, watchlist=list(eval=dtest, train=dtrain)))
#system.time(train.gdbt <- xgb.train(params=list(objective="multi:softmax", num_class=10, eval_metric="mlogloss", max_depth=5, eta=0.1), data=dtrain, nrounds=3000, watchlist=list(eval=dtest, train=dtrain)))
#system.time(train.gdbt <- xgb.train(params=list(objective="multi:softmax", num_class=10, eval_metric="mlogloss", max_depth=5), data=dtrain, nrounds=100))
system.time(train.gdbt <- xgb.train(params=list(booster = "gbtree", objective="multi:softmax", num_class=nb_num_class, eval_metric="mlogloss", eta=nb_eta, gamma=0, max_depth=nb_max_depth, subsample=1, colsample_bytree = nb_colsample_bytree ), data=dtrain, nrounds=5, nthread = nb_nthread , verbose=0))
pred <- predict(train.gdbt, newdata=dtest)
erreur<-(nrow(test)-sum(diag(table(test$label,pred))))/nrow(test)
table(test$label,pred)


M=matrix(c(nb_num_class,nb_max_depth,nb_nrounds,nb_eta,nb_colsample_bytree,nb_nthread,erreur),1,7)

# M2<-rbind(M1,M)
t_sorti<-as.data.frame(M)
write.table(t_sorti, "ressource_suivi_Gradient_Boosting.csv", row.names=F,col.names = F, sep="\t",dec=",", na=" ",append = TRUE)
#				}
           }
        }
      }
    }
  }
}
    
    

