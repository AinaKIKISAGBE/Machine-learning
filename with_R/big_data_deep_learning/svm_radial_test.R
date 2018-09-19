setwd("H:/Documents/big data/test_svm_radial_sortie")


# SVM sur axes factoriels
library(e1071)

# SVM avec noyau radial

# Installation mxnet : https://mxnet.incubator.apache.org/install/index.html
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
#install.packages("e1071")
library(mxnet)
library(e1071)

M1=matrix(c("nb_cost","nb_gamma","erreur"),1,3)
write.table(M1, "ressource_suivi_svm_radial.csv", row.names=F,col.names = F, sep="\t",dec=",", na=" ",append = TRUE)


ancien_suivi <- read.csv("ressource_suivi_svm_radial.csv", header = T, sep="\t",dec=",", na=" ")
ancien_suivi$nb_cost<-as.character(ancien_suivi$nb_cost)
ancien_suivi$nb_gamma<-as.character(ancien_suivi$nb_gamma)

ancien_suivi$list_suivi<-paste( ancien_suivi$list_suivi,
								ancien_suivi$nb_gamma, sep="_")

                               
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

# list_drop_out<- seq(0.1,0.9,by=0.1)
 list_gamma<- seq(0.01,0.09,by=0.01)
# list_momentum<- seq(0.1,0.9,by=0.1)
# list_size<- seq(50,500,by=50)
list_cost<-seq(50,500,by=50)
# nb_drop_out<- 0.5
# nb_learning_rate<- 0.07
# nb_momentum<- 0.9
# nb_size<- 100


# for (nb_drop_out in list_drop_out) {
   for (nb_gamma in list_gamma) {
    # for (nb_momentum in list_momentum) {
     #  for (nb_size in list_size) {
        for (nb_cost in list_cost) {
        #   nb_hidden_node_2<-nb_hidden_node_1/2
 
                     donnee_a_verifier<-paste(nb_cost,nb_gamma, sep="_")
                                         
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


# SVM avec noyau radial
system.time(svmrad <- svm(Xfinal, Y, kernel="radial", probability=TRUE, gamma=nb_gamma, cost=nb_cost))
print(svmrad)
summary(svmrad)
svmrad$rho
pred.svm <- predict(svmrad, newdata=testreduced, decision.values=T)
# taux d'erreur en test
erreur<-sum(pred.svm != test[,1]) / nrow(test)
# matrice de confusion
table(test[,1], pred.svm)



M=matrix(c(nb_cost,nb_gamma,erreur),1,3)

# M2<-rbind(M1,M)
t_sorti<-as.data.frame(M)
write.table(t_sorti, "ressource_suivi_svm_radial.csv", row.names=F,col.names = F, sep="\t",dec=",", na=" ",append = TRUE)
 #          }
 #       }
 #     }
    }
  }
}
    
    

