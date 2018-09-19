setwd("H:/Documents/big data/test_mxnet_sortie")

# ---------------------------------------------------------------------------------------------------------
# Réseaux de neurones à convolution : MNIST avec MXNet 
# ---------------------------------------------------------------------------------------------------------

# Installation mxnet : https://mxnet.incubator.apache.org/install/index.html
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
library(mxnet)

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
list_hidden_node_1<-seq(64,640,by=64)
list_mx.init.uniform<-seq(0.01,0.09,by=0.01)
# nb_drop_out<- 0.5
# nb_learning_rate<- 0.07
# nb_momentum<- 0.9
# nb_size<- 100

M1=matrix(c("nb_mx.init.uniform","nb_learning_rate","nb_momentum","nb_size","nb_hidden_node_1","erreur"),1,5)
write.table(M1, "ressource_suivi_mxnet.csv", row.names=F,col.names = F, sep="\t",dec=",", na=" ",append = TRUE)

# for (nb_drop_out in list_drop_out) {
  for (nb_learning_rate in list_learning_rate) {
    for (nb_momentum in list_momentum) {
      for (nb_size in list_size) {
        for (nb_hidden_node_1 in list_hidden_node_1) {
          for (nb_mx.init.uniform in list_mx.init.uniform) {
            
          nb_hidden_node_2<-nb_hidden_node_1/2
    
# 1er réseau : perceptron à 1 couche cachée
mx.set.seed(0)
# model <- mx.mlp(train.x, train.y, hidden_node=100, out_node=10, out_activation="softmax", num.round=10,
#                 array.batch.size=100, learning.rate=0.07, momentum=0.9, eval.metric = mx.metric.accuracy)
# # 2e réseau : perceptron à 2 couches cachées
# mx.set.seed(0)
# model <- mx.mlp(train.x, train.y, hidden_node=c(100,50), out_node=10, out_activation="softmax", num.round=10,
#                 array.batch.size=100, learning.rate=0.07, momentum=0.9, eval.metric = mx.metric.accuracy)
# avec dropout

# autre façon de spécifier un perceptron multi-couches
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=nb_hidden_node_1)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=nb_hidden_node_2)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=10)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")

devices <- mx.cpu()
mx.set.seed(0)
ptm <- proc.time()
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y, ctx=devices, num.round=5, array.batch.size=nb_size, 
                                     learning.rate=nb_learning_rate, momentum=nb_momentum, eval.metric=mx.metric.accuracy, initializer=mx.init.uniform(nb_mx.init.uniform), epoch.end.callback=mx.callback.log.train.metric(100))






preds <- predict(model, test.x)
dim(preds) # 1 ligne par valeur à prédire - 1 colonne par observation
#preds[,1:20]
# après transposition, on a une ligne par observation, et une colonne par valeur de 0 à 9 (la colonne 1 correspond à 0...)
pred.label <- max.col(t(preds)) - 1 
erreur<-(nrow(test)-sum(diag(table(test$label,pred.label))))/nrow(test) # taux d'erreur
#table(test$label,pred.label) # matrice de confusion

M=matrix(c(nb_mx.init.uniform,nb_learning_rate,nb_momentum,nb_size,nb_hidden_node_1,erreur),1,5)

# M2<-rbind(M1,M)
t_sorti<-as.data.frame(M)
write.table(t_sorti, "ressource_suivi_mxnet.csv", row.names=F,col.names = F, sep="\t",dec=",", na=" ",append = TRUE)
        }
      }
    }
  }
}
    
    

