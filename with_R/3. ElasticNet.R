# Importation et préparation des données
setwd("H:/PROJET")
BDD_app<-read.csv("Base_TRAIN.csv", header=TRUE, sep=";")
BDD_test<-read.csv("Base_TEST.csv", header=TRUE, sep=";")

BDD_app<-BDD_app[,33:73]
BDD_app<-BDD_app[,-2]

BDD_test<-BDD_test[,33:73]
BDD_test<-BDD_test[,-2]

install.packages("ROCR")
library(ROCR)
install.packages("glmnet")
library(glmnet)

################################################################################################
##################################### Regression logistique ####################################
################################################################################################

##################################### Base d'apprentissage #####################################

reg <- glm(Ynum ~ ., data = BDD_app, family = binomial(logit))
summary(reg)

# Problème de modalité de référence
BDD_app<-BDD_app[,-c(4,6,8,10,13,17,20,24,27,32,36,40)]

# regression logistique
reg <- glm(Ynum ~ ., data = BDD_app, family = binomial(logit))
summary(reg)

#Supression de maniere iterative des variables non significatives
BDD_app<-BDD_app[,-4]
BDD_app<-BDD_app[,-26]
BDD_app<-BDD_app[,-16]
BDD_app<-BDD_app[,-24]
BDD_app<-BDD_app[,-2]
BDD_app<-BDD_app[,-21]
BDD_app<-BDD_app[,-21]

reg <- glm(Ynum ~ ., data = BDD_app, family = binomial(logit))
summary(reg)

#auc
AAA <- predict(reg, newdata = BDD_app, type = "response")
pred11=prediction(AAA, BDD_app$Ynum)
L=performance(pred11, "tpr", "fpr")
auc=performance(pred11, "auc")@y.values[[1]]
auc
plot(unlist(L@x.values),unlist(L@y.values),xlab="TFP (1-specificite)", ylab="TVP (sensibilite)", main="Courbe ROC - Regression logistique (base d'apprentissage)",type="s",col="blue")

##################################### Base test #####################################

BDD_test<-BDD_test[,-c(4,6,8,10,13,17,20,24,27,32,36,40)]
reg <- glm(Ynum ~ ., data = BDD_test, family = binomial(logit))
summary(reg)

#Supression de maniere iterative des variables non significatives
BDD_test<-BDD_test[,-4]
BDD_test<-BDD_test[,-26]
BDD_test<-BDD_test[,-16]
BDD_test<-BDD_test[,-24]
BDD_test<-BDD_test[,-2]
BDD_test<-BDD_test[,-21]
BDD_test<-BDD_test[,-21]

reg <- glm(Ynum ~ ., data = BDD_test, family = binomial(logit))
summary(reg)

#auc
AAA <- predict(reg, newdata = BDD_test, type = "response")
pred12=prediction(AAA, BDD_test$Ynum)
L=performance(pred12, "tpr", "fpr")
auc=performance(pred12, "auc")@y.values[[1]]
auc
plot(unlist(L@x.values),unlist(L@y.values),xlab="TFP (1-specificite)", ylab="TVP (sensibilite)", main="Courbe ROC - Regression logistique (base test)",type="s",col="blue")

#######################################################################################
##################################### Elastic Net #####################################
#######################################################################################

##################################### alpha = 0.5 #####################################

# Variables expliquée et explicatives

BDD_app<-read.csv("Base_TRAIN.csv", header=TRUE, sep=";")
BDD_test<-read.csv("Base_TEST.csv", header=TRUE, sep=";")

BDD_app<-BDD_app[,33:73]
BDD_app<-BDD_app[,-2]

BDD_test<-BDD_test[,33:73]
BDD_test<-BDD_test[,-2]

xx.app<-BDD_app[,2:21]
y.app<-BDD_app[,1]

y.app<-as.matrix(y.app)
xx.app<-as.matrix(xx.app)

#### 1er type de mesure d'erreur
a.elnet2=cv.glmnet(xx.app,y.app,family="binomial",alpha=0.5, type.measure= "class")
lambda.opt=a.elnet2$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet2=predict(a.elnet2,newx=xx.app)
mean((app.elnet2-BDD_app[,1])^2)

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet2=predict(a.elnet2,newx=xx.test)
mean((predi.elnet2-BDD_test[,1])^2)


#### 2eme type de mesure d'erreur
a.elnet3=cv.glmnet(xx.app,y.app,family="binomial",alpha=0.5, type.measure= "auc")
lambda.opt=a.elnet3$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet3=predict(a.elnet3,newx=xx.app)
mean((app.elnet3-BDD_app[,1])^2)

pred21=prediction(app.elnet3, BDD_app$Ynum)
L=performance(pred21, "tpr", "fpr")
auc=performance(pred21, "auc")@y.values[[1]]
auc
plot(unlist(L@x.values),unlist(L@y.values),xlab="TFP (1-specificite)", ylab="TVP (sensibilite)", main="Courbe ROC - Elastic Net (base d'apprentissage)",type="s",col="blue")

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet3=predict(a.elnet3,newx=xx.test)
mean((predi.elnet3-BDD_test[,1])^2)

pred22=prediction(predi.elnet3, BDD_test$Ynum)
L=performance(pred22, "tpr", "fpr")
auc=performance(pred22, "auc")@y.values[[1]]
auc
plot(unlist(L@x.values),unlist(L@y.values),xlab="TFP (1-specificite)", ylab="TVP (sensibilite)", main="Courbe ROC - Elastic Net (base test)",type="s",col="blue")

#### 3eme type de mesure d'erreur
a.elnet4=cv.glmnet(xx.app,y.app,family="binomial", alpha=0.5, type.measure= "mse")
lambda.opt=a.elnet4$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet4=predict(a.elnet4,newx=xx.app)
mean((app.elnet4-BDD_app[,1])^2)

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet4=predict(a.elnet4,newx=xx.test)
mean((predi.elnet4-BDD_test[,1])^2)

##################################### alpha = 0.4 #####################################

BDD_app<-read.csv("Base_TRAIN.csv", header=TRUE, sep=";")
BDD_test<-read.csv("Base_TEST.csv", header=TRUE, sep=";")

BDD_app<-BDD_app[,33:73]
BDD_app<-BDD_app[,-2]

BDD_test<-BDD_test[,33:73]
BDD_test<-BDD_test[,-2]

xx.app<-BDD_app[,2:21]
y.app<-BDD_app[,1]

y.app<-as.matrix(y.app)
xx.app<-as.matrix(xx.app)

#### 1er type de mesure d'erreur
a.elnet2=cv.glmnet(xx.app,y.app,family="binomial",alpha=0.4, type.measure= "class")
lambda.opt=a.elnet2$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet2=predict(a.elnet2,newx=xx.app)
mean((app.elnet2-BDD_app[,1])^2)

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet2=predict(a.elnet2,newx=xx.test)
mean((predi.elnet2-BDD_test[,1])^2)


#### 2eme type de mesure d'erreur
a.elnet3=cv.glmnet(xx.app,y.app,family="binomial",alpha=0.4, type.measure= "auc")
lambda.opt=a.elnet3$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet3=predict(a.elnet3,newx=xx.app)
mean((app.elnet3-BDD_app[,1])^2)

pred21=prediction(app.elnet3, BDD_app$Ynum)
L=performance(pred21, "tpr", "fpr")
auc=performance(pred21, "auc")@y.values[[1]]
auc
plot(unlist(L@x.values),unlist(L@y.values),xlab="TFP (1-specificite)", ylab="TVP (sensibilite)", main="Courbe ROC - Elastic Net (base d'apprentissage)",type="s",col="blue")

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet3=predict(a.elnet3,newx=xx.test)
mean((predi.elnet3-BDD_test[,1])^2)

pred22=prediction(predi.elnet3, BDD_test$Ynum)
L=performance(pred22, "tpr", "fpr")
auc=performance(pred22, "auc")@y.values[[1]]
auc
plot(unlist(L@x.values),unlist(L@y.values),xlab="TFP (1-specificite)", ylab="TVP (sensibilite)", main="Courbe ROC - Elastic Net (base test)",type="s",col="blue")

#### 3eme type de mesure d'erreur
a.elnet4=cv.glmnet(xx.app,y.app,family="binomial", alpha=0.4, type.measure= "mse")
lambda.opt=a.elnet4$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet4=predict(a.elnet4,newx=xx.app)
mean((app.elnet4-BDD_app[,1])^2)

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet4=predict(a.elnet4,newx=xx.test)
mean((predi.elnet4-BDD_test[,1])^2)

##################################### alpha = 0.7 #####################################

BDD_app<-read.csv("Base_TRAIN.csv", header=TRUE, sep=";")
BDD_test<-read.csv("Base_TEST.csv", header=TRUE, sep=";")

BDD_app<-BDD_app[,33:73]
BDD_app<-BDD_app[,-2]

BDD_test<-BDD_test[,33:73]
BDD_test<-BDD_test[,-2]

xx.app<-BDD_app[,2:21]
y.app<-BDD_app[,1]

y.app<-as.matrix(y.app)
xx.app<-as.matrix(xx.app)

#### 1er type de mesure d'erreur
a.elnet2=cv.glmnet(xx.app,y.app,family="binomial",alpha=0.7, type.measure= "class")
lambda.opt=a.elnet2$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet2=predict(a.elnet2,newx=xx.app)
mean((app.elnet2-BDD_app[,1])^2)

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet2=predict(a.elnet2,newx=xx.test)
mean((predi.elnet2-BDD_test[,1])^2)


#### 2eme type de mesure d'erreur
a.elnet3=cv.glmnet(xx.app,y.app,family="binomial",alpha=0.7, type.measure= "auc")
lambda.opt=a.elnet3$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet3=predict(a.elnet3,newx=xx.app)
mean((app.elnet3-BDD_app[,1])^2)

pred21=prediction(app.elnet3, BDD_app$Ynum)
L=performance(pred21, "tpr", "fpr")
auc=performance(pred21, "auc")@y.values[[1]]
auc
plot(unlist(L@x.values),unlist(L@y.values),xlab="TFP (1-specificite)", ylab="TVP (sensibilite)", main="Courbe ROC - Elastic Net (base d'apprentissage)",type="s",col="blue")

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet3=predict(a.elnet3,newx=xx.test)
mean((predi.elnet3-BDD_test[,1])^2)

pred22=prediction(predi.elnet3, BDD_test$Ynum)
L=performance(pred22, "tpr", "fpr")
auc=performance(pred22, "auc")@y.values[[1]]
auc
plot(unlist(L@x.values),unlist(L@y.values),xlab="TFP (1-specificite)", ylab="TVP (sensibilite)", main="Courbe ROC - Elastic Net (base test)",type="s",col="blue")

#### 3eme type de mesure d'erreur
a.elnet4=cv.glmnet(xx.app,y.app,family="binomial", alpha=0.7, type.measure= "mse")
lambda.opt=a.elnet4$lambda.min
app=glmnet(xx.app,y.app,lambda=lambda.opt)
#erreur apprentissage
app.elnet4=predict(a.elnet4,newx=xx.app)
mean((app.elnet4-BDD_app[,1])^2)

#erreur de prédiction
xx.test<-BDD_test[,2:21]
xx.test<-as.matrix(xx.test)
predi.elnet4=predict(a.elnet4,newx=xx.test)
mean((predi.elnet4-BDD_test[,1])^2)

#########################################################################################################################
##################################### Visualisation graphique pour différents alpha #####################################
#########################################################################################################################

y.test<-BDD_test[,1]

############################################ Base d'apprentissage l########################################################

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(xx.app,y.app, type.measure="auc", 
                                            alpha=i/10,family ="binomial"))
}

#risque de prendre beaucoup de temps, mais fini par donner des resultats

############################################## alpha=0.4 #################################################################
elnet_04 <- glmnet(xx.app,y.app,family="binomial", alpha=0.4)
par(mfrow=c(1,2))
plot(elnet_04, xvar="lambda")
plot(fit4, main="Elastic Net alpha= 0.4")

############################################ alpha=0.5 ################################################################
elnet_05 <- glmnet(xx.app,y.app,family="binomial", alpha=0.5)
par(mfrow=c(1,2))
plot(elnet_05, xvar="lambda")
plot(fit5, main="Elastic Net alpha= 0.5")

############################################ alpha=0.7 ################################################################
elnet_07 <- glmnet(xx.app,y.app,family="binomial", alpha=0.7)
par(mfrow=c(1,2))
plot(elnet_07, xvar="lambda")
plot(fit8, main="Elastic Net alpha= 0.7")

############################################ Graph rÃ©sumÃ© ###########################################################
dev.off()
par(mfrow=c(3,2))
plot(elnet_04, xvar="lambda")
plot(fit4, main="Elastic Net alpha= 0.4")
plot(elnet_05, xvar="lambda")
plot(fit5, main="Elastic Net alpha= 0.5")
plot(elnet_07, xvar="lambda")
plot(fit7, main="Elastic Net alpha= 0.7")

###################################################### Base test #######################################################

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(xx.test,y.test, type.measure="auc", 
                                            alpha=i/10,family ="binomial"))
}

############################################## alpha=0.4 #################################################################
elnet_04 <- glmnet(xx.test,y.test,family="binomial", alpha=0.4)
par(mfrow=c(1,2))
plot(elnet_04, xvar="lambda")
plot(fit4, main="Elastic Net alpha= 0.4")

############################################## alpha=0.5 #################################################################
elnet_05 <- glmnet(xx.test,y.test,family="binomial", alpha=0.5)
par(mfrow=c(1,2))
plot(elnet_05, xvar="lambda")
plot(fit5, main="Elastic Net alpha= 0.5")

############################################ alpha=0.7 ##################################################################
elnet_07 <- glmnet(xx.test,y.test,family="binomial", alpha=0.7)
par(mfrow=c(1,2))
plot(elnet_07, xvar="lambda")
plot(fit8, main="Elastic Net alpha= 0.7")

############################################ Graph resume ######################################################################
dev.off()
par(mfrow=c(3,2))
plot(elnet_04, xvar="lambda")
plot(fit4, main="Elastic Net alpha= 0.4")
plot(elnet_05, xvar="lambda")
plot(fit5, main="Elastic Net alpha= 0.5")
plot(elnet_07, xvar="lambda")
plot(fit7, main="Elastic Net alpha= 0.7")





