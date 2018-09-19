##################################################################
##################         GRADIENT BOOSTING       ###############


library(questionr)
library(broom)
library(ggplot2)
library(GGally)
library(FactoMineR)
library(gbm)
library(caret)
library(ROCR)
library(leaps)
library(car)
library(rpart)
library(h2o)





###################################
####    Exploration donnees    ####


Train <- read.csv2("Base_TRAIN.csv", sep = ",")
Train2 <- Train[,c(33,32,31,5:7,9:11,28,18,22:25,29,30)] #choix de garder que les vb en classes
Test <- read.csv2("Base_TEST.csv", sep = ",")
X <- Train2[,-1]
Y <- (Train2[,1])
Ytest <- (Test$Ynum)



### Statistiques Univariées
# Dimension data
dim(Train2)


# Variables qualitatives
for(i in 1:ncol(Train2)){
  print(table(Train2[,i], dnn = c(colnames(Train2))[i]))
}


### Statistiques Bivariées
# Variables qualitatives et Cible
for(i in 1:ncol(X)){
  print(cprop(table(Y, X[,i], dnn = c(colnames(X))[i])))
}

# Variable cible
freq(Y)



### Selection des variables
# Méthode forward
formule <- as.formula(paste("Y ~" , paste(names(X), collapse = "+")))
modelinit <- glm(Y~1,data=X,family=binomial(link = "logit"))
modelinit
model_for <- step(modelinit,direction="forward", trace = TRUE, k=log(nrow(Train2)), scope = list(upper=formule)) #choix du critère BIC pour modele plus parcomonieux
model_for
summary(model_for)

train.bic <- predict(model_for, newdata = Train2, type = "response")
valid.bic <- predict(model_for, newdata = Test, type = "response")

predA <- prediction(train.bic, Y, label.ordering = c(0,1))
predV <- prediction(valid.bic, Test$Ynum, label.ordering = c(0,1))
performance(predA,"auc")@y.values[[1]]
performance(predV,"auc")@y.values[[1]] #resulat similaire entre ech Train et Test


# V de cramer
cramer <- matrix(NA, ncol(X), 3)

for(i in (1:ncol(X))){
  cramer[i,1] <- names(X[i])
  cramer[i,2] <- sqrt(chisq.test(table(X[,i], Y))$statistic / (length(X[,i])))
  cramer[i,3] <- chisq.test(table(X[,i], Y))$p.value
}
colnames(cramer) <- c("variables", "V de Cramer", "p-value chi2")

vcramer <- cramer[order(cramer[,2], decreasing = TRUE),]

barplot(
  as.numeric(vcramer[,2]), 
  col=gray(0:nrow(vcramer)/nrow(vcramer)), 
  names.arg=vcramer[,1],
  ylab='V de Cramer',
  ylim=c(0,0.35),
  cex.names =0.8,
  las=3)


#Selection des vb par forward car modele plus parciemonieux et il a un auc plus eleve sur l'ech test
X <- X[,-c(2,4,5,13)]




##################################### 
####    Regression Logistique    ####

### Regression logistique
# Changer la modalité de reference
X$marital <- relevel(X$marital, ref = 2)
X$contact <- relevel(X$contact, ref = 3)
X$cl_month <- relevel(X$cl_month, ref = 3)

reglog <- glm(formula = Y ~ cl_duration + poutcome + housing + contact + 
                cl_job + cl_month + cl_balance2 + marital + cl_campaign + 
                loan , family = binomial(link = "logit"), data = X)
summary(reglog)

# Aplication a l'ech test
validreg <- predict(reglog, newdata = Test, type = "response")

predreg <- prediction(validreg, Ytest, label.ordering = c(0,1))
performance(predreg,"auc")@y.values[[1]] # 0.8770434




#################################
####    Gradient Boosting    ####

## Gradient Boosting initial
boost1 = gbm(formula = Y ~ ., data = X,
              distribution = "bernoulli") 


# Application a l'echentillon test
validboost1 <- predict(boost1,Test,n.trees=boost1$n.trees)
predboost1 <- prediction(validboost1, Ytest)
performance(predboost1,"auc")@y.values[[1]] # 0.7566703



# Détermination des paramètres par tatonnement
n.trees <- 1000
boost = gbm(formula = Y ~ ., data = X,
            distribution = "bernoulli",
            shrinkage=0.05, # parametre de correction
            n.trees=max(n.trees), # nb d'arbres
            interaction.depth = 4, # profondeur des arbres
            n.minobsinnode = 50) # nombre d'individus dans les dernières feuilles


# Application a l'echentillon test et calcul auc
validboost <- predict(boost,Test,n.trees=boost$n.trees)
predboost <- prediction(validboost, Ytest)
performance(predboost,"auc")@y.values[[1]] # 0.9140741




## Package Caret
gbmGrid <- expand.grid(interaction.depth=4, n.trees = c(500, 1000), # definition de la grille de paramètres
                       shrinkage=c(0.05, 0.03, 0.01),
                       n.minobsinnode=50)
bootControl <- trainControl(method = "cv", number = 10) # méthode par validation croisée itérée 10 fois
gbmFit <- train(X, Y, distribution="bernoulli", method="gbm",
                trControl=bootControl, # liste des valeurs définissant le comportement de la fonction
                verbose=TRUE, # affichage de l'avancée de l'algorithme
                tuneGrid=gbmGrid) # grille de paramètres à tester)
