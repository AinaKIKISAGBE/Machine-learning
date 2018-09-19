library(dplyr)
# install.packages("ROCR")
library(ROCR)
library(car)
library(ggplot2)
library(tidyr)
install.packages("gridExtra")
library(ggthemr)
library(ggthemes)
library(gridExtra)
library(data.table)
library(scales)

train <- read.csv("Base_TRAIN.csv", header = T, sep=",")
train= train %>% mutate (primary=ifelse(education=="primary",1,0),
                         secondary=ifelse(education=="secondary",1,0),
                         tertiary=ifelse(education=="tertiary",1,0))
test= test %>% mutate (primary=ifelse(education=="primary",1,0),
                       secondary=ifelse(education=="secondary",1,0),
                       tertiary=ifelse(education=="tertiary",1,0))
train2 <- train[,c(33,34,74:ncol(train))]
test <- read.csv("Base_TEST.csv", header = T, sep=",", dec=".")
test2 <- test[,c(33,34,74:ncol(test))]
traincram <- train[c(2,4:25,28:32)]
traincram <- traincram[-c(6,12,15)]

# Choix des variables
cram = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  # print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}


cramer <- matrix(names(traincram)[-1],ncol=2, nrow=length(names(traincram)[-1]))
i <- 2
for (i in 2:ncol(traincram)){
  cramer[i-1,2] <- cram(traincram$y,traincram[,i])
}
# cl_duration à vérifier



traincram <- traincram[,-12]
traincram <- traincram[,-c(9,10,13:15,18)]
traincram <- traincram[,-2]

###############
### Reg Log ###
###############

# 1st Régression logistique (avec cl_duration)
reglog <- glm(data = traincram, formula = y ~ .  , family = binomial(link = "logit"))
summary(reglog)

bestmodel <- step(reglog)
summary(bestmodel)
# y ~ marital + education + housing + loan + contact + 
#   campaign + poutcome + cl_duration + cl_campaign + cl_month + 
#   cl_balance2 + cl_day2 + cl_age2 + cl_job

# Supression de la variable default (et cle)
train3 <- train2[,-c(2,6,7)]
test3 <- test2[,-c(2,6,7)]
i <- 1
for (i in 1:ncol(train3)){
  train3[,i] <- as.factor(train3[,i])
}
# Régression logistique
reglog2 <- glm(data = train3, formula = Ynum ~ . , family = binomial(link = "logit"))
resglm <- summary(reglog2)

cat("Pseudo R-Squared = ",round(1 - ( resglm$deviance / resglm$null.deviance ), 2 ))

lapply(test3, class)

i <- 1
for (i in 1:ncol(test3)){
  test3[,i] <- as.factor(test3[,i])
}



# On établit les prédictions à partir du modèle retenu : renvoie les probabilités 
# estimées de réalisation de l'évenement pour les individus dans l'échantillon "test"

train3$reponse <- predict(reglog2, newdata = train3, type = "response")
test3$reponse <- predict( reglog2, newdata = test3, type = "response" )

accuracy_info <- AccuracyCutoffInfo( train = train3, test = test3, 
                                     predict = proba_pred, actual = "Ynum" )
accuracy_info$plot + geom_vline(xintercept = 0.55, lty = 2)
co <- which(accuracy_info$data$train==max(accuracy_info$data$train))
accuracy_info$data[co]
# cutoff = 0.55


# calcul des différents indicateurs 
valeur <- as.numeric(as.character(train3$Ynum))
pred <- prediction(proba_pred, valeur)

perf <- performance(pred, measure = "spec", x.measure = "cutoff")
plot(perf)
perf2 <- performance(pred, measure = "sens", x.measure = "cutoff")
plot(perf2)
# courbe ROC
perf3 <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf3, colorize = T, print.cutoffs.at=seq(0.1, by=.1))

# retourne l'aire sur la courbe
ROC <- performance(pred, measure = "auc", x.measure = "cutoff")
ROC@y.values
# AUC : 0.8821995


# On choisit le seuil qui minimise la différence entre la sensibilité et la spécificité
# spécificité en valeur absolue
sensitivity <- perf2@y.values[[1]]
specificity <- perf@y.values[[1]]
diffspecificity <- 1-specificity
diff <- abs(sensitivity-specificity)
tab_indicateurs <- data.frame(diff, diffspecificity,cutoff_optimal=perf@x.values[[1]], sensitivity, specificity)

# Calcul du cutoff optimal
cutoff <- tab_indicateurs[tab_indicateurs$diff==min(tab_indicateurs$diff),"cutoff_optimal"]


# plot du cutoff optimal
plot(x = tab_indicateurs$cutoff_optimal, y = tab_indicateurs$diff,type='l', 
     main = "cutoff optimal", xlab = "Cutoff", ylab="sensitivity-specificity")
points(x=cutoff, y=0, pch=16)
# ou
plot(y = tab_indicateurs$sensitivity, x = tab_indicateurs$cutoff_optimal , type="l", col = "red", 
     main = "cutoff optimal", xlab = "cutoff", ylab = "")
lines(y=tab_indicateurs$specificity,x = tab_indicateurs$cutoff_optimal)
legend("bottom", legend=c("Sensitivity", "Specificity"),
       col=c("red", "black"), lty=1, cex=0.8)
abline(h=0.8, v=cutoff, lty=2)
points(y=0.8,x=cutoff, pch=16)

# cutoff train : 0.1278
# cutoff test : 0.1263

# Si la valeur prédite est > au cutoff alors le modèle prévoit ynum=1 et inversement
# pour 0
# cutoff = 0.55
test_p <- test3 %>% mutate(reponse=ifelse(proba_pred>(0.55), 1,0))
# cutoff = 0.126
test_p <- test3 %>% mutate(reponse=ifelse(proba_pred>cutoff, 1,0))

# proportion de 1 prédits avec le modele vs proportion de 1 effectif
prop.table( table( test3$Ynum ) )  #11,7%
prop.table( table( test_p$reponse ) )  #4%


# Création matrice de confusion

# install.packages("caret")
library(caret)
confusionMatrix(test_p$Ynum,test_p$reponse)

# Accuracy train/test = 89,4%

################
### ADABOOST ###
################


# install.packages("ada")
library(car)
library("ada")
i <- 1
for (i in 1:ncol(train2)){
  train2[,i]=as.factor(as.numeric(train2[,i])-1)
}
lapply(train2, class)

i <- 1
for (i in 1:ncol(test2)){
  test2[,i]=as.factor(as.numeric(test2[,i])-1)
}

# on fixe la graine
set.seed(235)


adab <- ada(formula=Ynum ~ ., data = train2, test.x = test2[,-1], test.y=test2[,1],type = "discrete", iter = 100, nu=0.7)
summary(adab)
# Train Error: 0.1
# plot du taux d'erreur en fonction de n=100
plot(adab, test=T)
# évolution des erreurs
adab$model$errs[,3]


pred_ada <- predict(adab, newdata = test2, type='vector')

# on ajoute les prédictions dans la variable reponse pour comparer les resultats
test_a <- test2 %>% mutate(reponse=pred_ada)

# matrice de confusion
confusionMatrix(test_a$Ynum,test_a$reponse)
# 0.1047 d'erreur

valeur <- as.numeric(as.character(test2_a$Ynum))
pred <- prediction(as.numeric(pred_ada), valeur)
# courbe ROC
perf_a <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf_a, main ="Courbe ROC (Adaboost)")

ROC <- performance(pred, measure = "auc", x.measure = "cutoff")
# retourne l'aire sur la courbe
ROC@y.values
# AUC : 0.6400183





