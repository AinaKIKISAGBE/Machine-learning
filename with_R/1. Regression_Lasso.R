#=======================================================#
#       Projet Machine Learning - Méthode Lasso         #
#=======================================================#


setwd("E:/Projet/")


#================#
#    Données     #
#================#

library(readxl)
base <- read_excel("DE_Base.xlsx")
base <- base[,1:2]
base_train <- read.csv("Base_TRAIN.csv",",",header = TRUE)
base_test <- read.csv("Base_TEST.csv",",",header = TRUE)

View(base_train)


# On dichotomise la variable education pour chacune de ses modalités

base_train$cl_education_primary <- (base_train$education == "primary")
base_train$cl_education_primary <- as.numeric(base_train$cl_education_primary)
# Variable education dichtomisée : Primary (0/1)

base_train$cl_education_secondary <- (base_train$education == "secondary")
base_train$cl_education_secondary <- as.numeric(base_train$cl_education_secondary)
# Variable education dichtomisée : Secondary (0/1)

base_train$cl_education_tertiary <- (base_train$education == "tertiary")
base_train$cl_education_tertiary <- as.numeric(base_train$cl_education_tertiary)
# Variable education dichtomisée : Tertiary (0/1)

base_train$cl_education_unknown <- (base_train$education == "unknown")
base_train$cl_education_unknown <- as.numeric(base_train$cl_education_unknown)
# Variable education dichtomisée : Unknown (0/1)


base_test$cl_education_primary <- (base_test$education == "primary")
base_test$cl_education_primary <- as.numeric(base_test$cl_education_primary)
# Variable education dichtomisée : Primary (0/1)

base_test$cl_education_secondary <- (base_test$education == "secondary")
base_test$cl_education_secondary <- as.numeric(base_test$cl_education_secondary)
# Variable education dichtomisée : Secondary (0/1)

base_test$cl_education_tertiary <- (base_test$education == "tertiary")
base_test$cl_education_tertiary <- as.numeric(base_test$cl_education_tertiary)
# Variable education dichtomisée : Tertiary (0/1)

base_test$cl_education_unknown <- (base_test$education == "unknown")
base_test$cl_education_unknown <- as.numeric(base_test$cl_education_unknown)
# Variable education dichtomisée : Unknown (0/1)


#==============================================================#
#    Régression logistique sur les variables dichotomisées     #
#==============================================================#

# On sélectionne la variable cible + les variables dichotomisées pour notre table d'apprentissage

base_dicho_train <- base_train[,33:ncol(base_train)]
base_dicho_train <- base_dicho_train[,-c(1:41)]
base_dicho_train <- base_dicho_train[,-c(3,5,7,9,12,16,19,23,26,31,35,39,44,48)]
base_dicho_train <- cbind(Ynum = base_train$Ynum,base_dicho_train)


# On sélectionne la variable cible + les variables dichotomisées pour notre table de validation

base_dicho_test <- base_test[,33:ncol(base_test)]
base_dicho_test <- base_dicho_test[,-c(1:41)]
base_dicho_test<- base_dicho_test[,-c(3,5,7,9,12,16,19,23,26,31,35,39,44,48)]
base_dicho_test <- cbind(Ynum = base_test$Ynum,base_dicho_test)


# Régression logistique avec toutes les variables dichotomisées 

fit.glm <- glm(Ynum ~ ., data = base_dicho_train, family = binomial(link=logit))
summary(fit.glm)


# Stepwise Algorithm

step(fit.glm, direction = "both")
# Variables sélectionnées : 
#  maritalmarried + housingno + loanno + contactcellular + 
#  contacttelephone + poutcomefailure + poutcomeother + poutcomesuccess + 
#  cl_age22_35_55_ans + cl_balance21__100_ + cl_balance22_100_500_ + 
#  cl_balance23_500_3000_ + cl_day22__15_21_jours + cl_duration1____60s + 
#  cl_duration2__60_120s + cl_duration3__120_180s + cl_duration3__180_360s + 
#  cl_campaign1__1_campagne + cl_month1_Trim3 + Cl_job_1 + Cl_job_2 + 
#  Cl_job_3 + Cl_job_4 + cl_education_tertiary


# Backward elimination
step(fit.glm, direction = "backward")
# On obtient les mêmes résultats.


fit.glm <- glm(formula = Ynum ~ maritalmarried + housingno + loanno + contactcellular + 
                 contacttelephone + poutcomefailure + poutcomeother + poutcomesuccess + 
                 cl_age22_35_55_ans + cl_balance21__100_ + cl_balance22_100_500_ + 
                 cl_balance23_500_3000_ + cl_day22__15_21_jours + cl_duration1____60s + 
                 cl_duration2__60_120s + cl_duration3__120_180s + cl_duration3__180_360s + 
                 cl_campaign1__1_campagne + cl_month1_Trim3 + Cl_job_1 + Cl_job_2 + 
                 Cl_job_3 + Cl_job_4 + cl_education_tertiary, family = binomial(link = logit), 
                 data = base_dicho_train)

summary(fit.glm)


# Multicolinéarité

library(car)
vif(fit.glm)
# Pas de problème de multicolinéarité


# Probabilités estimées de réalisation sur la table d'apprentissage 

predict(fit.glm, type = "response")


# Indicateurs de la matrice de confusion (sur la table d'apprentissage)

library(ROCR)
library(InformationValue)
library(pROC)
proba_predite <- predict(fit.glm,type = "response")
valeur <- as.numeric(base_dicho_train$Ynum)
pred <- prediction(proba_predite,valeur)

# Choix du cutoff : on choisit le cutoff en fonction de la répartition de notre variable cible
summary(base_train$y)
# => Choix d'un cutoff égal à 0.12

# Sensibilité
sens <- performance(pred,"sens")
plot(sens) # Représentation de la sensibilité en fonction du cutoff

# Sensibilité pour un cutoff de 0.12
sensitivity(base_dicho_train$Ynum,proba_predite,threshold = 0.12)
# Sensibilité  = 0.8252768

# Spécificité
spec <- performance(pred,"spec")
plot(spec) # Représentation de la spécificité en fonction du cutoff

# Spécificité pour un cutoff de 0.12
specificity(base_dicho_train$Ynum,proba_predite,threshold = 0.12)
# Spécificité = 0.7942818

# Accuracy
acc <- performance(pred,"acc")
plot(acc)

# Taux d'erreur
err <- performance(pred,"err")
plot(err)

# Taux d'erreur pour un cutoff de 0.12
misClassError(base_dicho_train$Ynum,proba_predite,threshold = 0.12)
# Taux d'erreur = 20,2%

# Courbe ROC
roc <- roc(base_dicho_train$Ynum,proba_predite)
plot(roc)

# Aire sous la courbe ROC
auc <- performance(pred,"auc")
auc@y.values
# AUC = 0.8821707

# Matrice de confusion pour un cutoff de 0.12
confusionMatrix(base_dicho_train$Ynum,proba_predite,threshold = 0.12)

######################################################################################################

# Probabilités estimées de réalisation sur la table de validation

predict(fit.glm, newdata = base_dicho_test, type = "response")


# Indicateurs de la matrice de confusion (sur la table de validation)

proba_predite <- predict(fit.glm, newdata = base_dicho_test, type = "response")
valeur <- as.numeric(base_dicho_test$Ynum)
pred <- prediction(proba_predite,valeur)


# Sensibilité
sens <- performance(pred,"sens")
plot(sens) # Représentation de la sensibilité en fonction du cutoff

# Sensibilité pour un cutoff de 0.12
sensitivity(base_dicho_test$Ynum,proba_predite,threshold = 0.12)
# Sensibilité  = 0.8209332

# Spécificité
spec <- performance(pred,"spec")
plot(spec) # Représentation de la spécificité en fonction du cutoff

# Spécificité pour un cutoff de 0.12
specificity(base_dicho_test$Ynum,proba_predite,threshold = 0.12)
# Spécificité = 0.7889947

# Accuracy
acc <- performance(pred,"acc")
plot(acc)

# Taux d'erreur
err <- performance(pred,"err")
plot(err)

# Taux d'erreur pour un cutoff de 0.12
misClassError(base_dicho_test$Ynum,proba_predite,threshold = 0.12)
# Taux d'erreur = 20,7%

# Courbe ROC
roc <- roc(base_dicho_test$Ynum,proba_predite)
plot(roc)

# Aire sous la courbe ROC
auc <- performance(pred,"auc")
auc@y.values
# AUC = 0.8787972


#===========================================================================================#
#    Régression logistique sur les variables continues et catégorielles (dichotomisées)     #
#===========================================================================================#

# On sélectionne la variable cible + les variables continues et catégorielles (dichotomisées) pour notre table d'apprentissage

base_train_1 <- base_train[,c(3,8,12,14,15,16,17)]
base <- base_train[,-c(1:41)]
base <- cbind(base[,c(33:48)],base[,c(68:80)])
base <- base[,-c(3,5,7,9,12,16,20,25,29)]
base_train_1 <- cbind(base_train_1,base)
base_train_1 <- cbind(Ynum = base_train$Ynum,base_train_1)


# On sélectionne la variable cible + les variables dichotomisées pour notre table de validation

base_test_1 <- base_test[,c(3,8,12,14,15,16,17)]
base <- base_test[,-c(1:41)]
base <- cbind(base[,c(33:48)],base[,c(68:80)])
base <- base[,-c(3,5,7,9,12,16,20,25,29)]
base_test_1 <- cbind(base_test_1,base)
base_test_1 <- cbind(Ynum = base_test$Ynum,base_test_1)


# Régression logistique avec toutes les variables continues et catégorielles (dichotomisées)

fit.glm <- glm(Ynum ~ ., data = base_train_1, family = binomial(link=logit))
summary(fit.glm)


# Stepwise Algorithm

step(fit.glm, direction = "both")
# Variables sélectionnées : 
#  balance + day + duration + campaign + maritaldivorced + 
#  maritalmarried + housingno + loanno + contactcellular + contacttelephone + 
#  poutcomefailure + poutcomeother + poutcomesuccess + 
#  cl_month1_Trim2 + cl_month1_Trim3 + Cl_job_1 + Cl_job_2 + 
#  Cl_job_3 + Cl_job_4 + cl_education_primary + cl_education_secondary


# Backward elimination
step(fit.glm, direction = "backward")
# On obtient les mêmes résultats.


fit.glm <- glm(formula = Ynum ~ balance + day + duration + campaign + maritaldivorced + 
                 maritalmarried + housingno + loanno + contactcellular + contacttelephone + 
                 poutcomefailure + poutcomeother + poutcomesuccess + cl_month1_Trim2 + cl_month1_Trim3 + Cl_job_1 + Cl_job_2 + 
                 Cl_job_3 + Cl_job_4 + cl_education_primary + cl_education_secondary, 
               family = binomial(link = logit), data = base_train_1)

summary(fit.glm)


# Multicolinéarité

vif(fit.glm)
# Pas de problème de multicolinéarité


# Probabilités estimées de réalisation sur la table d'apprentissage 

predict(fit.glm, type = "response")


# Indicateurs de la matrice de confusion (sur la table d'apprentissage)

proba_predite <- predict(fit.glm,type = "response")
valeur <- as.numeric(base_train_1$Ynum)
pred <- prediction(proba_predite,valeur)


# Sensibilité
sens <- performance(pred,"sens")
plot(sens) # Représentation de la sensibilité en fonction du cutoff

# Sensibilité pour un cutoff de 0.12
sensitivity(base_train_1$Ynum,proba_predite,threshold = 0.12)
# Sensibilité  = 0.8042128

# Spécificité
spec <- performance(pred,"spec")
plot(spec) # Représentation de la spécificité en fonction du cutoff

# Spécificité pour un cutoff de 0.12
specificity(base_train_1$Ynum,proba_predite,threshold = 0.12)
# Spécificité = 0.8317112

# Accuracy
acc <- performance(pred,"acc")
plot(acc)

# Taux d'erreur
err <- performance(pred,"err")
plot(err)

# Taux d'erreur pour un cutoff de 0.12
misClassError(base_train_1$Ynum,proba_predite,threshold = 0.12)
# Taux d'erreur = 17,2%

# Courbe ROC
roc <- roc(base_train_1$Ynum,proba_predite)
plot(roc)

# Aire sous la courbe ROC
auc <- performance(pred,"auc")
auc@y.values
# AUC = 0.8943173

# Matrice de confusion pour un cutoff de 0.12
confusionMatrix(base_train_1$Ynum,proba_predite,threshold = 0.12)

######################################################################################################

# Probabilités estimées de réalisation sur la table de validation

predict(fit.glm, newdata = base_test_1, type = "response")


# Indicateurs de la matrice de confusion (sur la table de validation)

proba_predite <- predict(fit.glm, newdata = base_test_1, type = "response")
valeur <- as.numeric(base_test_1$Ynum)
pred <- prediction(proba_predite,valeur)


# Sensibilité
sens <- performance(pred,"sens")
plot(sens) # Représentation de la sensibilité en fonction du cutoff

# Sensibilité pour un cutoff de 0.12
sensitivity(base_test_1$Ynum,proba_predite,threshold = 0.12)
# Sensibilité  = 0.8083228

# Spécificité
spec <- performance(pred,"spec")
plot(spec) # Représentation de la spécificité en fonction du cutoff

# Spécificité pour un cutoff de 0.12
specificity(base_test_1$Ynum,proba_predite,threshold = 0.12)
# Spécificité = 0.8256513

# Accuracy
acc <- performance(pred,"acc")
plot(acc)

# Taux d'erreur
err <- performance(pred,"err")
plot(err)

# Taux d'erreur pour un cutoff de 0.12
misClassError(base_test_1$Ynum,proba_predite,threshold = 0.12)
# Taux d'erreur = 17,6%

# Courbe ROC
roc <- roc(base_test_1$Ynum,proba_predite)
plot(roc)

# Aire sous la courbe ROC
auc <- performance(pred,"auc")
auc@y.values
# AUC = 0.8904408


#====================================================================#
#     Régression Logistique Lasso sur les variables dichotomisées    #
#====================================================================#

library(glmnet)
help("glmnet")

Ynum_train <- base_dicho_train[,1]
Ynum_test <- base_dicho_test[1]
lasso <- glmnet(x = as.matrix(base_dicho_train[,-1]), y = Ynum_train, family = "binomial", alpha = 1)
print(lasso)


# Visualisation des coefficients

plot(lasso)


# Choix du paramètre lambda

cv_lasso <- cv.glmnet(x = as.matrix(base_dicho_train[,-1]), y = Ynum_train)
cv_lasso

plot(cv_lasso)

cv_lasso$lambda.min
cv_lasso$lambda.1se
# Choix du coefficient : lambda.min = 0.0001355952 ou lambda.1se = 0.005105071


# Coefficients avec lambda = 0.0001355952

coef(cv_lasso, s = "lambda.min")


# Coefficients avec lambda = 0.005105071

coef(cv_lasso, s = "lambda.1se")


lambda <- cv_lasso$lambda.min


# Probabilités estimées de réalisation sur la table d'apprentissage 

predict(lasso, newx = as.matrix(base_dicho_train[,-1]), type = "response", s = lambda)


# Indicateurs de la matrice de confusion (sur la table d'apprentissage)

proba_predite <- predict(lasso, newx = as.matrix(base_dicho_train[,-1]), type = "response", s = lambda)
proba_predite <- as.numeric(proba_predite)
valeur <- as.numeric(base_dicho_train$Ynum)
pred <- prediction(proba_predite,valeur)

# Sensibilité
sens <- performance(pred,"sens")
plot(sens) # Représentation de la sensibilité en fonction du cutoff

# Sensibilité pour un cutoff de 0.12
sensitivity(base_dicho_train$Ynum,proba_predite,threshold = 0.12)
# Sensibilité  = 0.8250068

# Spécificité
spec <- performance(pred,"spec")
plot(spec) # Représentation de la spécificité en fonction du cutoff

# Spécificité pour un cutoff de 0.12
specificity(base_dicho_train$Ynum,proba_predite,threshold = 0.12)
# Spécificité = 0.7939598

# Accuracy
acc <- performance(pred,"acc")
plot(acc)

# Taux d'erreur
err <- performance(pred,"err")
plot(err)

# Taux d'erreur pour un cutoff de 0.12
misClassError(base_dicho_train$Ynum,proba_predite,threshold = 0.12)
# Taux d'erreur = 20,2%

# Courbe ROC
roc <- roc(base_dicho_train$Ynum,proba_predite)
plot(roc)

# Aire sous la courbe ROC
auc <- performance(pred,"auc")
auc@y.values
# AUC = 0.882218

# Matrice de confusion pour un cutoff de 0.12
confusionMatrix(base_dicho_train$Ynum,proba_predite,threshold = 0.12)

######################################################################################################

# Probabilités estimées de réalisation sur la table de validation

predict(lasso, newx = as.matrix(base_dicho_test[,-1]), type = "response", s = lambda)


# Indicateurs de la matrice de confusion (sur la table de validation)

proba_predite <- predict(lasso, newx = as.matrix(base_dicho_test[,-1]), type = "response", s = lambda)
proba_predite <- as.numeric(proba_predite)
valeur <- as.numeric(base_dicho_test$Ynum)
pred <- prediction(proba_predite,valeur)

# Sensibilité
sens <- performance(pred,"sens")
plot(sens) # Représentation de la sensibilité en fonction du cutoff

# Sensibilité pour un cutoff de 0.12
sensitivity(base_dicho_test$Ynum,proba_predite,threshold = 0.12)
# Sensibilité  = 0.8190416

# Spécificité
spec <- performance(pred,"spec")
plot(spec) # Représentation de la spécificité en fonction du cutoff

# Spécificité pour un cutoff de 0.12
specificity(base_dicho_test$Ynum,proba_predite,threshold = 0.12)
# Spécificité = 0.7889112

# Accuracy
acc <- performance(pred,"acc")
plot(acc)

# Taux d'erreur
err <- performance(pred,"err")
plot(err)

# Taux d'erreur pour un cutoff de 0.12
misClassError(base_dicho_test$Ynum,proba_predite,threshold = 0.12)
# Taux d'erreur = 20,8%

# Courbe ROC
roc <- roc(base_dicho_test$Ynum,proba_predite)
plot(roc)

# Aire sous la courbe ROC
auc <- performance(pred,"auc")
auc@y.values
# AUC = 0.8790092

# Matrice de confusion pour un cutoff de 0.12
confusionMatrix(base_dicho_test$Ynum,proba_predite,threshold = 0.12)


#=================================================================================================#
#    Régression logistique Lasso sur les variables continues et catégorielles (dichotomisées)     #
#=================================================================================================#

Ynum_train <- base_train_1[,1]
Ynum_test <- base_test_1[1]
lasso <- glmnet(x = as.matrix(base_train_1[,-1]), y = Ynum_train, family = "binomial", alpha = 1)
print(lasso)


# Visualisation des coefficients

plot(lasso)


# Choix du paramètre lambda

cv_lasso <- cv.glmnet(x = as.matrix(base_train_1[,-1]), y = Ynum_train)
cv_lasso

plot(cv_lasso)

cv_lasso$lambda.min
cv_lasso$lambda.1se
# Choix du coefficient : lambda.min = 0.0002501064 ou lambda.1se = 0.006490321


# Coefficients avec lambda = 0.0002501064

coef(cv_lasso, s = "lambda.min")


# Coefficients avec lambda = 0.006490321

coef(cv_lasso, s = "lambda.1se")


lambda <- cv_lasso$lambda.min


# Probabilités estimées de réalisation sur la table d'apprentissage 

predict(lasso, newx = as.matrix(base_train_1[,-1]), type = "response", s = lambda)


# Indicateurs de la matrice de confusion (sur la table d'apprentissage)

proba_predite <- predict(lasso, newx = as.matrix(base_train_1[,-1]), type = "response", s = lambda)
proba_predite <- as.numeric(proba_predite)
valeur <- as.numeric(base_train_1$Ynum)
pred <- prediction(proba_predite,valeur)

# Sensibilité
sens <- performance(pred,"sens")
plot(sens) # Représentation de la sensibilité en fonction du cutoff

# Sensibilité pour un cutoff de 0.12
sensitivity(base_train_1$Ynum,proba_predite,threshold = 0.12)
# Sensibilité  = 0.8058331

# Spécificité
spec <- performance(pred,"spec")
plot(spec) # Représentation de la spécificité en fonction du cutoff

# Spécificité pour un cutoff de 0.12
specificity(base_train_1$Ynum,proba_predite,threshold = 0.12)
# Spécificité = 0.8322479

# Accuracy
acc <- performance(pred,"acc")
plot(acc)

# Taux d'erreur
err <- performance(pred,"err")
plot(err)

# Taux d'erreur pour un cutoff de 0.12
misClassError(base_train_1$Ynum,proba_predite,threshold = 0.12)
# Taux d'erreur = 17,1%

# Courbe ROC
roc <- roc(base_train_1$Ynum,proba_predite)
plot(roc)

# Aire sous la courbe ROC
auc <- performance(pred,"auc")
auc@y.values
# AUC = 0.8948105

# Matrice de confusion pour un cutoff de 0.12
confusionMatrix(base_dicho_train$Ynum,proba_predite,threshold = 0.12)

######################################################################################################

# Probabilités estimées de réalisation sur la table de validation

predict(lasso, newx = as.matrix(base_test_1[,-1]), type = "response", s = lambda)


# Indicateurs de la matrice de confusion (sur la table de validation)

proba_predite <- predict(lasso, newx = as.matrix(base_test_1[,-1]), type = "response", s = lambda)
proba_predite <- as.numeric(proba_predite)
valeur <- as.numeric(base_test_1$Ynum)
pred <- prediction(proba_predite,valeur)

# Sensibilité
sens <- performance(pred,"sens")
plot(sens) # Représentation de la sensibilité en fonction du cutoff

# Sensibilité pour un cutoff de 0.12
sensitivity(base_test_1$Ynum,proba_predite,threshold = 0.12)
# Sensibilité  = 0.8095839

# Spécificité
spec <- performance(pred,"spec")
plot(spec) # Représentation de la spécificité en fonction du cutoff

# Spécificité pour un cutoff de 0.12
specificity(base_test_1$Ynum,proba_predite,threshold = 0.12)
# Spécificité = 0.8252338

# Accuracy
acc <- performance(pred,"acc")
plot(acc)

# Taux d'erreur
err <- performance(pred,"err")
plot(err)

# Taux d'erreur pour un cutoff de 0.12
misClassError(base_test_1$Ynum,proba_predite,threshold = 0.12)
# Taux d'erreur = 17,7%

# Courbe ROC
roc <- roc(base_test_1$Ynum,proba_predite)
plot(roc)

# Aire sous la courbe ROC
auc <- performance(pred,"auc")
auc@y.values
# AUC = 0.8909959

# Matrice de confusion pour un cutoff de 0.12
confusionMatrix(base_test_1$Ynum,proba_predite,threshold = 0.12)


#======================================================#
#   Comparaison des résultats obtenus : Courbes ROC    #
#======================================================#

# Régression logistique sur les variables dichotomisées 

fit.glm <- glm(formula = Ynum ~ maritalmarried + housingno + loanno + contactcellular + 
                 contacttelephone + poutcomefailure + poutcomeother + poutcomesuccess + 
                 cl_age22_35_55_ans + cl_balance21__100_ + cl_balance22_100_500_ + 
                 cl_balance23_500_3000_ + cl_day22__15_21_jours + cl_duration1____60s + 
                 cl_duration2__60_120s + cl_duration3__120_180s + cl_duration3__180_360s + 
                 cl_campaign1__1_campagne + cl_month1_Trim3 + Cl_job_1 + Cl_job_2 + 
                 Cl_job_3 + Cl_job_4 + cl_education_tertiary, family = binomial(link = logit), 
               data = base_dicho_train)

proba_predite <- predict(fit.glm, newdata = base_dicho_test, type = "response")
valeur <- as.numeric(base_dicho_test$Ynum)
pred <- prediction(proba_predite,valeur)
C1 <- confusionMatrix(base_dicho_test$Ynum,proba_predite,threshold = 0.12)

roc1 <- roc(base_dicho_test$Ynum,proba_predite)
plot(roc1,col = "red")


# Régression logistique sur les variables continues et catégorielles (dichotomisées)

fit.glm <- glm(formula = Ynum ~ balance + day + duration + campaign + maritaldivorced + 
                 maritalmarried + housingno + loanno + contactcellular + contacttelephone + 
                 poutcomefailure + poutcomeother + poutcomesuccess + cl_month1_Trim2 + cl_month1_Trim3 + Cl_job_1 + Cl_job_2 + 
                 Cl_job_3 + Cl_job_4 + cl_education_primary + cl_education_secondary, 
               family = binomial(link = logit), data = base_train_1)

proba_predite <- predict(fit.glm, newdata = base_test_1, type = "response")
valeur <- as.numeric(base_test_1$Ynum)
pred <- prediction(proba_predite,valeur)
C2 <- confusionMatrix(base_test_1$Ynum,proba_predite,threshold = 0.12)

roc2 <- roc(base_test_1$Ynum,proba_predite)
lines(roc2, col = "blue")


# Régression Logistique Lasso sur les variables dichotomisées 

Ynum_train <- base_dicho_train[,1]
Ynum_test <- base_dicho_test[1]
lasso <- glmnet(x = as.matrix(base_dicho_train[,-1]), y = Ynum_train, family = "binomial", alpha = 1)

cv_lasso <- cv.glmnet(x = as.matrix(base_dicho_train[,-1]), y = Ynum_train)
cv_lasso

lambda <- cv_lasso$lambda.min

proba_predite <- predict(lasso, newx = as.matrix(base_dicho_test[,-1]), type = "response", s = lambda)
proba_predite <- as.numeric(proba_predite)
valeur <- as.numeric(base_dicho_test$Ynum)
pred <- prediction(proba_predite,valeur)
C3 <- confusionMatrix(base_dicho_test$Ynum,proba_predite,threshold = 0.12)

roc3 <- roc(base_dicho_test$Ynum,proba_predite)
lines(roc3,col = "orange")


# Régression logistique Lasso sur les variables continues et catégorielles (dichotomisées) 

Ynum_train <- base_train_1[,1]
Ynum_test <- base_test_1[1]
lasso <- glmnet(x = as.matrix(base_train_1[,-1]), y = Ynum_train, family = "binomial", alpha = 1)

cv_lasso <- cv.glmnet(x = as.matrix(base_train_1[,-1]), y = Ynum_train)
cv_lasso

lambda <- cv_lasso$lambda.min

proba_predite <- predict(lasso, newx = as.matrix(base_test_1[,-1]), type = "response", s = lambda)
proba_predite <- as.numeric(proba_predite)
valeur <- as.numeric(base_test_1$Ynum)
pred <- prediction(proba_predite,valeur)
C4 <- confusionMatrix(base_test_1$Ynum,proba_predite,threshold = 0.12)

roc4 <- roc(base_test_1$Ynum,proba_predite)
lines(roc4,col = "green")

legend("bottomright",legend = c("Régression logistique (D)","Régression logistique (C + D)","Régression logistique Lasso (D)",
                                "Régression logistique Lasso (C + D)"), fill = c("red","blue","orange","green"))


#===============================================================#
#   Comparaison des résultats obtenus : Matrices de confusion   #
#===============================================================#

C1
C2
C3
C4
# On remarque que les matrices de confusion obtenues pour la régression logistique et la régression logistique Lasso 
# sur les variables dichotomisées sont quasimment identiques.
# Même constat pour les matrices de confusions obtenues pour la régression logistique et la régression logistique Lasso 
# sur les variables continues et catégorielles (dichotomisées).
# On remarque également que pour nos 4 modèles, le taux de faux positifs (environ 17%) est moins important 
# que le taux de faux négatifs (environ 19%)
