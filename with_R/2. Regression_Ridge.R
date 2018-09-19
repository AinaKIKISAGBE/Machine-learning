setwd("D:/Projet")
getwd()

# install.packages("questionr")
library(questionr)

### Importation des fichier  Train et Test ###
fichier_train <- read.csv2("Base_TRAIN.csv", sep = ",")
fichier_test <- read.csv2("Base_TEST.csv", sep = ",")

########################################
#### Vérification de la colinéarité ####
########################################

T_1 <- fichier_train[,c(5,6,7,9,10,11,19:25,27,28,32)]

cramer <- function(x, y) {
  res <- chisq.test(x, y, correct = FALSE)
  chi2 <- as.numeric(res$statistic)
  n <- length(x)
  p <- length(levels(x))
  q <- length(levels(y))
  m <- min(p - 1, q - 1)
  V <- sqrt(chi2/(n * m))
  return(V)
}

cramer(T_1$cl_job,T_1$marital)
cramer(T_1$cl_job,T_1$education)
cramer(T_1$cl_job,T_1$default)
cramer(T_1$cl_job,T_1$housing)
cramer(T_1$cl_job,T_1$loan)
cramer(T_1$cl_job,T_1$contact)
cramer(T_1$cl_job,T_1$cl_age)
cramer(T_1$cl_job,T_1$cl_balance)
cramer(T_1$cl_job,T_1$cl_day)
cramer(T_1$cl_job,T_1$cl_duration)
cramer(T_1$cl_job,T_1$cl_campaign)
cramer(T_1$cl_job,T_1$cl_pdays)
cramer(T_1$cl_job,T_1$cl_previous)
cramer(T_1$cl_job,T_1$cible)
cramer(T_1$cl_job,T_1$cl_month)

cramer(T_1$marital,T_1$education)
cramer(T_1$marital,T_1$default)
cramer(T_1$marital,T_1$housing)
cramer(T_1$marital,T_1$loan)
cramer(T_1$marital,T_1$contact)
cramer(T_1$marital,T_1$cl_age)
cramer(T_1$marital,T_1$cl_balance)
cramer(T_1$marital,T_1$cl_day)
cramer(T_1$marital,T_1$cl_duration)
cramer(T_1$marital,T_1$cl_campaign)
cramer(T_1$marital,T_1$cl_pdays)
cramer(T_1$marital,T_1$cl_previous)
cramer(T_1$marital,T_1$cible)
cramer(T_1$marital,T_1$cl_month)

cramer(T_1$education,T_1$default)
cramer(T_1$education,T_1$housing)
cramer(T_1$education,T_1$loan)
cramer(T_1$education,T_1$contact)
cramer(T_1$education,T_1$cl_age)
cramer(T_1$education,T_1$cl_balance)
cramer(T_1$education,T_1$cl_day)
cramer(T_1$education,T_1$cl_duration)
cramer(T_1$education,T_1$cl_campaign)
cramer(T_1$education,T_1$cl_pdays)
cramer(T_1$education,T_1$cl_previous)
cramer(T_1$education,T_1$cible)
cramer(T_1$education,T_1$cl_month)

cramer(T_1$default,T_1$housing)
cramer(T_1$default,T_1$loan)
cramer(T_1$default,T_1$contact)
cramer(T_1$default,T_1$cl_age)
cramer(T_1$default,T_1$cl_balance)
cramer(T_1$default,T_1$cl_day)
cramer(T_1$default,T_1$cl_duration)
cramer(T_1$default,T_1$cl_campaign)
cramer(T_1$default,T_1$cl_pdays)
cramer(T_1$default,T_1$cl_previous)
cramer(T_1$default,T_1$cible)
cramer(T_1$default,T_1$cl_month)

cramer(T_1$housing,T_1$loan)
cramer(T_1$housing,T_1$contact)
cramer(T_1$housing,T_1$cl_age)
cramer(T_1$housing,T_1$cl_balance)
cramer(T_1$housing,T_1$cl_day)
cramer(T_1$housing,T_1$cl_duration)
cramer(T_1$housing,T_1$cl_campaign)
cramer(T_1$housing,T_1$cl_pdays)
cramer(T_1$housing,T_1$cl_previous)
cramer(T_1$housing,T_1$cible)
cramer(T_1$housing,T_1$cl_month)

cramer(T_1$loan,T_1$contact)
cramer(T_1$loan,T_1$cl_age)
cramer(T_1$loan,T_1$cl_balance)
cramer(T_1$loan,T_1$cl_day)
cramer(T_1$loan,T_1$cl_duration)
cramer(T_1$loan,T_1$cl_campaign)
cramer(T_1$loan,T_1$cl_pdays)
cramer(T_1$loan,T_1$cl_previous)
cramer(T_1$loan,T_1$cible)
cramer(T_1$loan,T_1$cl_month)

cramer(T_1$contact,T_1$cl_age)
cramer(T_1$contact,T_1$cl_balance)
cramer(T_1$contact,T_1$cl_day)
cramer(T_1$contact,T_1$cl_duration)
cramer(T_1$contact,T_1$cl_campaign)
cramer(T_1$contact,T_1$cl_pdays)
cramer(T_1$contact,T_1$cl_previous)
cramer(T_1$contact,T_1$cible)
cramer(T_1$contact,T_1$cl_month)

cramer(T_1$cl_age,T_1$cl_balance)
cramer(T_1$cl_age,T_1$cl_day)
cramer(T_1$cl_age,T_1$cl_duration)
cramer(T_1$cl_age,T_1$cl_campaign)
cramer(T_1$cl_age,T_1$cl_pdays)
cramer(T_1$cl_age,T_1$cl_previous)
cramer(T_1$cl_age,T_1$cible)
cramer(T_1$cl_age,T_1$cl_month)

cramer(T_1$cl_balance,T_1$cl_day)
cramer(T_1$cl_balance,T_1$cl_duration)
cramer(T_1$cl_balance,T_1$cl_campaign)
cramer(T_1$cl_balance,T_1$cl_pdays)
cramer(T_1$cl_balance,T_1$cl_previous)
cramer(T_1$cl_balance,T_1$cible)
cramer(T_1$cl_balance,T_1$cl_month)

cramer(T_1$cl_day,T_1$cl_duration)
cramer(T_1$cl_day,T_1$cl_campaign)
cramer(T_1$cl_day,T_1$cl_pdays)
cramer(T_1$cl_day,T_1$cl_previous)
cramer(T_1$cl_day,T_1$cible)
cramer(T_1$cl_day,T_1$cl_month)

cramer(T_1$cl_duration,T_1$cl_campaign)
cramer(T_1$cl_duration,T_1$cl_pdays)
cramer(T_1$cl_duration,T_1$cl_previous)
cramer(T_1$cl_duration,T_1$cible)
cramer(T_1$cl_duration,T_1$cl_month)

cramer(T_1$cl_campaign,T_1$cl_pdays)
cramer(T_1$cl_campaign,T_1$cl_previous)
cramer(T_1$cl_campaign,T_1$cible)
cramer(T_1$cl_campaign,T_1$cl_month)

cramer(T_1$cl_pdays,T_1$cl_previous)
cramer(T_1$cl_pdays,T_1$cible)
cramer(T_1$cl_pdays,T_1$cl_month)

cramer(T_1$cl_previous,T_1$cible)
cramer(T_1$cl_previous,T_1$cl_month)

cramer(T_1$cible,T_1$cl_month)
######################################

# irec(fichier_train)

## Recodage de fichier_train$cl_previous en fichier_train$cl_previous_rec ###
fichier_train$cl_previous_rec <- as.character(fichier_train$cl_previous)
fichier_train$cl_previous_rec[fichier_train$cl_previous == ">1 contact"] <- "1"
fichier_train$cl_previous_rec[fichier_train$cl_previous == "0 contact"] <- "0"
fichier_train$cl_previous_rec <- as.factor(fichier_train$cl_previous_rec)

#### Dichotomisation de la variable Education ###
fichier_train$education_primary <- ifelse(fichier_train$education == "primary", "1","0" )
fichier_train$education_secondary <- ifelse(fichier_train$education == "secondary", "1","0" )
fichier_train$education_tertiary <- ifelse(fichier_train$education == "tertiary", "1","0" )
fichier_train$education_unknown <- ifelse(fichier_train$education == "unknown", "1","0" )

### On change le mode des nouvelles variables ###
fichier_train$education_primary <- as.factor(fichier_train$education_primary)
fichier_train$education_secondary <- as.factor(fichier_train$education_secondary)
fichier_train$education_tertiary <- as.factor(fichier_train$education_tertiary)
fichier_train$education_unknown <- as.factor(fichier_train$education_unknown)

### Préparation de la base ###
fichier_train_1 <- fichier_train[,c(33:73)]
fichier_train_2 <- fichier_train[,c(118:122)]
Base_Train <- cbind(fichier_train_1,fichier_train_2)
Base_Train <- Base_Train[,c(1,3:46)]

# install.packages("glm2")
# install.packages("glm.predict")

library(glm2)
library(glm.predict)

Base_Train$Ynum <- as.factor(Base_Train$Ynum)
Base_Train$Marital_D <- as.factor(Base_Train$Marital_D)
Base_Train$Marital_M <- as.factor(Base_Train$Marital_M)
Base_Train$Marital_S <- as.factor(Base_Train$Marital_S)
Base_Train$Default_0 <- as.factor(Base_Train$Default_0)
Base_Train$Default_1 <- as.factor(Base_Train$Default_1)
Base_Train$Housing_0 <- as.factor(Base_Train$Housing_0)
Base_Train$Housing_1 <- as.factor(Base_Train$Housing_1)
Base_Train$Loan_0 <- as.factor(Base_Train$Loan_0)
Base_Train$Loan_1 <- as.factor(Base_Train$Loan_1)
Base_Train$Contact_C <- as.factor(Base_Train$Contact_C)
Base_Train$Contact_T <- as.factor(Base_Train$Contact_T)
Base_Train$Contact_U <- as.factor(Base_Train$Contact_U)
Base_Train$Poutcome_F <- as.factor(Base_Train$Poutcome_F)
Base_Train$Poutcome_O <- as.factor(Base_Train$Poutcome_O)
Base_Train$Poutcome_S <- as.factor(Base_Train$Poutcome_S)
Base_Train$Poutcome_U <- as.factor(Base_Train$Poutcome_U)
Base_Train$Cl_age_1 <- as.factor(Base_Train$Cl_age_1)
Base_Train$Cl_age_2 <- as.factor(Base_Train$Cl_age_2)
Base_Train$Cl_age_3 <- as.factor(Base_Train$Cl_age_3)
Base_Train$Cl_balance_1 <- as.factor(Base_Train$Cl_balance_1)
Base_Train$Cl_balance_2 <- as.factor(Base_Train$Cl_balance_2)
Base_Train$Cl_balance_3 <- as.factor(Base_Train$Cl_balance_3)
Base_Train$Cl_balance_4 <- as.factor(Base_Train$Cl_balance_4)
Base_Train$Cl_day_1 <- as.factor(Base_Train$Cl_day_1)
Base_Train$Cl_day_2 <- as.factor(Base_Train$Cl_day_2)
Base_Train$Cl_day_3 <- as.factor(Base_Train$Cl_day_3)
Base_Train$Cl_duration_1 <- as.factor(Base_Train$Cl_duration_1)
Base_Train$Cl_duration_2 <- as.factor(Base_Train$Cl_duration_2)
Base_Train$Cl_duration_3 <- as.factor(Base_Train$Cl_duration_3)
Base_Train$Cl_duration_4 <- as.factor(Base_Train$Cl_duration_4)
Base_Train$Cl_duration_5 <- as.factor(Base_Train$Cl_duration_5)
Base_Train$Cl_campaign_1 <- as.factor(Base_Train$Cl_campaign_1)
Base_Train$Cl_campaign_2 <- as.factor(Base_Train$Cl_campaign_2)
Base_Train$Cl_campaign_3 <- as.factor(Base_Train$Cl_campaign_3)
Base_Train$Cl_campaign_4 <- as.factor(Base_Train$Cl_campaign_4)
Base_Train$Cl_month_1 <- as.factor(Base_Train$Cl_month_1)
Base_Train$Cl_month_2 <- as.factor(Base_Train$Cl_month_2)
Base_Train$Cl_month_3 <- as.factor(Base_Train$Cl_month_3)
Base_Train$Cl_month_4 <- as.factor(Base_Train$Cl_month_4)

Base_train_ridge <- Base_Train
Base_Train <- Base_Train[,-c(37:45)]


########################################
#### Vérification de la colinéarité ####
########################################

T_2 <- fichier_test[,c(5,6,7,9,10,11,19:25,27,28,32)]

cramer <- function(x, y) {
  res <- chisq.test(x, y, correct = FALSE)
  chi2 <- as.numeric(res$statistic)
  n <- length(x)
  p <- length(levels(x))
  q <- length(levels(y))
  m <- min(p - 1, q - 1)
  V <- sqrt(chi2/(n * m))
  return(V)
}

cramer(T_2$cl_job,T_2$marital)
cramer(T_2$cl_job,T_2$education)
cramer(T_2$cl_job,T_2$default)
cramer(T_2$cl_job,T_2$housing)
cramer(T_2$cl_job,T_2$loan)
cramer(T_2$cl_job,T_2$contact)
cramer(T_2$cl_job,T_2$cl_age)
cramer(T_2$cl_job,T_2$cl_balance)
cramer(T_2$cl_job,T_2$cl_day)
cramer(T_2$cl_job,T_2$cl_duration)
cramer(T_2$cl_job,T_2$cl_campaign)
cramer(T_2$cl_job,T_2$cl_pdays)
cramer(T_2$cl_job,T_2$cl_previous)
cramer(T_2$cl_job,T_2$cl_month)

cramer(T_2$marital,T_2$education)
cramer(T_2$marital,T_2$default)
cramer(T_2$marital,T_2$housing)
cramer(T_2$marital,T_2$loan)
cramer(T_2$marital,T_2$contact)
cramer(T_2$marital,T_2$cl_age)
cramer(T_2$marital,T_2$cl_balance)
cramer(T_2$marital,T_2$cl_day)
cramer(T_2$marital,T_2$cl_duration)
cramer(T_2$marital,T_2$cl_campaign)
cramer(T_2$marital,T_2$cl_pdays)
cramer(T_2$marital,T_2$cl_previous)
cramer(T_2$marital,T_2$cl_month)

cramer(T_2$education,T_2$default)
cramer(T_2$education,T_2$housing)
cramer(T_2$education,T_2$loan)
cramer(T_2$education,T_2$contact)
cramer(T_2$education,T_2$cl_age)
cramer(T_2$education,T_2$cl_balance)
cramer(T_2$education,T_2$cl_day)
cramer(T_2$education,T_2$cl_duration)
cramer(T_2$education,T_2$cl_campaign)
cramer(T_2$education,T_2$cl_pdays)
cramer(T_2$education,T_2$cl_previous)
cramer(T_2$education,T_2$cl_month)

cramer(T_2$default,T_2$housing)
cramer(T_2$default,T_2$loan)
cramer(T_2$default,T_2$contact)
cramer(T_2$default,T_2$cl_age)
cramer(T_2$default,T_2$cl_balance)
cramer(T_2$default,T_2$cl_day)
cramer(T_2$default,T_2$cl_duration)
cramer(T_2$default,T_2$cl_campaign)
cramer(T_2$default,T_2$cl_pdays)
cramer(T_2$default,T_2$cl_previous)
cramer(T_2$default,T_2$cl_month)

cramer(T_2$housing,T_2$loan)
cramer(T_2$housing,T_2$contact)
cramer(T_2$housing,T_2$cl_age)
cramer(T_2$housing,T_2$cl_balance)
cramer(T_2$housing,T_2$cl_day)
cramer(T_2$housing,T_2$cl_duration)
cramer(T_2$housing,T_2$cl_campaign)
cramer(T_2$housing,T_2$cl_pdays)
cramer(T_2$housing,T_2$cl_previous)
cramer(T_2$housing,T_2$cl_month)

cramer(T_2$loan,T_2$contact)
cramer(T_2$loan,T_2$cl_age)
cramer(T_2$loan,T_2$cl_balance)
cramer(T_2$loan,T_2$cl_day)
cramer(T_2$loan,T_2$cl_duration)
cramer(T_2$loan,T_2$cl_campaign)
cramer(T_2$loan,T_2$cl_pdays)
cramer(T_2$loan,T_2$cl_previous)
cramer(T_2$loan,T_2$cl_month)

cramer(T_2$contact,T_2$cl_age)
cramer(T_2$contact,T_2$cl_balance)
cramer(T_2$contact,T_2$cl_day)
cramer(T_2$contact,T_2$cl_duration)
cramer(T_2$contact,T_2$cl_campaign)
cramer(T_2$contact,T_2$cl_pdays)
cramer(T_2$contact,T_2$cl_previous)
cramer(T_2$contact,T_2$cl_month)

cramer(T_2$cl_age,T_2$cl_balance)
cramer(T_2$cl_age,T_2$cl_day)
cramer(T_2$cl_age,T_2$cl_duration)
cramer(T_2$cl_age,T_2$cl_campaign)
cramer(T_2$cl_age,T_2$cl_pdays)
cramer(T_2$cl_age,T_2$cl_previous)
cramer(T_2$cl_age,T_2$cl_month)

cramer(T_2$cl_balance,T_2$cl_day)
cramer(T_2$cl_balance,T_2$cl_duration)
cramer(T_2$cl_balance,T_2$cl_campaign)
cramer(T_2$cl_balance,T_2$cl_pdays)
cramer(T_2$cl_balance,T_2$cl_previous)
cramer(T_2$cl_balance,T_2$cl_month)

cramer(T_2$cl_day,T_2$cl_duration)
cramer(T_2$cl_day,T_2$cl_campaign)
cramer(T_2$cl_day,T_2$cl_pdays)
cramer(T_2$cl_day,T_2$cl_previous)
cramer(T_2$cl_day,T_2$cl_month)

cramer(T_2$cl_duration,T_2$cl_campaign)
cramer(T_2$cl_duration,T_2$cl_pdays)
cramer(T_2$cl_duration,T_2$cl_previous)
cramer(T_2$cl_duration,T_2$cl_month)

cramer(T_2$cl_campaign,T_2$cl_pdays)
cramer(T_2$cl_campaign,T_2$cl_previous)
cramer(T_2$cl_campaign,T_2$cl_month)

cramer(T_2$cl_pdays,T_2$cl_previous)
cramer(T_2$cl_pdays,T_2$cl_month)


cramer(T_2$cl_previous,T_2$cl_month)

######################################

#####################################################################
########## On refait la même chose pour la base test ################
#####################################################################

## Recodage de fichier_test$cl_previous en fichier_test$cl_previous_rec
fichier_test$cl_previous_rec <- as.character(fichier_test$cl_previous)
fichier_test$cl_previous_rec[fichier_test$cl_previous == ">1 contact"] <- "1"
fichier_test$cl_previous_rec[fichier_test$cl_previous == "0 contact"] <- "0"
fichier_test$cl_previous_rec <- as.factor(fichier_test$cl_previous_rec)

#### Dichotomisation de la variable Education
fichier_test$education_primary <- ifelse(fichier_test$education == "primary", "1","0" )
fichier_test$education_secondary <- ifelse(fichier_test$education == "secondary", "1","0" )
fichier_test$education_tertiary <- ifelse(fichier_test$education == "tertiary", "1","0" )
fichier_test$education_unknown <- ifelse(fichier_test$education == "unknown", "1","0" )

fichier_test$education_primary <- as.factor(fichier_test$education_primary)
fichier_test$education_secondary <- as.factor(fichier_test$education_secondary)
fichier_test$education_tertiary <- as.factor(fichier_test$education_tertiary)
fichier_test$education_unknown <- as.factor(fichier_test$education_unknown)


fichier_test_1 <- fichier_test[,c(33:73)]
fichier_test_2 <- fichier_test[,c(118:122)]
Base_Test <- cbind(fichier_test_1,fichier_test_2)
Base_Test <- Base_Test[,c(1,3:46)]

Base_Test$Ynum <- as.factor(Base_Test$Ynum)
Base_Test$Marital_D <- as.factor(Base_Test$Marital_D)
Base_Test$Marital_M <- as.factor(Base_Test$Marital_M)
Base_Test$Marital_S <- as.factor(Base_Test$Marital_S)
Base_Test$Default_0 <- as.factor(Base_Test$Default_0)
Base_Test$Default_1 <- as.factor(Base_Test$Default_1)
Base_Test$Housing_0 <- as.factor(Base_Test$Housing_0)
Base_Test$Housing_1 <- as.factor(Base_Test$Housing_1)
Base_Test$Loan_0 <- as.factor(Base_Test$Loan_0)
Base_Test$Loan_1 <- as.factor(Base_Test$Loan_1)
Base_Test$Contact_C <- as.factor(Base_Test$Contact_C)
Base_Test$Contact_T <- as.factor(Base_Test$Contact_T)
Base_Test$Contact_U <- as.factor(Base_Test$Contact_U)
Base_Test$Poutcome_F <- as.factor(Base_Test$Poutcome_F)
Base_Test$Poutcome_O <- as.factor(Base_Test$Poutcome_O)
Base_Test$Poutcome_S <- as.factor(Base_Test$Poutcome_S)
Base_Test$Poutcome_U <- as.factor(Base_Test$Poutcome_U)
Base_Test$Cl_age_1 <- as.factor(Base_Test$Cl_age_1)
Base_Test$Cl_age_2 <- as.factor(Base_Test$Cl_age_2)
Base_Test$Cl_age_3 <- as.factor(Base_Test$Cl_age_3)
Base_Test$Cl_balance_1 <- as.factor(Base_Test$Cl_balance_1)
Base_Test$Cl_balance_2 <- as.factor(Base_Test$Cl_balance_2)
Base_Test$Cl_balance_3 <- as.factor(Base_Test$Cl_balance_3)
Base_Test$Cl_balance_4 <- as.factor(Base_Test$Cl_balance_4)
Base_Test$Cl_day_1 <- as.factor(Base_Test$Cl_day_1)
Base_Test$Cl_day_2 <- as.factor(Base_Test$Cl_day_2)
Base_Test$Cl_day_3 <- as.factor(Base_Test$Cl_day_3)
Base_Test$Cl_duration_1 <- as.factor(Base_Test$Cl_duration_1)
Base_Test$Cl_duration_2 <- as.factor(Base_Test$Cl_duration_2)
Base_Test$Cl_duration_3 <- as.factor(Base_Test$Cl_duration_3)
Base_Test$Cl_duration_4 <- as.factor(Base_Test$Cl_duration_4)
Base_Test$Cl_duration_5 <- as.factor(Base_Test$Cl_duration_5)
Base_Test$Cl_campaign_1 <- as.factor(Base_Test$Cl_campaign_1)
Base_Test$Cl_campaign_2 <- as.factor(Base_Test$Cl_campaign_2)
Base_Test$Cl_campaign_3 <- as.factor(Base_Test$Cl_campaign_3)
Base_Test$Cl_campaign_4 <- as.factor(Base_Test$Cl_campaign_4)
Base_Test$Cl_month_1 <- as.factor(Base_Test$Cl_month_1)
Base_Test$Cl_month_2 <- as.factor(Base_Test$Cl_month_2)
Base_Test$Cl_month_3 <- as.factor(Base_Test$Cl_month_3)
Base_Test$Cl_month_4 <- as.factor(Base_Test$Cl_month_4)

Base_Test_ridge <- Base_Test
Base_Test <- Base_Test[,-c(37:45)]
#####################################################################

# install.packages("corrplot")
# install.packages("RColorBrewer")
#library(corrplot)
#library(RColorBrewer)

#col_var <- data.frame(cor(train_col))
#corrplot.mixed(cor(train_col), col = rev(brewer.pal(40, "Spectral")))
#chisq.test(Base_Train$Marital_D,Base_Train$Housing_0)
#####################################################################

### Modèle de regression logistique ###
reg_log <- glm(Ynum~., data = Base_Train, family = binomial(link = "logit"))
summary(reg_log)

Base_Train <- Base_Train[,-c(2,4,6,8,10,13,17,20,24,27,32,36)]

reg_log <- glm(Ynum~., data = Base_Train, family = binomial(link = "logit"))
summary(reg_log)

Base_Train <- Base_Train[,-c(3,16)]

reg_log <- glm(Ynum~., data = Base_Train, family = binomial(link = "logit"))
summary(reg_log)
### Probabilité de la variable Ynum ###
predict_train <- data.frame(predict_ynum=predict(reg_log))

# Choix du meilleur modele
# modselect=stepAIC(reg_log,~.,trace=TRUE,
#                  direction=c("both"))
#both est l'option par défaut
#summary(modselect)


### Modèle de regression logistique, avec le modele qui minimise l'aic ###
#reg_log <- glm(Ynum ~ Marital_M + Housing_0 + Loan_0 + Contact_C + Contact_T + 
#                 Poutcome_F + Poutcome_S + Cl_age_1 + Cl_age_2 + Cl_balance_1 + 
#                 Cl_balance_2 + Cl_balance_3 + Cl_day_2 + Cl_duration_1 + 
#                 Cl_duration_2 + Cl_duration_3 + Cl_duration_4 + Cl_campaign_1 + 
#                 Cl_month_3 + cl_previous_rec + education_primary + education_tertiary + 
#                 Cl_campaign_4, data = Base_Train, family = binomial(link = "logit"))


### recodage de la variable pred_ynum en factor, on impose un seuil de 0.11 ###
### qui corresspond à la proportion de 1 dans la base ###
predict_train$pred_ynum <- ifelse(predict_train$predict_ynum <= 0.79,"0","1")
predict_train$pred_ynum <- as.factor(predict_train$pred_ynum)
summary(predict_train$pred_ynum)
summary(Base_Train$Ynum)

### matrice de confusion ###
m.confusion <- as.matrix(table(Base_Train$Ynum, predict_train$pred_ynum))

# Taux d'erreur
Tx_err <- function(y, ypred) {
  mc <- table(y, ypred)
  error <- (mc[1, 2] + mc[2, 1])/sum(mc)
  print(error)
}
Tx_err(Base_Train$Ynum, predict_train$pred_ynum)



#### on applique notre modèle à la base Test ####
Base_Test_1 <- Base_Test[,-c(2,4,6,8,10,13,17,20,24,27,32,36)]
Base_Test_1 <- Base_Test_1[,-c(3,16)]
Base_Test_1 <- Base_Test_1[,c(2:22)]

Base_Test_1 <- cbind(Base_Test_1, predict(reg_log, newdata = Base_Test_1, type = "response", se = TRUE))
Base_Test_1$fit_pred <- ifelse(Base_Test_1$fit <= 0.79,"0","1")
Base_Test_1$fit_pred <- as.factor(Base_Test_1$fit_pred)
m.confusiontest <- as.matrix(table(Base_Test$Ynum, Base_Test_1$fit_pred))

Tx_err(Base_Test$Ynum, Base_Test_1$fit_pred)

### Performance ###
# install.packages("ROCR")
library(ROCR)

### Performance sur la base train ###
proba_predite <- predict(reg_log,newdata = Base_Train[,c(2:22)], type = "response")
valeur <- as.numeric(as.character(Base_Train$Ynum))
pred <- prediction(proba_predite, valeur)

performance(pred, measure = "auc") # aire sous la courbe ROC = 0.88
pred_sens <- performance(pred, measure = "sens", x.measure = "cutoff")
x <- (performance(pred, measure = "sens", x.measure = "cutoff")) # taux de prédit pour 1
y <- (performance(pred, measure = "spec", x.measure = "cutoff")) # taux de prédit pour 0


e <- data.frame(x@x.values,x@y.values)
names(e) <- c("x","y")
plot(e, typ="l")

f <- data.frame(y@x.values,y@y.values)
names(f) <- c("x","y")
plot(f, typ="l")

plot(e, typ="l")
lines(f)
g <- data.frame(g=abs(e$y-f$y))
which.min(g$g)
g[2371,]


### Courbe de ROC ####

roc <-performance(pred, "tpr", "fpr") # aire sous la courbe ROC = 0.87
plot(roc,
     colorize = T,
     main= "Courbe de ROC",
     ylab="sensitivity",
     xlab="1 - Specificity")
abline(a=0, b=1)

###AUC###
auc <- performance (pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round (auc, 4)
legend(.8, .4, auc, title="AUC", cex= 0.8)


### Performance sur la base Test ###
proba_predite <- predict(reg_log,newdata = Base_Test[,c(2:22)], type = "response")
valeur <- as.numeric(as.character(Base_Train$Ynum))
pred <- prediction(proba_predite, valeur)

performance(pred, measure = "auc") # aire sous la courbe ROC = 0.88
pred_sens <- performance(pred, measure = "sens", x.measure = "cutoff")
x <- (performance(pred, measure = "sens", x.measure = "cutoff")) # taux de prédit pour 1
y <- (performance(pred, measure = "spec", x.measure = "cutoff")) # taux de prédit pour 0

# install.packages("arulesViz")
library(arulesViz)
e <- data.frame(x@x.values,x@y.values)
names(e) <- c("x","y")
plot(e, typ="l")

f <- data.frame(y@x.values,y@y.values)
names(f) <- c("x","y")
plot(f, typ="l")

plot(e, typ="l")
lines(f)
g <- data.frame(g=abs(e$y-f$y))
which.min(g$g)
g[1553,]

# pour determiner le point d'intersection
# h<-locator()
### Courbe de ROC ####

roc <-performance(pred, "tpr", "fpr") # aire sous la courbe ROC = 0.88
plot(roc,
     colorize = T,
     main= "Courbe de ROC",
     ylab="sensitivity",
     xlab="1 - Specificity")
abline(a=0, b=1)

###AUC###
auc <- performance (pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round (auc, 4)
legend(.8, .4, auc, title="AUC", cex= 0.8)


################################################
##### Regression logistique pénalisée Ridge ####
################################################

#install.packages("glmnet")
library(glmnet)

### Base Train ###

x=model.matrix(Ynum~.-1,data=Base_train_ridge) 
x<-as.matrix(data.frame(x))
y=Base_train_ridge$Ynum
y=as.factor(y)

Ridge_model_cv=cv.glmnet(x,y,alpha=0,family='binomial')
plot(Ridge_model_cv)

# lambda = 0.01178993 --> valeurs optimale de lambda
Ridge_model_cv$lambda.min

Ridge_model_train=glmnet(x,y,alpha=0,family='binomial', lambda = Ridge_model_cv$lambda.min)
plot(Ridge_model_train,xvar="lambda")
grid()
summary(Ridge_model_train)

a <- predict(Ridge_model_train, newx = x[1:31649,], type = "class", s = c(0.01178993))
a <- as.factor(a)
a <- data.frame(a)

# matrice de confusion
m.confusion <- as.matrix(table(Base_Train$Ynum, a$a))

# Taux d'erreur
Tx_err(Base_Train$Ynum, a$a)

### Base Test ###

x1=model.matrix(Ynum~.-1,data=Base_Test_ridge) 
x1<-as.matrix(data.frame(x1))
y1=Base_Test_ridge$Ynum
y1=as.factor(y1)

b <- predict(Ridge_model_train, newx = x1[1:13562,],type = "class", s = c(0.01178993))
b <- as.factor(b)
b <- data.frame(b)

# matrice de confusion
m.confusiontest <- as.matrix(table(Base_Test$Ynum, b$b))

# Taux d'erreur
Tx_err(Base_Test$Ynum, b$b)

# Performance sur la base Train
proba_predite_R <- predict(Ridge_model_train,newx = x[1:31649,], type = "class", s = c(0.01178993))
valeur_R <- as.numeric(as.character(Base_train_ridge$Ynum))
pred_R <- prediction(as.numeric(as.character(proba_predite_R)), valeur_R)

performance(pred_R, measure = "auc") 
pred_sens <- performance(pred_R, measure = "sens", x.measure = "cutoff")
j <- (performance(pred_R, measure = "sens", x.measure = "cutoff")) # taux de prédit pour 1
l <- (performance(pred_R, measure = "spec", x.measure = "cutoff")) # taux de prédit pour 0

### Courbe de ROC ####

roc <-performance(pred_R, "tpr", "fpr") # aire sous la courbe ROC = 0.61
plot(roc,
     colorize = T,
     main= "Courbe de ROC",
     ylab="sensitivity",
     xlab="1 - Specificity")
abline(a=0, b=1)

###AUC###
auc <- performance (pred_R, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round (auc, 4)
legend(.8, .4, auc, title="AUC", cex= 0.8)



# Performance sur la base Test
proba_predite_R <- predict(Ridge_model_train,newx = x1[1:13562,], type = "class", s = c(0.01178993))
valeur_R <- as.numeric(as.character(Base_Test_ridge$Ynum))
pred_R <- prediction(as.numeric(as.character(proba_predite_R)), valeur_R)

performance(pred_R, measure = "auc") 
pred_sens <- performance(pred_R, measure = "sens", x.measure = "cutoff")
x <- (performance(pred_R, measure = "sens", x.measure = "cutoff")) # taux de prédit pour 1
y <- (performance(pred_R, measure = "spec", x.measure = "cutoff")) # taux de prédit pour 0

### Courbe de ROC ####

roc <-performance(pred_R, "tpr", "fpr") # aire sous la courbe ROC = 0.61
plot(roc,
     colorize = T,
     main= "Courbe de ROC",
     ylab="sensitivity",
     xlab="1 - Specificity")
abline(a=0, b=1)

###AUC###
auc <- performance (pred_R, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round (auc, 4)
legend(.8, .4, auc, title="AUC", cex= 0.8)

