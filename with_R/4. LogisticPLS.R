#################### Chargement des packages ci-dessous #################### 
setwd("D:/projet/")
getwd()
#install.packages("broom")
#install.packages("questionr")
#install.packages("plsRglm")
#install.packages("ROCR")
#install.packages("GGally")
#install.packages("devtools")
#devtools::install_github("larmarange/JLutils")
#install.packages(c("FactoMineR", "factoextra"))
#install.packages("effects")
library (readr)
library(JLutils)
library(broom)
library(lmtest)
library(questionr)
library(plsRglm)
library(ROCR)
library(GGally)
library(devtools)
library(factoextra)
library(FactoMineR)
library(effects)
##### Fonctions calcul des corrélations et des taux d'erreurs ##### 
cram = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}
Tx_err <- function(y, ypred) {
  mc <- table(y, ypred)
  error <- (mc[1, 2] + mc[2, 1])/sum(mc)
  print(error)
}
#############################################################################

### Importation de l'échantillon d'apprentissage et de l'échantillon test/validation ###
# ECHANTILLON DE VALIDATION 30% #
ech_valid<-read_csv("C:/Users/P.Lhostis/Documents/Master/CLASSIFICATION/Base_TEST.csv")
# ECHANTILLON D'APPRENTISSAGE 70%#
ech_test<-read_csv("C:/Users/P.Lhostis/Documents/Master/CLASSIFICATION/Base_TRAIN.csv")

## Identification de doublon de nos individus ##
doublons <- which(duplicated(ech_test$cle)) 
# Aucun doublon dans nos individus #

### On va maintenant sélectionner nos variables à l'aide d'un V de Cramer ###
### Suppression de nos variable quanti, prédictions d'une dummy ###
### On effectue ensuite une sélection stepwise sur notre régression logistique ###

# 1 . Observation de notre variable cible et des variables explicatives ##
ech_test$y<-as.factor(ech_test$y)
levels(ech_test$y)
ech_test$y<-relevel(ech_test$y,"no")
freq(ech_test$y)
# 0 -> n=11976 = 88.3%
# 1-> n=1586 = 11.7%

### Profession ###
ech_test$job<-as.factor(ech_test$job)
levels(ech_test$job)
freq(ech_test$job)
#Classes sours représentées => observation classe#
ech_test$cl_job<-as.factor(ech_test$cl_job)
levels(ech_test$cl_job)
freq(ech_test$cl_job)
cram(ech_test$y,ech_test$cl_job)
## V=0.1274

### Statut Marital ###
ech_test$marital<-as.factor(ech_test$marital)
levels(ech_test$marital)
freq(ech_test$marital)
ech_test$marital<-relevel(ech_test$marital,ref="married")
cram(ech_test$y,ech_test$marital)
## V=0.0678

### Niveau education ###
ech_test$education<-as.factor(ech_test$education)
levels(ech_test$education)
freq(ech_test$education)
ech_test$education<-relevel(ech_test$education,"secondary")
cram(ech_test$y,ech_test$education)
## V=0.07557
## Sous représentation de la classe unknown

### Defaut de credit ###
ech_test$default<-as.factor(ech_test$default)
levels(ech_test$default)
freq(ech_test$default)
cram(ech_test$y,ech_test$default)
## V = 0.0218

### pret immobilier ###
ech_test$housing<-as.factor(ech_test$housing)
levels(ech_test$housing)
freq(ech_test$housing)
ech_test$housing<-relevel(ech_test$housing,"yes")
cram(ech_test$y,ech_test$housing)
##V = 0.14245

### pret personnel ###
ech_test$loan<-as.factor(ech_test$loan)
levels(ech_test$loan)
freq(ech_test$loan)
cram(ech_test$y,ech_test$loan)
##V = 0.0674

### Contact ###
ech_test$contact<-as.factor(ech_test$contact)
levels(ech_test$contact)
freq(ech_test$contact)
cram(ech_test$y,ech_test$contact)
##V=0.1474

### resulat campagne marketing ###
ech_test$poutcome<-as.factor(ech_test$poutcome)
levels(ech_test$poutcome)
freq(ech_test$poutcome)
ech_test$poutcome<-relevel(ech_test$poutcome,"unknown")
cram(ech_test$y,ech_test$poutcome)
##V=0.3164
## Attention V cramer élevé mais le canal unknown est présent à 81.7% 
## Si conserver, faire attention à l'individu type

### Classe age ###
boxplot(ech_test$age)
ech_test$cl_age<-as.factor(ech_test$cl_age)
levels(ech_test$cl_age)
freq(ech_test$cl_age)
cram(ech_test$y,ech_test$cl_age)
##V=0.0843
## Attention à la mise en classe des individus abérrants

## Comparaison cl_age2 ##
ech_test$cl_age2<-as.factor(ech_test$cl_age2)
freq(ech_test$cl_age2)
ech_test$cl_age2<-relevel(ech_test$cl_age2,"2.35-55 ans")
cram(ech_test$y,ech_test$cl_age2)
## V=0.0841
## Observer si l'une reste à la regression stepwise ##

### Solde du compte ###
boxplot(ech_test$balance)
ech_test$cl_balance<-as.factor(ech_test$cl_balance)
levels(ech_test$cl_balance)
ech_test$cl_balance<-relevel(ech_test$cl_balance,"3.100-500\u0080")
freq(ech_test$cl_balance)
cram(ech_test$y,ech_test$cl_balance)
### Attention a la modalite de reference, pb avec le signe euros ##
## V= 0.0998
## Seconde classe #
ech_test$cl_balance2<-as.factor(ech_test$cl_balance2)
levels(ech_test$cl_balance2)
ech_test$cl_balance<-relevel(ech_test$cl_balance,"3.100-500\u0080")
freq(ech_test$cl_balance2)
cram(ech_test$y,ech_test$cl_balance2)
## V=0.0953 

### Jour de la semaine du dernier contact ###
ech_test$cl_day<-as.factor(ech_test$cl_day)
levels(ech_test$cl_day)
ech_test$cl_day<-relevel(ech_test$cl_day,"3. 15-21 jours")
freq(ech_test$cl_day)
cram(ech_test$y,ech_test$cl_day)
## V= 0.0432
## classe de jour 2
ech_test$cl_day2<-as.factor(ech_test$cl_day2)
levels(ech_test$cl_day2)
freq(ech_test$cl_day2)
cram(ech_test$y,ech_test$cl_day2)
## V= 0.0427

### duree du premier contact ###
boxplot(ech_test$duration)
ech_test$cl_duration<-as.factor(ech_test$cl_duration)
levels(ech_test$cl_duration)
ech_test$cl_duration<-relevel(ech_test$cl_duration,"3. 120-180s")
freq(ech_test$cl_duration)
cram(ech_test$y,ech_test$cl_duration)
## V= 0.3563

### Nombre de contact réalisé ###
boxplot(ech_test$campaign)
ech_test$cl_campaign<-as.factor(ech_test$cl_campaign)
levels(ech_test$cl_campaign)
freq(ech_test$cl_campaign)
cram(ech_test$y,ech_test$cl_campaign)
## V= 0.087

### Nb jours depuis derniers contacts ###
boxplot(ech_test$pdays)
ech_test$cl_pdays<-as.factor(ech_test$cl_pdays)
levels(ech_test$cl_pdays)
ech_test$cl_pdays<-relevel(ech_test$cl_pdays,"0 day")
freq(ech_test$cl_pdays)
cram(ech_test$y,ech_test$cl_pdays)
## V= 0.1668

### Nb contacts avant cette campagne ###
boxplot(ech_test$previous)
ech_test$cl_previous<-as.factor(ech_test$cl_previous)
levels(ech_test$cl_previous)
ech_test$cl_previous<-relevel(ech_test$cl_previous,"0 contact")
freq(ech_test$cl_previous)
cram(ech_test$y,ech_test$cl_previous)
## V= 0.1668

### Mois du dernier contact###
ech_test$cl_month<-as.factor(ech_test$cl_month)
levels(ech_test$cl_month)
ech_test$cl_month<-relevel(ech_test$cl_month,"1.Trim2")
freq(ech_test$cl_month)
cram(ech_test$y,ech_test$cl_month)
## V= 0.09837

### Régression logistique et sélection du modèle minimisant l'aic ###
reg <- glm(y ~ 1, 
           data = ech_test, family = binomial(logit))
model.aic.both <- step(reg, direction = "both", trace = 1, 
                       scope = ~ marital + education + default + housing + loan + contact+poutcome+cl_age+cl_age2+
                       cl_balance+cl_balance2+cl_day+cl_day2+cl_duration+cl_campaign+cl_pdays+cl_previous+cl_month+cl_job)
summary(model.aic.both)

### On ne retient que les variables avec une significativité >95% ###
reg<-glm(y ~ cl_duration + poutcome + housing +  marital + cl_campaign + loan + cl_day + cl_age2,data=ech_test, family = binomial(logit))
reg2<-step(reg)
summary(reg2)
# Test des résidus studentsés # 
bptest(reg2)
par(mfrow = c(1, 1))
plot(rstudent(reg2), type = "p", cex = 0.5, ylab = "Résidus studentisés ", 
     col = "springgreen2", ylim = c(-3, 3))
abline(h = c(-2, 2), col = "red")
### Correction de white devra être faîte afin de régler les problèmes d'htéroscédasticité###

###### ODDS-RATIOS #####
#### Récupération des odds ratios ####
drop1(reg2,.~., test = "Chisq")
exp(cbind(OR=coef(reg2),confint(reg2)))
tmp <- tidy(reg2, conf.int = TRUE, exponentiate = TRUE)
str(tmp)
ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
                  xmax = conf.high) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()

ggcoef(reg2, exponentiate = TRUE)

td <- tidy_detailed(reg2, exponentiate = TRUE, conf.int = TRUE)
td$label <- factor(td$label, rev(td$label))  # Pour fixer l'ordre pour ggplot2
ggcoef(td, mapping = aes(y = level_detail, x = estimate, 
                         colour = variable_label), exponentiate = TRUE)
#### Résumé des odds ratios par variables ####
plot(allEffects(reg2))
summary(reg2)

############ Passage à un modèle avec les dummy pour supprimer de la colinéarité ############
### Observations du taux d'erreur avec les variables dummy ###

regbi<-glm(y ~ Cl_duration_2 +Cl_duration_3 + Cl_duration_4 + Cl_duration_5 +
             Poutcome_F + Poutcome_O +Poutcome_S + 
             cl_balance24__3000_+ cl_balance22_100_500_ + cl_balance23_500_3000_ + 
             maritaldivorced + maritalsingle +
             Loan_0 + Housing_0 +
             Cl_day_1 + Cl_day_3 +
             Cl_age_1 + Cl_age_3,data=ech_test,family = binomial(logit))
summary(regbi)

### Récupération de l'AUC, tpr, fpr###
proba_predite<-predict(regbi, type="response")
P<-proba_predite
pred=prediction(P,ech_test$y)
perf=performance(pred,"tpr", "fpr")
spec=performance(pred,"spec")
sens=performance(pred,"sens")
auc=performance(pred,"auc")
plot(perf,colorize = TRUE)

# On choisit le seuil qui minimise la différence entre la sensibilité et la spécificité
# spécificité en valeur absolue
sensitivity <- sens@y.values[[1]]
specificity <- spec@y.values[[1]]
diffspecificity <- 1-specificity
diff <- abs(sensitivity-specificity)
tab_indicateurs <- data.frame(diff, diffspecificity,cutoff_optimal=spec@x.values[[1]], sensitivity, specificity)

# Calcul de la proba optimal #
cutoff <- tab_indicateurs[tab_indicateurs$diff==min(tab_indicateurs$diff),"cutoff_optimal"]

plot(x = tab_indicateurs$cutoff_optimal, y = tab_indicateurs$diff,type='l', 
     main = "cutoff optimal", xlab = "Cutoff", ylab="sensitivity-specificity")
points(x=cutoff, y=0, pch=16)
## Diagramme du croisement de la sensibilité et de la spécificité ##
plot(y = tab_indicateurs$sensitivity, x = tab_indicateurs$cutoff_optimal , type="l", col = "red", 
     main = "cutoff optimal", xlab = "cutoff", ylab = "")
lines(y=tab_indicateurs$specificity,x = tab_indicateurs$cutoff_optimal)

## On conserve la valeur optimal au croisement de la courbe sensibilité et spécificité ##
appren.p <- cbind(ech_test, predict(regbi, newdata = ech_test, type = "link", 
                                se = TRUE))

## Récupération de la proba prédite et de l'IC ##
appren.p <- within(appren.p, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## On fixe la proba>0.13 croisement spécificité et sensibilité ##
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 
                                                       cutoff, 1, 0)))
m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$y))
m.confusion <- unclass(m.confusion)
# Taux d'erreur
Tx_err(appren.p$pred.chd, appren.p$y)

############################### Comparaison du modèle sur l'échantillon test ##############
ech_valid<-read_csv("C:/Users/P.Lhostis/Documents/Master/CLASSIFICATION/Base_TEST.csv")
ech_valid$y<-as.factor(ech_valid$y)
ech_valid$cl_duration<-as.factor(ech_valid$cl_duration)
ech_valid$poutcome<-as.factor(ech_valid$poutcome)
ech_valid$housing<-as.factor(ech_valid$housing)
ech_valid$cl_balance2<-as.factor(ech_valid$cl_balance2)
ech_valid$marital<-as.factor(ech_valid$marital)
ech_valid$cl_campaign<-as.factor(ech_valid$cl_campaign)
ech_valid$loan<-as.factor(ech_valid$loan)
ech_valid$cl_day<-as.factor(ech_valid$cl_day)
ech_valid$cl_age2<-as.factor(ech_valid$cl_age2)
##### Comparaison sur l'echantillon test #####
regbi2<-glm(y ~ Cl_duration_2 +Cl_duration_3 + Cl_duration_4 + Cl_duration_5 +
             Poutcome_F + Poutcome_O +Poutcome_S + 
             cl_balance24__3000_+ cl_balance22_100_500_ + cl_balance23_500_3000_ + 
             maritaldivorced + maritalsingle +
             Loan_0 + Housing_0 +
             Cl_day_1 + Cl_day_3 +
             Cl_age_1 + Cl_age_3,data=ech_valid,family = binomial(logit))
summary(regbi2)
### Toutes les variables reste significative à plus de 95% ###
### Récupération de l'AUC, tpr, fpr###
proba_predite<-predict(regbi2, type="response")
P<-proba_predite
pred=prediction(P,ech_valid$y)
perf=performance(pred,"tpr", "fpr")
spec=performance(pred,"spec")
sens=performance(pred,"sens")
auc=performance(pred,"auc")
plot(perf,colorize = TRUE)
appren.p2 <- cbind(ech_valid, predict(regbi2, newdata = ech_valid, type = "link", 
                                     se = TRUE))
appren.p2 <- within(appren.p2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p2 <- cbind(appren.p2, pred.chd = factor(ifelse(appren.p2$PredictedProb > 
                                                       cutoff, 1, 0)))
m.confusion <- as.matrix(table(appren.p2$pred.chd, appren.p2$y))
m.confusion <- unclass(m.confusion)
# Taux d'erreur
Tx_err(appren.p$pred.chd, appren.p$y)

### Pas forcément pertinent de garder toutes les variables de la reglog ###
### Cependant, principe de la méthode est d'ajouté un maximum de variables et d'utiliser les composantes ##
### PLS avec toutes les variables suppression de multicolinarité avec régression sur les composantes orthogonales ###
xpls<-cbind(ech_test$Cl_duration_2 ,ech_test$Cl_duration_3 ,ech_test$Cl_duration_4 ,ech_test$Cl_duration_5,
            ech_test$Poutcome_F,ech_test$Poutcome_O, ech_test$Poutcome_S,
            ech_test$cl_balance24__3000_, ech_test$cl_balance22_100_500_ , ech_test$cl_balance23_500_3000_ , 
            ech_test$maritaldivorced, ech_test$maritalsingle,
            ech_test$Loan_0, ech_test$Housing_0,
            ech_test$Cl_day_1, ech_test$Cl_day_3,
            ech_test$Cl_age_1, ech_test$Cl_age_3)
ypls<-ech_test$Ynum
### Observation de notre modèle avec 10 composantes et les variables explicatives ###
res10<-plsRglm(ypls, xpls, nt=10, modele="pls-glm-logistic", pvals.expli=TRUE)
res.cv.modpls<-cvtable(summary(res10, MClassed = TRUE))
colSums(res10$pvalstep)
modpls2 <- plsRglm(dataY=ypls,dataX=xpls, nt = 10, modele = "pls-glm-logistic",sparse=TRUE,
                   sparseStop=TRUE)
set.seed(123)
### On cherche à créer 2 classes => les oui et les nons ###
### On réalise cela sur 8 composantes orthogonales et on répète cette action 100 fois afin de s'assurer de la validité de la classification #
cv.modpls.logit<-cv.plsRglm(dataY=ypls,dataX=xpls,nt=8,modele="pls-glm-logistic",K=2,NK=100)

res.cv.modpls.logit=cvtable(summary(cv.modpls.logit, MClassed = TRUE))
plot(res.cv.modpls.logit)
res_compl<-plsRglm(ypls, xpls, nt = 8, modele = "pls-glm-logistic",
                   pvals.expli=TRUE)
### Modèle final ###
res<-plsRglm(ypls, xpls, nt = 3, modele = "pls-glm-logistic", pvals.expli=TRUE)

res$wwetoile
biplot(res$tt,res$pp)

proba_predite<-predict(res, type="response")
P<-proba_predite
pred=prediction(P,ech_test$y)
perf=performance(pred,"tpr", "fpr")
spec=performance(pred,"spec")
sens=performance(pred,"sens")
auc=performance(pred,"auc")
plot(perf,colorize = TRUE)
# spécificité en valeur absolue
sensitivity <- sens@y.values[[1]]
specificity <- spec@y.values[[1]]
diffspecificity <- 1-specificity
diff <- abs(sensitivity-specificity)
tab_indicateurs <- data.frame(diff, diffspecificity,cutoff_optimal=spec@x.values[[1]], sensitivity, specificity)

## Recuperation du cutoff optiml pour ma variable prédite #
cutoff <- tab_indicateurs[tab_indicateurs$diff==min(tab_indicateurs$diff),"cutoff_optimal"]


plot(x = tab_indicateurs$cutoff_optimal, y = tab_indicateurs$diff,type='l', 
     main = "cutoff optimal", xlab = "Cutoff", ylab="sensitivity-specificity")
points(x=cutoff, y=0, pch=16)
# Diagramme du croisement de la sensibilité et de la spécificité #
plot(y = tab_indicateurs$sensitivity, x = tab_indicateurs$cutoff_optimal , type="l", col = "red", 
     main = "cutoff optimal", xlab = "cutoff", ylab = "")
lines(y=tab_indicateurs$specificity,x = tab_indicateurs$cutoff_optimal)

appren.p <- cbind(ech_test, P)
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$P > 
                                                       cutoff, 1, 0)))
m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$y))
m.confusion <- unclass(m.confusion)
# Taux d'erreur
Tx_err(appren.p$pred.chd, appren.p$y)

#### Comparaison sur l'échantillon test #####
xpls2<-cbind(ech_valid$Cl_duration_2 ,ech_valid$Cl_duration_3 ,ech_valid$Cl_duration_4 ,ech_valid$Cl_duration_5,
            ech_valid$Poutcome_F,ech_valid$Poutcome_O, ech_valid$Poutcome_S,
            ech_valid$cl_balance24__3000_, ech_valid$cl_balance22_100_500_ , ech_valid$cl_balance23_500_3000_ , 
            ech_valid$maritaldivorced, ech_valid$maritalsingle,
            ech_valid$Loan_0, ech_valid$Housing_0,
            ech_valid$Cl_day_1, ech_valid$Cl_day_3,
            ech_valid$Cl_age_1, ech_valid$Cl_age_3)
ypls2<-ech_valid$Ynum
res2<-plsRglm(ypls2, xpls2, nt = 3, modele = "pls-glm-logistic", pvals.expli=TRUE)

res2$wwetoile
biplot(res2$tt,res2$pp)

proba_predite<-predict(res2, type="response")
P<-proba_predite
pred=prediction(P,ech_valid$y)
perf=performance(pred,"tpr", "fpr")
spec=performance(pred,"spec")
sens=performance(pred,"sens")
auc=performance(pred,"auc")
plot(perf,colorize = TRUE)

appren.p2 <- cbind(ech_valid, P)
appren.p2 <- cbind(appren.p2, pred.chd = factor(ifelse(appren.p2$P > 
                                                       0.13, 1, 0)))
m.confusion <- as.matrix(table(appren.p2$pred.chd, appren.p2$y))
m.confusion <- unclass(m.confusion)
# Taux d'erreur
Tx_err(appren.p2$pred.chd, appren.p2$y)

################################### ANNEXE ACP SUR NOS DONNEES  ######################################
### ACP est un bon lien pur comprendre l'utilisation des axes ###
### ACP sur les variables binaires ech_test$y,###
#ac.active<-cbind(ech_test$Cl_duration_2 ,ech_test$Cl_duration_3 ,ech_test$Cl_duration_4 ,ech_test$Cl_duration_5,
 #                ech_test$Loan_0, ech_test$Housing_0,
  #               ech_test$Cl_day_1, ech_test$Cl_day_3)
#PCA(ac.active, scale.unit = TRUE, ncp = 5, graph = TRUE)
#res.pca <- PCA(ac.active, graph = FALSE)
#print(res.pca)
#eig.val <- get_eigenvalue(res.pca)
#eig.val
#fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 20))
#var <- get_pca_var(res.pca)
#head(var$cos2, 4)
# Coordonnées
#head(var$coord)
# Cos2: qualité de répresentation
#head(var$cos2)
# Contributions aux composantes principales
#head(var$contrib)
#fviz_pca_var(res.pca, col.var = "black")
#library("corrplot")
#corrplot(var$cos2, is.corr=FALSE)
# Cos2 total des variables sur Dim.1 et Dim.2
#fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Colorer en fonction du cos2: qualité de représentation
#fviz_pca_var(res.pca, col.var = "cos2",
#            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE # Évite le chevauchement de texte
#)
#fviz_pca_var(res.pca, alpha.var = "cos2")
#head(var$contrib, 4)
#corrplot(var$contrib, is.corr=FALSE)
# Contributions des variables à PC1
#fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
#fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
#fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
#fviz_pca_var(res.pca, col.var = "contrib",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
#)
# Créer une variable aléatoire continue de longueur 10
#set.seed (123)
#my.cont.var <- rnorm (31649)
# Colorer les variables en fonction de la variable continue
#fviz_pca_var(res.pca, col.var = my.cont.var,
#             gradient.cols = c("blue", "yellow", "red"),
#             legend.title = "Cont.Var")
#res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description de la dimension 1
#res.desc$Dim.1
#res.desc$Dim.2
#ind <- get_pca_ind(res.pca)
#ind
# Coordonnées des individus
#head(ind$coord)
# Qualité des individus
#head(ind$cos2)
# Contributions des individus
#head(ind$contrib)
#fviz_pca_ind (res.pca)
#fviz_pca_ind (res.pca, col.ind = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE # Évite le chevauchement de texte
#)
#############################################################################################
