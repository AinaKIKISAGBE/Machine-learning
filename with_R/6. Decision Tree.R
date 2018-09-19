#########################################################

#################### PROJET DELAUNAY #####################
rm(list=ls())
# librairies

library(dplyr)



####### PARTIE 1 : CONSTRUIRE LA REGRESSION LOGISTIQUE ######


###### 1. IMPORT DES DONNEES : APPRENTISSAGE ET TEST ###########

setwd('H:/SEGMENTATION')


train<-read.table("Base_TRAIN.csv",sep=",",header=T)
test<-read.table("Base_TEST.csv",sep=",",header=T)

summary(train)
summary(train$y)
summary(train$cible)
summary(train$Ynum)
summary(train$job)
summary(train$month)
# summary(df_appr)
freq<-table(train$y,train$y)
(prop.table(freq))
table(train$cl_age,train$cl_age2)
table(train$cl_balance,train$cl_balance2)
table(train$cl_day,train$cl_day2)

tableau<-table(train$y,train$cl_job)
round(prop.table(tableau,2),2)

#######################################################################

base_app1<-train[,c(2:25,32)]
base_app2<-train[,33:117]

base_test1<-test[,c(2:25,32)]

df_test<-base_test1[,-c(1,2,3,7,11,12,13,14,15,16)]
#######################################################################

#####  2. Data des variables qualitatives

df_appr<-base_app1[,-c(2,3,7,11,12,13,14,15,16)]

##########################################################


####3. Analyse CRAMER variables explicatives 2 a 2

#install.packages("vcd")
library(vcd)

catcorrm <- function(vars, dat) sapply(vars, function(y) sapply(vars, function(x) assocstats(table(dat[,x], dat[,y]))$cramer))

res<-names(df_appr) # on recupere les noms des variables

tab_cramer<-catcorrm(res,df_appr) # on cacul les correlations 2 a 2

tab_cramer_final<-as.data.frame(as.table(tab_cramer)) # resultat final

tab_sup<-tab_cramer_final%>% filter(Freq>=0.4 & Freq<1.0)


# suppression des variables cl_job cl_pday et cl previous
#####################################################################

df_apprfinal<-df_appr[,-c(3,14,15)]

# a retirer cl_pdays cl_previous et education

# Modele complet 
mod_complet <- glm(y ~ ., data = base_app1, family = binomial(logit))

summary(mod_complet)

##################### Selection du modele ######################

# ?step
##### reg2 <- step(mod_complet)

summary(reg2)



########### MODELE FINAL REGRESSION LOGISTIQUE ##################################################


mod_final<-glm(formula = y ~ cl_job + marital  + balance + housing  + cl_campaign + poutcome  + cl_duration , 
               family = binomial(logit), data = base_app1)


summary(mod_final)


summary(base_app1$campaign)

################################################################

#### evaluation du modele

###############################################################

#1. sur l'échantillon d'apprentissage

prev2<-round(predict(mod_final,newdata=base_app1,type="response"))

# matrice de confusion
table(base_app1$y,prev2)

# 0     1
# no  27477   469
# yes  2834   869

# erreur sur l'échantillon d'apprentissage

(2834+469)/(27477+2834+469+869)

#10,44%

############################################################Y
#2. sur l'echantillon test
prev1<-round(predict(mod_final,newdata=base_test1,type="response"))


# matrice de confusion
table(base_test1$y,prev1)

# 0     1
# no  11761   215
# yes  1246   340

# erreur sur l'échantillon test

(1246+215)/(11761+1246+215+340)

#10,77%

#######################################################

###### COURBE ROC DE LA REGRESSION LOGISTIQUE #######

#######################################################

install.packages("ROCR")
install.packages("Kendall")
library(ROCR)
library(Kendall)

S_logit<-predict(mod_final,newdata=base_test1,type="response")

S1_pred<-prediction(S_logit,base_test1$y)

roc1<-performance(S1_pred,measure = "tpr",x.measure = "fpr")

#?performance

str(roc1)
plot(roc1,col="red",lwd=2)
abline(0,1)

legend("bottomright",legend=c("logit"),col=c("red"),lty=1,lwd=2)

####### TRACAGE DE LA SENSITIVITE ET SENSIBILITE POUR TROUVER LE SEUIL S ######

S1_pred<-prediction(S_logit,base_test1$y)
perf1 <- performance(S1_pred, "sens")
perf2 <- performance(S1_pred, "spec")
plot(perf1,col="red")
plot(perf2,add=TRUE,col="blue")

legend("bottomright",legend=c("sensibilite", "specificite"),col=c("red","blue"),lty=1,lwd=2)

title("Sensibilite vs specificite")

abline(v=0.13)


# la valeur de s optimal est s=0.13

#### PERFORMANCE DU MODELE REGRESSION LOGISTIQUE
performance(S1_pred,"auc")@y.values[[1]]# calcul de l'AUC
#0.8619028

Kendall(S_logit,base_test1$y)



################################################################

p_glm=predict(mod_final,newdata=base_test1,type="response")
tab=table(base_test1$y,p_glm>.62)   # tab sort la matrice de confusion (faire varier le seuil 0.62 ici)
c
summary(p_glm>0.5)
summary(base_test1$y)

#################################################################

##### ANALYSE DES VARIABLES DICHOTOMIES ##########

############################################################

base_appdicho<-train[,c(33,35:100)]


#############################################################################
############################################################################
##############################################################################


exp(coef(mod_final))
exp(confint(mod_final))
exp(cbind(coef(mod_final), confint(mod_final)))


install.packages("questionr")
install.packages("devtools")
library(devtools)
devtools::install_github("larmarange/JLutils")


install.packages("ggplot2")
library(questionr)
library(JLutils)
library(ggplot2)



# install.packages("broom")
# install.packages("GGally")
# install.packages("plyr")
# install.packages("labelled")
library(broom)
library(GGally)
library(plyr)
library(dplyr)
library(tidyr)


odds.ratio(mod_final) # in terpretation des odds ratios


########### Graphique des odds ratios######################

td <- tidy_detailed(mod_final, exponentiate = TRUE, conf.int = TRUE)
td$label <- factor(td$label, rev(td$label))  # Pour fixer l'ordre pour ggplot2
ggcoef(td, mapping = aes(y = level_detail, x = estimate, 
                         colour = variable_label), exponentiate = TRUE)

######################################################################################################





##############################################
########### ARBRE DE DECISION ################
###########                   ################
##############################################

### PREPARATION DE L'ENVIRONEMENT DE TRAVAIL 

setwd('H:/SEGMENTATION')

### IMPORTATION DES BASES 
train <- read.csv("Base_TRAIN.csv", header=TRUE)
test <- read.csv("Base_TEST.csv", header=TRUE)


####### REDIFINITION DES TYPES DES VARIABLES  ####

#### base train ####
for (i in 1:ncol(train)) {train[,i]<-as.character(train[,i]) }
for (i in 1:ncol(train)) {train[,i]<-as.factor(train[,i]) }

train$age<-as.character(train$age)
train$balance<-as.character(train$balance)
train$day<-as.character(train$day)
train$duration<-as.character(train$duration)
train$campaign<-as.character(train$campaign)
train$pdays<-as.character(train$pdays)
train$previous<-as.character(train$previous)

train$age<-as.numeric(train$age)
train$balance<-as.numeric(train$balance)
train$day<-as.numeric(train$day)
train$duration<-as.numeric(train$duration)
train$campaign<-as.numeric(train$campaign)
train$pdays<-as.numeric(train$pdays)
train$previous<-as.numeric(train$previous)

#### base test ####
for (i in 1:ncol(test)) {test[,i]<-as.character(test[,i]) }
for (i in 1:ncol(test)) {test[,i]<-as.factor(test[,i]) }

test$age<-as.character(test$age)
test$balance<-as.character(test$balance)
test$day<-as.character(test$day)
test$duration<-as.character(test$duration)
test$campaign<-as.character(test$campaign)
test$pdays<-as.character(test$pdays)
test$previous<-as.character(test$previous)

test$age<-as.numeric(test$age)
test$balance<-as.numeric(test$balance)
test$day<-as.numeric(test$day)
test$duration<-as.numeric(test$duration)
test$campaign<-as.numeric(test$campaign)
test$pdays<-as.numeric(test$pdays)
test$previous<-as.numeric(test$previous)

########## CREATION DE 5 BASES DE TRAVAIL SELON DES COMBINAISON DE TYPE DES VARIABLES EXPLICATIVES ######

N1<-matrix(c("y",	"age",	"balance",	"day",	"duration",	"campaign",	"pdays",	"previous",	"job",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"month",	"poutcome"))
nrow(N1)
train1<-train[,c("y",	"age",	"balance",	"day",	"duration",	"campaign",	"pdays",	"previous",	"job",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"month",	"poutcome")]
test1<-test[,c("y",	"age",	"balance",	"day",	"duration",	"campaign",	"pdays",	"previous",	"job",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"month",	"poutcome")]

N2<-matrix(c("y",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"poutcome",	"cl_age",	"cl_balance",	"cl_day",	"cl_duration",	"cl_campaign",	"cl_pdays",	"cl_previous",	"cl_month",	"cl_job"))
train2<-train[,c("y",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"poutcome",	"cl_age",	"cl_balance",	"cl_day",	"cl_duration",	"cl_campaign",	"cl_pdays",	"cl_previous",	"cl_month",	"cl_job")]
test2<-test[,c("y",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"poutcome",	"cl_age",	"cl_balance",	"cl_day",	"cl_duration",	"cl_campaign",	"cl_pdays",	"cl_previous",	"cl_month",	"cl_job")]

N3<-matrix(c("y",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"poutcome",	"cl_duration",	"cl_campaign",	"cl_pdays",	"cl_previous",	"cl_month",	"cl_balance2",	"cl_day2",	"cl_age2",	"cl_job"))
train3<-train[,c("y",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"poutcome",	"cl_duration",	"cl_campaign",	"cl_pdays",	"cl_previous",	"cl_month",	"cl_balance2",	"cl_day2",	"cl_age2",	"cl_job")]
test3<-test[,c("y",	"marital",	"education",	"default",	"housing",	"loan",	"contact",	"poutcome",	"cl_duration",	"cl_campaign",	"cl_pdays",	"cl_previous",	"cl_month",	"cl_balance2",	"cl_day2",	"cl_age2",	"cl_job")]

N4<-matrix(c("y",	"education",	"cl_pdays",	"cl_previous",	"Marital_D",	"Marital_M",	"Marital_S",	"Default_0",	"Default_1",	"Housing_0",	"Housing_1",	"Loan_0",	"Loan_1",	"Contact_C",	"Contact_T",	"Contact_U",	"Poutcome_F",	"Poutcome_O",	"Poutcome_S",	"Poutcome_U",	"Cl_age_1",	"Cl_age_2",	"Cl_age_3",	"Cl_balance_1",	"Cl_balance_2",	"Cl_balance_3",	"Cl_balance_4",	"Cl_day_1",	"Cl_day_2",	"Cl_day_3",	"Cl_duration_1",	"Cl_duration_2",	"Cl_duration_3",	"Cl_duration_4",	"Cl_duration_5",	"Cl_campaign_1",	"Cl_campaign_2",	"Cl_campaign_3",	"Cl_campaign_4",	"Cl_month_1",	"Cl_month_2",	"Cl_month_3",	"Cl_month_4",	"Cl_job_1",	"Cl_job_2",	"Cl_job_3",	"Cl_job_4",	"Cl_job_5"))
train4<-train[,c("y",	"education",	"cl_pdays",	"cl_previous",	"Marital_D",	"Marital_M",	"Marital_S",	"Default_0",	"Default_1",	"Housing_0",	"Housing_1",	"Loan_0",	"Loan_1",	"Contact_C",	"Contact_T",	"Contact_U",	"Poutcome_F",	"Poutcome_O",	"Poutcome_S",	"Poutcome_U",	"Cl_age_1",	"Cl_age_2",	"Cl_age_3",	"Cl_balance_1",	"Cl_balance_2",	"Cl_balance_3",	"Cl_balance_4",	"Cl_day_1",	"Cl_day_2",	"Cl_day_3",	"Cl_duration_1",	"Cl_duration_2",	"Cl_duration_3",	"Cl_duration_4",	"Cl_duration_5",	"Cl_campaign_1",	"Cl_campaign_2",	"Cl_campaign_3",	"Cl_campaign_4",	"Cl_month_1",	"Cl_month_2",	"Cl_month_3",	"Cl_month_4",	"Cl_job_1",	"Cl_job_2",	"Cl_job_3",	"Cl_job_4",	"Cl_job_5")]
test4<-test[,c("y",	"education",	"cl_pdays",	"cl_previous",	"Marital_D",	"Marital_M",	"Marital_S",	"Default_0",	"Default_1",	"Housing_0",	"Housing_1",	"Loan_0",	"Loan_1",	"Contact_C",	"Contact_T",	"Contact_U",	"Poutcome_F",	"Poutcome_O",	"Poutcome_S",	"Poutcome_U",	"Cl_age_1",	"Cl_age_2",	"Cl_age_3",	"Cl_balance_1",	"Cl_balance_2",	"Cl_balance_3",	"Cl_balance_4",	"Cl_day_1",	"Cl_day_2",	"Cl_day_3",	"Cl_duration_1",	"Cl_duration_2",	"Cl_duration_3",	"Cl_duration_4",	"Cl_duration_5",	"Cl_campaign_1",	"Cl_campaign_2",	"Cl_campaign_3",	"Cl_campaign_4",	"Cl_month_1",	"Cl_month_2",	"Cl_month_3",	"Cl_month_4",	"Cl_job_1",	"Cl_job_2",	"Cl_job_3",	"Cl_job_4",	"Cl_job_5")]

N5<-matrix(c("y",	"education",	"cl_pdays",	"cl_previous",	"Cl_job_1",	"Cl_job_2",	"Cl_job_3",	"Cl_job_4",	"Cl_job_5",	"maritaldivorced",	"maritalmarried",	"maritalsingle",	"defaultno",	"defaultyes",	"housingno",	"housingyes",	"loanno",	"loanyes",	"contactcellular",	"contacttelephone",	"contactunknown",	"poutcomefailure",	"poutcomeother",	"poutcomesuccess",	"poutcomeunknown",	"cl_age21___35_ans",	"cl_age22_35_55_ans",	"cl_age23___55_ans",	"cl_balance21__100_",	"cl_balance22_100_500_",	"cl_balance23_500_3000_",	"cl_balance24__3000_",	"cl_day21___14_jours",	"cl_day22__15_21_jours",	"cl_day23____21_jours",	"cl_duration1____60s",	"cl_duration2__60_120s",	"cl_duration3__120_180s",	"cl_duration3__180_360s",	"cl_duration4____360s",	"cl_campaign1__1_campagne",	"cl_campaign2__2_campagnes",	"cl_campaign3__3_4_campagne",	"cl_campaign4___4_campagnes",	"cl_month1_Trim1",	"cl_month1_Trim2",	"cl_month1_Trim3",	"cl_month1_Trim4"))
train5<-train[,c("y",	"education",	"cl_pdays",	"cl_previous",	"Cl_job_1",	"Cl_job_2",	"Cl_job_3",	"Cl_job_4",	"Cl_job_5",	"maritaldivorced",	"maritalmarried",	"maritalsingle",	"defaultno",	"defaultyes",	"housingno",	"housingyes",	"loanno",	"loanyes",	"contactcellular",	"contacttelephone",	"contactunknown",	"poutcomefailure",	"poutcomeother",	"poutcomesuccess",	"poutcomeunknown",	"cl_age21___35_ans",	"cl_age22_35_55_ans",	"cl_age23___55_ans",	"cl_balance21__100_",	"cl_balance22_100_500_",	"cl_balance23_500_3000_",	"cl_balance24__3000_",	"cl_day21___14_jours",	"cl_day22__15_21_jours",	"cl_day23____21_jours",	"cl_duration1____60s",	"cl_duration2__60_120s",	"cl_duration3__120_180s",	"cl_duration3__180_360s",	"cl_duration4____360s",	"cl_campaign1__1_campagne",	"cl_campaign2__2_campagnes",	"cl_campaign3__3_4_campagne",	"cl_campaign4___4_campagnes",	"cl_month1_Trim1",	"cl_month1_Trim2",	"cl_month1_Trim3",	"cl_month1_Trim4")]
test5<-test[,c("y",	"education",	"cl_pdays",	"cl_previous",	"Cl_job_1",	"Cl_job_2",	"Cl_job_3",	"Cl_job_4",	"Cl_job_5",	"maritaldivorced",	"maritalmarried",	"maritalsingle",	"defaultno",	"defaultyes",	"housingno",	"housingyes",	"loanno",	"loanyes",	"contactcellular",	"contacttelephone",	"contactunknown",	"poutcomefailure",	"poutcomeother",	"poutcomesuccess",	"poutcomeunknown",	"cl_age21___35_ans",	"cl_age22_35_55_ans",	"cl_age23___55_ans",	"cl_balance21__100_",	"cl_balance22_100_500_",	"cl_balance23_500_3000_",	"cl_balance24__3000_",	"cl_day21___14_jours",	"cl_day22__15_21_jours",	"cl_day23____21_jours",	"cl_duration1____60s",	"cl_duration2__60_120s",	"cl_duration3__120_180s",	"cl_duration3__180_360s",	"cl_duration4____360s",	"cl_campaign1__1_campagne",	"cl_campaign2__2_campagnes",	"cl_campaign3__3_4_campagne",	"cl_campaign4___4_campagnes",	"cl_month1_Trim1",	"cl_month1_Trim2",	"cl_month1_Trim3",	"cl_month1_Trim4")]



######### ARBRE DE DECISION "CART" #########

##### BASE "train1" et "test1" #####

#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

cart_1 <- rpart(y ~ .,data = train1,method="class",parms=list(split="gini"),cp=0)
printcp(cart_1)
min(cart_1$cptable[,5]) # min (xstd)=0.01406654 avec cp=0.001350257 et n=49 

##### donc on choisie dans la suite : cp=0.001350257
cart <- rpart(y ~ .,data = train1,method="class",parms=list(split="gini"),cp=0.001350257,control=list(minbucket=25,minsplit=49,maxdepth=5))
plot(cart,branch=.2, uniform=T, compress=T, margin=.1) # trac? de l'arbre

#plot(cart)
text(cart, fancy=F,use.n=T,pretty=0,all=T, cex=.5) # ajout des l?gendes des noeuds
print(cart)
printcp(cart)
summary(cart,digits=3) # plus d'informations sur les scissions

# affichage am?lior? avec package rpart.plot
prp(cart,type=2,extra=1,split.box.col="lightgray")

prp(cart,type=2,extra=1,split.box.col="lightgray",cex=.5)

# on peut aussi afficher cela dans un graphique
# montront la d?croissance erreur relative en fonction du coefficient de complexit?
plotcp(cart) # calcul par validation crois?e
summary(cart,digits=3)

# le graph nous indique n=6 et cp=0.0093
# mais on va aussi tester pour n=12 et cp=0.0037
# et n=14 et cp = 0.002

# ?laguage
prunedcart6f = prune(cart,cp=0.0093)
prunedcart12f = prune(cart,cp=0.0037)
prunedcart14f = prune(cart,cp=0.002)

# choix valeur coefficient de p?nalisation de la complexit? pour ?lagage
plot(prunedcart6f,branch=.2, uniform=T, compress=T, margin=.1)
text(prunedcart6f, fancy=F,use.n=T,pretty=0,all=T,cex=.5)
# affichage avec package rpart.plot
prp(prunedcart6f,type=2,extra=1,split.box.col="lightgray")

# ?laguage automatique au minimum d'erreur + 1 ?cart-type
xerr <- cart$cptable[,"xerror"]
xerr
minxerr <- which.min(xerr)
seuilerr <- cart$cptable[minxerr, "xerror"]+cart$cptable[minxerr, "xstd"]
xerr [xerr < seuilerr][1]
mincp <- cart$cptable[names(xerr [xerr < seuilerr][1]), "CP"]
prunedcart <- prune(cart,cp=mincp)
names(xerr [xerr < seuilerr][1])

# ?laguage automatique au minimum d'erreur
xerr <- cart$cptable[,"xerror"]
minxerr <- which.min(xerr)
mincp <- cart$cptable[minxerr, "CP"]
prunedcart <- prune(cart,cp=mincp)
# code plus compact :
prunedcart <- prune(cart, cp=cart$cptable[which.min(cart$cptable[,"xerror"]),"CP"])
# affichage de l'arbre ?lagu?
plot(prunedcart,branch=.2, uniform=T, compress=T, margin=.1)
text(prunedcart, fancy=F,use.n=T,pretty=0,all=T,cex=.5)
prp(prunedcart,type=2,extra=1,split.box.col="lightgray")

# ?lagage au nombre minimum de feuilles
mincart <- prune(cart,cp=cart$cptable[min(2,nrow(cart$cptable)), "CP"])
plot(mincart,branch=.2, uniform=T, compress=T, margin=.1)
text(mincart, fancy=F,use.n=T,pretty=0,all=T,cex=.5)
prp(mincart,type=2,extra=1,split.box.col="lightgray")

# for stumps : rpart.control(maxdepth=1,cp=-1,minsplit=0)
cart <- rpart(y ~ . ,data = train1,method="class",parms=list(split="gini"),control=list(maxdepth=1,cp=-1,minsplit=0))
plot(cart)
plot(cart,branch=.2, uniform=T, compress=T, margin=.1)
text(cart, fancy=F,use.n=T,pretty=0,all=T,cex=.5)
prp(cart,type=2,extra=1,split.box.col="lightgray")

# mesure des performances sur l'?chantillon de test
test1$CART6f <- predict(prunedcart6f,type="prob",test1)
test1$CART12f <- predict(prunedcart12f,type="prob",test1)
test1$CART14f <- predict(prunedcart14f,type="prob",test1)
head(test1["CART6f"],5)

# aire sous la courbe ROC
library(ROCR)
pred <- prediction(test1$CART6f[,2],test1$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
performance(pred,"auc")@y.values[[1]]
auc
# ?quivalent : attr(performance(pred,"auc"),'y.values')[[1]]
# r?cup?re l'attribut "y.values" de l'objet "performance"
perf <- performance(pred,"tpr","fpr")
plot(perf,main='Courbe ROC')
segments(0,0,1,1,col='grey',lty=3) # ajout diagonale en pointill?s gris
# courbe de lift
perf <- performance(pred,"lift","rpp")
plot(perf,main='Courbe de lift')
# courbe de lift usuelle
pred <- prediction(test1$CART6f[,2],test1$y,label.ordering=c("no","yes"))
lift <- performance(pred,"tpr","rpp")
plot(lift,main='Courbe de lift')
summary(train1$y)
# comparaison de courbes ROC
pred6f <- prediction(test1$CART6f[,2],test1$y,label.ordering=c("no","yes"))
pred12f <- prediction(test1$CART12f[,2],test1$y,label.ordering=c("no","yes"))
pred14f <- prediction(test1$CART14f[,2],test1$y,label.ordering=c("no","yes"))

plot(performance(pred6f,"tpr","fpr"),col='red',lty=2,main='AUC de mod?les CART')
plot(performance(pred12f,"tpr","fpr"), col='black',add=TRUE,lty=1)
plot(performance(pred14f,"tpr","fpr"), col='blue',add=TRUE,lty=1)

segments(0,0,1,1,lty=3)

legend("bottomright",c('6 feuilles','12 feuilles','14 feuilles'),col=c('red','black','blue'),
       lty=c(2,1),lwd=3)

# comparaison de courbes ROC avec intervalles de confiance
library(pROC)
roc <- plot.roc(test1$y,test1$CART6f[,2],col='black',lty=1,ci=TRUE)
plot.roc(test1$y,test1$CART12f[,2],add=TRUE,col='red',lty=2,ci=TRUE)
legend("bottomright",c('12 feuilles','6 feuilles'),col=c('red','black'),lty=c(2,1),lwd=3)


# erreur TEST n=6
test1$cart6 <- predict(prunedcart6f,type="class",test1)
err_cart <- sum(test1$cart != test1$y) / nrow(test1)
err_cart # 0.10146 pour CART ? 6 feuilles

# erreur TEST n=12
test1$cart12 <- predict(prunedcart12f,type="class",test1)
err_cart <- sum(test1$cart != test1$y) / nrow(test1)
err_cart # 0.09784693 pour CART ? 12 feuilles

# erreur TRAIN n=6
train1$cart6 <- predict(prunedcart6f,type="class",train1)
err_cart <- sum(train1$cart != train1$y) / nrow(train1)
err_cart # 0.09817056 pour CART ? 6 feuilles

# erreur TRAIN n=12
train1$cart12 <- predict(prunedcart12f,type="class",train1)
err_cart <- sum(train1$cart != train1$y) / nrow(train1)
err_cart # 0.09469494 pour CART ? 12 feuilles


table_train12<-table(train1$cart12 ,train1$y)

table_test12<-table(test1$cart12,test1$y)



M<-matrix(NA,1,7)
M[1,1] <- "err_cart_train"
M[1,2] <- "err_cart_test"
M[1,3] <- "auc_val"
M[1,4] <- "minbucket"
M[1,5] <- "minsplit"
M[1,6] <- "cp"
M[1,7] <- "base"

M1<-M

list_i<-seq(15,500,by=15)
for (i in list_i){
  j<-2*i

cart_boucle <- rpart(y ~ .,data = train1[,1:nrow(N1)],method="class",parms=list(split="gini"),cp=0.0012152,control=list(minbucket=i,minsplit=j))
list_cp<-c(0.0012152)
for (k in list_cp) {
prunedcartf = prune(cart_boucle,cp=k)
test1$CARTf <- predict(prunedcartf,type="prob",test1)

# aprraent
train1$cart <- predict(prunedcartf,type="class",data=train1)
err_cart_train <- sum(train1$cart != train1$y) / nrow(train1)
err_cart_train 

# test
test1$cart <- predict(prunedcartf,type="class",test1)
err_cart_test <- sum(test1$cart != test1$y) / nrow(test1)
err_cart_test

pred <- prediction(test1$CARTf[,2],test1$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]

M2<-matrix(c(err_cart_train,err_cart_test,auc_val,i,j,k,"base1"),1,7)

M3<-rbind(M1,M2)
M1<-M3
M3

  }
}


vd1<-as.data.frame(M3)

View(vd1)

## en choisissant le max de l'AUC ou le min de "err_cart_train" 
# ou du min de "err_cart_test" on peut retenir 15 et 30 si on veut plus de pr?cision et on aura 
#   err_cart_train     err_cart_test             auc_val   
# 0.0826566400202218   0.0950449786167232    0.856802718509739

# ou si on veut choisir les meilleurs valeurs ?lev? de minbucket et de minsplit, 
# on pourra choisir 225 et 450
#   err_cart_train     err_cart_test             auc_val     minbucket   minsplit    cp
# 0.0947265316439698   0.0984368087302758   0.851031613458106     225         450    0.0012152



# application de la meme methode aux autres bases 2,3,4 et 5 (on ne l'appliquera pas ici)

### base 2
cart_1 <- rpart(y ~ .,data = train2,method="class",parms=list(split="gini"),cp=0)
printcp(cart_1)
min_cp<-min(cart_1$cptable[,5])
M<-matrix(NA,1,7)
M[1,1] <- "err_cart_train"
M[1,2] <- "err_cart_test"
M[1,3] <- "auc_val"
M[1,4] <- "minbucket"
M[1,5] <- "minsplit"
M[1,6] <- "cp"
M[1,7] <- "base"


M1<-M

list_i<-seq(15,500,by=15)
for (i in list_i){
  j<-2*i
  
  cart_boucle <- rpart(y ~ .,data = train2[,1:nrow(N2)],method="class",parms=list(split="gini"),cp=min_cp,control=list(minbucket=i,minsplit=j))
  list_cp<-c(min_cp)
  for (k in list_cp) {
    prunedcartf = prune(cart_boucle,cp=k)
    test2$CARTf <- predict(prunedcartf,type="prob",test2)
    
    # aprraent
    train2$cart <- predict(prunedcartf,type="class",data=train2)
    err_cart_train <- sum(train2$cart != train2$y) / nrow(train2)
    err_cart_train 
    
    # test
    test2$cart <- predict(prunedcartf,type="class",test2)
    err_cart_test <- sum(test2$cart != test2$y) / nrow(test2)
    err_cart_test
    
    pred <- prediction(test2$CARTf[,2],test2$y,label.ordering=c("no","yes"))
    auc <- performance(pred,"auc")
    auc_val<-performance(pred,"auc")@y.values[[1]]
    
    M2<-matrix(c(err_cart_train,err_cart_test,auc_val,i,j,k,"base2"),1,7)
    
    M3<-rbind(M1,M2)
    M1<-M3
    M3
    
  }
}


vd2<-as.data.frame(M3)

View(vd2)

### base 3
cart_1 <- rpart(y ~ .,data = train3,method="class",parms=list(split="gini"),cp=0)
printcp(cart_1)
min_cp<-min(cart_1$cptable[,5])
M<-matrix(NA,1,7)
M[1,1] <- "err_cart_train"
M[1,2] <- "err_cart_test"
M[1,3] <- "auc_val"
M[1,4] <- "minbucket"
M[1,5] <- "minsplit"
M[1,6] <- "cp"
M[1,7] <- "base"


M1<-M

list_i<-seq(15,500,by=15)
for (i in list_i){
  j<-2*i
  
  cart_boucle <- rpart(y ~ .,data = train3[,1:nrow(N3)],method="class",parms=list(split="gini"),cp=min_cp,control=list(minbucket=i,minsplit=j))
  list_cp<-c(min_cp)
  for (k in list_cp) {
    prunedcartf = prune(cart_boucle,cp=k)
    test3$CARTf <- predict(prunedcartf,type="prob",test3)
    
    # aprraent
    train3$cart <- predict(prunedcartf,type="class",data=train3)
    err_cart_train <- sum(train3$cart != train3$y) / nrow(train3)
    err_cart_train 
    
    # test
    test3$cart <- predict(prunedcartf,type="class",test3)
    err_cart_test <- sum(test3$cart != test3$y) / nrow(test3)
    err_cart_test
    
    pred <- prediction(test3$CARTf[,2],test3$y,label.ordering=c("no","yes"))
    auc <- performance(pred,"auc")
    auc_val<-performance(pred,"auc")@y.values[[1]]
    
    M2<-matrix(c(err_cart_train,err_cart_test,auc_val,i,j,k,"base3"),1,7)
    
    M3<-rbind(M1,M2)
    M1<-M3
    M3
    
  }
}


vd3<-as.data.frame(M3)

View(vd3)

### base 4
cart_1 <- rpart(y ~ .,data = train4,method="class",parms=list(split="gini"),cp=0)
printcp(cart_1)
min_cp<-min(cart_1$cptable[,5])
M<-matrix(NA,1,7)
M[1,1] <- "err_cart_train"
M[1,2] <- "err_cart_test"
M[1,3] <- "auc_val"
M[1,4] <- "minbucket"
M[1,5] <- "minsplit"
M[1,6] <- "cp"
M[1,7] <- "base"


M1<-M

list_i<-seq(15,500,by=15)
for (i in list_i){
  j<-2*i
  
  cart_boucle <- rpart(y ~ .,data = train4[,1:nrow(N4)],method="class",parms=list(split="gini"),cp=min_cp,control=list(minbucket=i,minsplit=j))
  list_cp<-c(min_cp)
  for (k in list_cp) {
    prunedcartf = prune(cart_boucle,cp=k)
    test4$CARTf <- predict(prunedcartf,type="prob",test4)
    
    # aprraent
    train4$cart <- predict(prunedcartf,type="class",data=train4)
    err_cart_train <- sum(train4$cart != train4$y) / nrow(train4)
    err_cart_train 
    
    # test
    test4$cart <- predict(prunedcartf,type="class",test4)
    err_cart_test <- sum(test4$cart != test4$y) / nrow(test4)
    err_cart_test
    
    pred <- prediction(test4$CARTf[,2],test4$y,label.ordering=c("no","yes"))
    auc <- performance(pred,"auc")
    auc_val<-performance(pred,"auc")@y.values[[1]]
    
    M2<-matrix(c(err_cart_train,err_cart_test,auc_val,i,j,k,"base4"),1,7)
    
    M3<-rbind(M1,M2)
    M1<-M3
    M3
    
  }
}


vd4<-as.data.frame(M3)

View(vd4)

### base 5
cart_1 <- rpart(y ~ .,data = train5,method="class",parms=list(split="gini"),cp=0)
printcp(cart_1)
min_cp<-min(cart_1$cptable[,5])
M<-matrix(NA,1,7)
M[1,1] <- "err_cart_train"
M[1,2] <- "err_cart_test"
M[1,3] <- "auc_val"
M[1,4] <- "minbucket"
M[1,5] <- "minsplit"
M[1,6] <- "cp"
M[1,7] <- "base"


M1<-M

list_i<-seq(15,500,by=15)
for (i in list_i){
  j<-2*i
  
  cart_boucle <- rpart(y ~ .,data = train5[,1:nrow(N5)],method="class",parms=list(split="gini"),cp=min_cp,control=list(minbucket=i,minsplit=j))
  list_cp<-c(min_cp)
  for (k in list_cp) {
    prunedcartf = prune(cart_boucle,cp=k)
    test5$CARTf <- predict(prunedcartf,type="prob",test5)
    
    # aprraent
    train5$cart <- predict(prunedcartf,type="class",data=train5)
    err_cart_train <- sum(train5$cart != train5$y) / nrow(train5)
    err_cart_train 
    
    # test
    test5$cart <- predict(prunedcartf,type="class",test5)
    err_cart_test <- sum(test5$cart != test5$y) / nrow(test5)
    err_cart_test
    
    pred <- prediction(test5$CARTf[,2],test5$y,label.ordering=c("no","yes"))
    auc <- performance(pred,"auc")
    auc_val<-performance(pred,"auc")@y.values[[1]]
    
    M2<-matrix(c(err_cart_train,err_cart_test,auc_val,i,j,k,"base5"),1,7)
    
    M3<-rbind(M1,M2)
    M1<-M3
    M3
    
  }
}


vd5<-as.data.frame(M3)

View(vd5)

################# RECAPITULATIF #########

vd1_mat<-as.matrix(vd1)
vd2_mat<-as.matrix(vd2)
vd3_mat<-as.matrix(vd3)
vd4_mat<-as.matrix(vd4)
vd5_mat<-as.matrix(vd5)

v_all_mat<-rbind(vd1_mat,vd2_mat,vd3_mat,vd4_mat,vd5_mat)
v_all<-as.data.frame(v_all_mat)



#### model final d'arbre de d?cision #######

# puis apr?s v?rification du max (AUC) ou de la minimisation des erreurs, 
# on retient la base1 ainsi que ses donn?e issu de l'annalyse de la base1 plus haut'



##### donc on choisie dans la suite : cp=0.0012152
cart <- rpart(y ~ .,data = train1[,1:nrow(N1)],method="class",parms=list(split="gini"),cp=0.0012152,control=list(minbucket=15,minsplit=30,maxdepth=5))
plot(cart,branch=.2, uniform=T, compress=T, margin=.1) # trac? de l'arbre
text(cart, fancy=F,use.n=T,pretty=0,all=T, cex=.5) # ajout des l?gendes des noeuds
cart
# affichage am?lior? avec package rpart.plot
prp(cart,type=2,extra=1,split.box.col="lightgray",cex=.5)

plotcp(cart) # calcul par validation crois?e

# aire sous la courbe ROC
library(ROCR)

cart_boucle <- rpart(y ~ .,data = train1[,1:nrow(N1)],method="class",parms=list(split="gini"),cp=0.0012152,control=list(minbucket=15,minsplit=30))
cart<-cart_boucle 

prunedcartf = prune(cart_boucle,cp=0.0012152)
test1$CARTf <- predict(prunedcartf,type="prob",test1)

# aprraent
train1$cart <- predict(prunedcartf,type="class",data=train1)
err_cart_train <- sum(train1$cart != train1$y) / nrow(train1)
err_cart_train 

table_train<-table(train1$cart ,train1$y)


# test
test1$cart <- predict(prunedcartf,type="class",test1)
err_cart_test <- sum(test1$cart != test1$y) / nrow(test1)
err_cart_test

table_test<-table(test1$cart,test1$y)


pred <- prediction(test1$CARTf[,2],test1$y,label.ordering=c("no","yes"))
pred_choisi_cart <- prediction(test1$CARTf[,2],test1$y,label.ordering=c("no","yes"))

auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
plot(performance(pred_choisi_cart,"tpr","fpr"),col='red',lty=2,main='AUC de mod?les CART')


##########################################

pred <- prediction(test1$CARTf[,2],test1$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
performance(pred,"auc")@y.values[[1]]
auc
# ?quivalent : attr(performance(pred,"auc"),'y.values')[[1]]
# r?cup?re l'attribut "y.values" de l'objet "performance"
perf <- performance(pred,"tpr","fpr")
plot(perf,main='Courbe ROC')
segments(0,0,1,1,col='grey',lty=3) # ajout diagonale en pointill?s gris
# courbe de lift
perf <- performance(pred,"lift","rpp")
plot(perf,main='Courbe de lift')
# courbe de lift usuelle
pred <- prediction(test1$CART6f[,2],test1$y,label.ordering=c("no","yes"))
lift <- performance(pred,"tpr","rpp")
plot(lift,main='Courbe de lift')
segments(0,0,1,1,col='grey',lty=3) # ajout diagonale en pointill?s gris

#  courbes ROC 
library(pROC)
roc <- plot.roc(test1$y,test1$CARTf[,2],col='black',lty=1,ci=TRUE)



# ---------------------------------------------------------------------------------------------------------
# Arbre de d?cision CHAID
# ---------------------------------------------------------------------------------------------------------

# install.packages("partykit")
#install.packages("CHAID", repos="http://R-Forge.R-project.org")

library("CHAID")

# LE CHAID s'applique uniquement aux variable qualitatives 
# donc est applicable pour les bases 2,3,4 et 5

### base 2 #####
str(train2)
# param?tres par d?faut
chaid <- chaid(y ~ . ,data = train2[,1:nrow(N2)])


test2$CARTf2 <- predict(chaid,type="prob",test2)

# aprraent
train2$cart <- predict(chaid,data=train2)
err_cart_train <- sum(train2$cart != train2$y) / nrow(train2)
err_cart_train  # 0.09880249

table_train<-table(train2$cart ,train2$y)

# no   yes
# no  27531  2712
# yes   415   991


# test
test2$cart <- predict(chaid,test2)
err_cart_test <- sum(test2$cart != test2$y) / nrow(test2)
err_cart_test # 0.1046306

table_test<-table(test2$cart,test2$y)

# no   yes
# no  11755  1198
# yes   221   388


pred <- prediction(test2$CARTf2[,2],test2$y,label.ordering=c("no","yes"))
pred_choisi_chaid <- prediction(test2$CARTf2[,2],test2$y,label.ordering=c("no","yes"))

auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val # 0.8883741
plot(performance(pred_choisi_chaid,"tpr","fpr"),col='red',lty=2,main='AUC de mod?les CART')


### base 3 #####
str(train3)
# param?tres par d?faut
chaid <- chaid(y ~ . ,data = train3[,1:nrow(N3)])


test3$CARTf3 <- predict(chaid,type="prob",test3)

# aprraent
train3$cart <- predict(chaid,data=train3)
err_cart_train <- sum(train3$cart != train3$y) / nrow(train3)
err_cart_train #0.0995924

table_train<-table(train3$cart ,train3$y)

# no   yes
# no  27548  2754
# yes   398   949

# test
test3$cart <- predict(chaid,test3)
err_cart_test <- sum(test3$cart != test3$y) / nrow(test3)
err_cart_test # 0.1055154

table_test<-table(test3$cart,test3$y)

# no   yes
# no  11761  1216
# yes   215   370


pred <- prediction(test3$CARTf3[,2],test3$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val # 0.8844384

### base 4 #####
str(train4)
# param?tres par d?faut
chaid <- chaid(y ~ . ,data = train4[,1:nrow(N4)])


test4$CARTf4 <- predict(chaid,type="prob",test4)

# aprraent
train4$cart <- predict(chaid,data=train4)
err_cart_train <- sum(train4$cart != train4$y) / nrow(train4)
err_cart_train #0.09636955

table_train<-table(train4$cart ,train4$y)


# no   yes
# no  27485  2589
# yes   461  1114


# test
test4$cart <- predict(chaid,test4)
err_cart_test <- sum(test4$cart != test4$y) / nrow(test4)
err_cart_test #0.106179

table_test<-table(test4$cart,test4$y)

# no   yes
# no  11708  1172
# yes   268   414


pred <- prediction(test4$CARTf4[,2],test4$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val# 0.8854434


### base 5 #####
str(train5)
# param?tres par d?faut
chaid <- chaid(y ~ . ,data = train5[,1:nrow(N5)])


test5$CARTf5 <- predict(chaid,type="prob",test5)

# aprraent
train5$cart <- predict(chaid,data=train5)
err_cart_train <- sum(train5$cart != train5$y) / nrow(train5)
err_cart_train #0.09614838

table_train<-table(train5$cart ,train5$y)

# 
# no   yes
# no  27496  2593
# yes   450  1110


# test
test5$cart <- predict(chaid,test5)
err_cart_test <- sum(test5$cart != test5$y) / nrow(test5)
err_cart_test # 0.1061053

table_test<-table(test5$cart,test5$y)

# no   yes
# no  11710  1173
# yes   266   413


pred <- prediction(test5$CARTf5[,2],test2$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val# 0.8854314


# ---------------------------------------------------------------------------------------------------------
# Arbre C5.0
# ---------------------------------------------------------------------------------------------------------

# arbre de d?cision C5.0
#install.packages("C50")
library(C50)

#### base 1 #####

c50 <- C5.0(y ~ . ,data = train1[,1:nrow(N1)],rules=T)

test1$CARTf1 <- predict(c50,type="prob",test1)

# aprraent
train1$cart <- predict(c50,type="class",train1)
err_cart_train <- sum(train1$cart != train1$y) / nrow(train1)
err_cart_train  # 0.07428355

table_train<-table(train1$cart ,train1$y)

# no   yes
# no  27332  1737
# yes   614  1966


# test
test1$cart <- predict(c50,type="class",test1)
err_cart_test <- sum(test1$cart != test1$y) / nrow(test1)
err_cart_test # 0.09666716

table_test<-table(test1$cart,test1$y)
# 
# no   yes
# no  11554   889
# yes   422   697

pred <- prediction(test1$CARTf1[,2],test1$y,label.ordering=c("no","yes"))
pred_choisi_c5.0 <- prediction(test1$CARTf1[,2],test1$y,label.ordering=c("no","yes"))

auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val # 0.7790163

plot(performance(pred_choisi_c5.0,"tpr","fpr"),col='red',lty=2,main='AUC de mod?les CART')



#### base 2 #####

c50 <- C5.0(y ~ . ,data = train2[,1:nrow(N2)],rules=T)

test2$CARTf1 <- predict(c50,type="prob",test2)

# aprraent
train2$cart <- predict(c50,type="class",train2)
err_cart_train <- sum(train2$cart != train2$y) / nrow(train2)
err_cart_train  # 0.0958956

table_train<-table(train2$cart ,train2$y)

# no   yes
# no  27641  2730
# yes   305   973


# test
test2$cart <- predict(c50,type="class",test2)
err_cart_test <- sum(test2$cart != test2$y) / nrow(test2)
err_cart_test # 0.1047781

table_test<-table(test2$cart,test2$y)
# 
# no   yes
# no  11782  1227
# yes   194   359


pred <- prediction(test2$CARTf1[,2],test2$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val # 0.6263115



#### base 3 #####

c50 <- C5.0(y ~ . ,data = train3[,1:nrow(N3)],rules=T)

test3$CARTf1 <- predict(c50,type="prob",test3)

# aprraent
train3$cart <- predict(c50,type="class",train3)
err_cart_train <- sum(train3$cart != train3$y) / nrow(train3)
err_cart_train  # 0.0950425

table_train<-table(train3$cart ,train3$y)


# no   yes
# no  27584  2646
# yes   362  1057


# test
test3$cart <- predict(c50,type="class",test3)
err_cart_test <- sum(test3$cart != test3$y) / nrow(test3)
err_cart_test # 0.1038932

table_test<-table(test3$cart,test3$y)
# 
# 
# no   yes
# no  11765  1198
# yes   211   388


pred <- prediction(test3$CARTf1[,2],test3$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val # 0.6435701



#### base 4 #####

c50 <- C5.0(y ~ . ,data = train4[,1:nrow(N4)],rules=T)

test4$CARTf1 <- predict(c50,type="prob",test4)

# aprraent
train4$cart <- predict(c50,type="class",train4)
err_cart_train <- sum(train4$cart != train4$y) / nrow(train4)
err_cart_train  # 0.1043319

table_train<-table(train4$cart ,train4$y)

# no   yes
# no  27693  3049
# yes   253   654


# test
test4$cart <- predict(c50,type="class",test4)
err_cart_test <- sum(test4$cart != test4$y) / nrow(test4)
err_cart_test # 0.1053679

table_test<-table(test4$cart,test4$y)
# 

# no   yes
# no  11852  1305
# yes   124   281


pred <- prediction(test4$CARTf1[,2],test4$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val # 0.5834106




#### base 5 #####

c50 <- C5.0(y ~ . ,data = train5[,1:nrow(N5)],rules=T)

test5$CARTf1 <- predict(c50,type="prob",test5)

# aprraent
train5$cart <- predict(c50,type="class",train5)
err_cart_train <- sum(train5$cart != train5$y) / nrow(train5)
err_cart_train  # 0.1043319

table_train<-table(train5$cart ,train5$y)
# 
# no   yes
# no  27693  3049
# yes   253   654


# test
test5$cart <- predict(c50,type="class",test5)
err_cart_test <- sum(test5$cart != test5$y) / nrow(test5)
err_cart_test # 0.1053679

table_test<-table(test5$cart,test5$y)
# # 
# no   yes
# no  11852  1305
# yes   124   281

pred <- prediction(test5$CARTf1[,2],test5$y,label.ordering=c("no","yes"))
auc <- performance(pred,"auc")
auc_val<-performance(pred,"auc")@y.values[[1]]
auc_val # 0.5834106



######################### fin #############################

#### les courbes ROC #####

# COURBE ROC LOGIT
plot(roc1,col="black",lwd=1)

# COURBE ROC CART
plot(performance(pred_choisi_cart,"tpr","fpr"),col='red',lty=2,add=TRUE)

# COURBE ROC CHAID
plot(performance(pred_choisi_chaid,"tpr","fpr"),col='blue',lty=2,add=TRUE)

# COURBE ROC C5.0
plot(performance(pred_choisi_c5.0,"tpr","fpr"),col='magenta',lty=1,add=TRUE)


abline(0,1)

# legend("bottomright",legend=c("logit"),col=c("red"),lty=1,lwd=2)
# segments(0,0,1,1,lty=3)

legend("bottomright",c('LOGIT ',' CART ',' CHAID ', ' C5.0 '),col=c('black','red','blue','magenta'),
       lty=c(2,1),lwd=3)
title("Courbes ROC")

######################### fin du tp ############################
