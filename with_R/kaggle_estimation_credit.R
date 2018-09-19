base_importer_init_test <- read.csv("D:/kaggle/all/application_test.csv", header = T, sep = ",")
row.names(base_importer_init_test) <- paste("ind_test_",row.names(base_importer_init_test),sep="")
dim(base_importer_init_test)

Mat_na_test <- matrix(NA,1,1)
colnames(Mat_na_test) <- "colonne_na_test"
for(i in 2:ncol(base_importer_init_test)){
presence_na <- sum(is.na(base_importer_init_test[,i]))
if (presence_na!=0) { 
  Mat_na_test <- rbind(Mat_na_test,i)
  }
}
# head(base_importer_init_train)
dim(Mat_na_test)

base_importer_init_train <- read.csv("D:/kaggle/all/application_train.csv", header = T, sep = ",")
row.names(base_importer_init_train) <- paste("ind_train_",row.names(base_importer_init_train),sep="")

Var_expliquer <- base_importer_init_train$TARGET
base_importer_init_train$TARGET <- NULL
## > base_importer <- rbind( base_importer_init_train,base_importer_init_test)
base_importer <- base_importer_init_train

head(base_importer)
#####
list_quanti <- c("CNT_CHILDREN","AMT_INCOME_TOTAL",
"AMT_CREDIT", "AMT_ANNUITY","AMT_GOODS_PRICE",
"REGION_POPULATION_RELATIVE", "DAYS_BIRTH",
"DAYS_EMPLOYED", "DAYS_REGISTRATION", "DAYS_ID_PUBLISH",
"OWN_CAR_AGE ","CNT_FAM_MEMBERS","HOUR_APPR_PROCESS_START",
"EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3",
"APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BEGINEXPLUATATION_AVG",
"YEARS_BUILD_AVG", "COMMONAREA_AVG", "ELEVATORS_AVG",
"ENTRANCES_AVG", "FLOORSMAX_AVG", "FLOORSMIN_AVG",
"LANDAREA_AVG" ,
"LIVINGAPARTMENTS_AVG","LIVINGAREA_AVG",
"NONLIVINGAPARTMENTS_AVG","NONLIVINGAREA_AVG",
"APARTMENTS_MODE","BASEMENTAREA_MODE",
"YEARS_BEGINEXPLUATATION_MODE","YEARS_BUILD_MODE",
"COMMONAREA_MODE","ELEVATORS_MODE","ENTRANCES_MODE",
"FLOORSMAX_MODE","FLOORSMIN_MODE","LANDAREA_MODE",
"LIVINGAPARTMENTS_MODE","LIVINGAREA_MODE",
"NONLIVINGAPARTMENTS_MODE","NONLIVINGAREA_MODE",
"APARTMENTS_MEDI","BASEMENTAREA_MEDI",
"YEARS_BEGINEXPLUATATION_MEDI","YEARS_BUILD_MEDI",
"COMMONAREA_MEDI","ELEVATORS_MEDI","ENTRANCES_MEDI",
"FLOORSMAX_MEDI","FLOORSMIN_MEDI","LANDAREA_MEDI",
"LIVINGAPARTMENTS_MEDI","LIVINGAREA_MEDI",
"NONLIVINGAPARTMENTS_MEDI","NONLIVINGAREA_MEDI",
"TOTALAREA_MODE","OBS_30_CNT_SOCIAL_CIRCLE",
"DEF_30_CNT_SOCIAL_CIRCLE",
"OBS_60_CNT_SOCIAL_CIRCLE","DEF_60_CNT_SOCIAL_CIRCLE",
"DAYS_LAST_PHONE_CHANGE","AMT_REQ_CREDIT_BUREAU_HOUR",
"AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK",
"AMT_REQ_CREDIT_BUREAU_MON","AMT_REQ_CREDIT_BUREAU_QRT",
"AMT_REQ_CREDIT_BUREAU_YEAR"
)

liste_categoriel <- c("OWN_CAR_AGE ","CNT_FAM_MEMBERS",
                      "HOUR_APPR_PROCESS_START",
                      "OBS_30_CNT_SOCIAL_CIRCLE",
                      "DEF_30_CNT_SOCIAL_CIRCLE",
                      "OBS_60_CNT_SOCIAL_CIRCLE","DEF_60_CNT_SOCIAL_CIRCLE",
                      "AMT_REQ_CREDIT_BUREAU_HOUR",
                      "AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK",
                      "AMT_REQ_CREDIT_BUREAU_MON","AMT_REQ_CREDIT_BUREAU_QRT",
                      "AMT_REQ_CREDIT_BUREAU_YEAR"
  )
#####
# convertion de variable est quanti et quali
for ( i in 1:ncol(base_importer) )  {
  if ( colnames(base_importer)[i] %in% list_quanti) { 
    base_importer[,i] <- as.numeric(as.character(base_importer[,i]))
  } else { 
    base_importer[,i] <- as.factor(as.character(base_importer[,i]))
    }
}

class(base_importer)
# row.names(base_importer) <- paste("ind_",row.names(base_importer),sep="")
dim(base_importer)

# 1.1 / on separe X de Y
# Var_expliquer <- base_importer$TARGET
# base_importer$TARGET <- NULL
base_importer_no_Y <- base_importer
# On sépare la base en deux selon les individus qui ont des NA et ceux qui n'ont pas de NA

base_importer_no_Y_matrix <- as.matrix(base_importer_no_Y)
 
# 1.2.1
base_importer_no_Y_NA <- matrix(base_importer_no_Y_matrix[is.na(base_importer_no_Y_matrix)],ncol=ncol(base_importer_no_Y_matrix))
colnames(base_importer_no_Y_NA) <- colnames(base_importer_no_Y_matrix)

# 1.2.2
base_importer_no_Y_NO_NA <- matrix(base_importer_no_Y_matrix[!(is.na(base_importer_no_Y_matrix))],ncol=ncol(base_importer_no_Y_matrix))
colnames(base_importer_no_Y_NO_NA) <- colnames(base_importer_no_Y_matrix)

# Vérification
isTRUE((nrow(base_importer_no_Y_NA)+
          nrow(base_importer_no_Y_NO_NA))==nrow(base_importer_no_Y_matrix))

base_importer_no_Y_NO_NA <- as.data.frame(base_importer_no_Y_NO_NA)


# 1.2.2.1
nb_ind_na_to_generat <- round((nrow(base_importer_no_Y_NO_NA)*0.01),0)

# selection des individus aleatoirement
set.seed(123)
id_ind_na_to_gener <- sample(1:nrow(base_importer_no_Y_NO_NA),nb_ind_na_to_generat)


# séparation de la base des NA généré
no_na_gegere_seul <- base_importer_no_Y_NO_NA[-id_ind_na_to_gener,]

na_gegere_seul <- base_importer_no_Y_NO_NA[id_ind_na_to_gener,]

nb_ind_na_to_generat_2 <- round((nrow(na_gegere_seul)*0.03),0)

# sauvegarde de la valeur relle des NA
na_gegere_seul_real <- na_gegere_seul
row.names(na_gegere_seul_real)


# suite : génération des NA
# initialisation de la matrice pour accéder aux NA
M1 <- matrix(c("nom_ligne","name_colonne","num_colonne","valeur_real"),1,4)
for(i in 2:ncol(na_gegere_seul)) {
# if ( is.factor(na_gegere_seul[,i]) == FALSE & is.character(na_gegere_seul[,i]) == FALSE ) { 
  set.seed(14121990)
  id_ind_na_gener_2 <- sample(1:nrow(na_gegere_seul),nb_ind_na_to_generat_2)
  
  for ( j in id_ind_na_gener_2){
  Val_real <- na_gegere_seul[j,i]
  na_gegere_seul[j,i] <- NA
  M1_row_names_j <- row.names(na_gegere_seul[j,])
  M1_col_nam_i <- colnames(na_gegere_seul)[i]
  M1_col_num_i <- i
  
  M1_transit <- matrix(c(M1_row_names_j,M1_col_nam_i,M1_col_num_i,Val_real),1,4)
  
  M1 <- rbind(M1,M1_transit)
  
   }
# }
} 
dim(M1)
head(M1)
tail(M1)
 colnames(M1) <- M1[1,]
M1 <- M1[-1,]
dim(M1)
head(M1)
tail(M1)
M1_DATA <- as.data.frame(M1)
# reconstitution de la bbase avec les NA généré

base_rematcher <- rbind(no_na_gegere_seul,na_gegere_seul)

dim(base_rematcher)
dim(no_na_gegere_seul)
dim(na_gegere_seul)

sum(is.na(na_gegere_seul))

# row.names(na_gegere_seul)

# variable numérique
# install.packages("missMDA")

library(missMDA)

# head(base_rematcher)
# summary(base_rematcher)
# for(i in colnames(base_rematcher)) {

M_total_erreur <- matrix(c("nb_iteration","val_erreur"),1,2)
M_total_M_erreur_num_all_erreur <- matrix(c("nb_iteration","val_erreur"),1,2)

for (iki in 1:10 ) {
M_erreur <- matrix(0,1,1)
M_erreur_num_all <- matrix(0,1,1)
 for (i in 1:ncol(base_rematcher)) {
   # i=121
   # i=7
   # i=44
 x_var_na <- as.data.frame(base_rematcher[,i] )
 row.names(x_var_na) <- row.names(base_rematcher)
 colnames(x_var_na) <- "valeur_i"
 x_var_na$num_col <- row.names(x_var_na)
 

 
 # if ( is.factor(base_rematcher[,i])==FALSE) { 
   if( i %in% (unique(M1_DATA$num_colonne)) ){ 
     
     x_var_na_var_x <- base_rematcher[(row.names(base_rematcher) %in% (M1_DATA[M1_DATA$num_colonne==i,]$nom_ligne)) ,i]
     x_var_na_var_x <- as.data.frame(x_var_na_var_x)
     row.names(x_var_na_var_x) <- M1_DATA[M1_DATA$num_colonne==i,]$nom_ligne
     
     x_var_na_var_y <- x_var_na[!(row.names(x_var_na) %in% row.names(x_var_na_var_x)),]
     row.names(x_var_na_var_y) <-  x_var_na_var_y$num_col
     
     
    
     
   b_use_to_pred <- base_rematcher[,-i]
# b_use_to_pred <- base_rematcher
  b_use_to_pred_quant <- b_use_to_pred[,colnames(b_use_to_pred) %in% list_quanti]
   
  for ( iz in 1:ncol(b_use_to_pred_quant) )  {
    if ( colnames(b_use_to_pred_quant)[iz] %in% list_quanti) { 
      b_use_to_pred_quant[,iz] <- as.numeric(as.character(b_use_to_pred_quant[,iz]))

      } else { 
      b_use_to_pred_quant[,iz] <- as.factor(as.character(b_use_to_pred_quant[,iz]))
    }
  }
  
   
  for(iz in 1:ncol(b_use_to_pred_quant)) {
    # if ( colnames(b_use_to_pred_quant)[iz] %in% liste_categoriel) {
    lvel <- matrix(levels( as.factor(as.character(b_use_to_pred_quant[,iz]))),ncol=1)
    if ( nrow(lvel) < 50) {
    b_use_to_pred_quant[,iz] <- as.factor(as.character(b_use_to_pred_quant[,iz]))
    }
  }
  
  
  mt_mt <- matrix(NA,1,1)
  for(iz in 1:ncol(b_use_to_pred_quant)) {
    if ( sum(is.na(b_use_to_pred_quant[,iz])) > (nrow(b_use_to_pred_quant)/5)) { 
      mt_mt <- rbind(mt_mt,iz)
      }
    # print(iz)
  }
  
  mt_mt <- mt_mt[-1,]
  dim(b_use_to_pred_quant)
  b_use_to_pred_quant[,mt_mt] <- NULL
  dim(b_use_to_pred_quant)
  
  # b_use_to_pred_quant <- b_use_to_pred_quant[,!(colnames(b_use_to_pred_quant) %in% liste_categoriel)]
  
  # # MÃ©thode3 de gestion des valeurs manquantes:
  # #    ACP indirect les valeurs manquantes sont imputÃ©es
  # base_importer_init_test
mtm <- matrix(NA,1,1)
  for (iz in 1:ncol(b_use_to_pred_quant)){
    if (is.numeric(b_use_to_pred_quant[,iz])==TRUE){
  mtm <- rbind(mtm,iz)    
    }
  }
mtm <- mtm[-1,]
dim(mtm)
dim(b_use_to_pred_quant)
b_use_to_pred_quant <- b_use_to_pred_quant [,mtm]
dim(b_use_to_pred_quant)
    
# for( ir in 1:ncol(b_use_to_pred_quant)){
#   b_use_to_pred_quant[,ir] <- as.numeric(as.character(b_use_to_pred_quant[,ir]))
# }

# nb<-estim_ncpPCA(b_use_to_pred_quant[1:100,],
#                     ncp.min = 0,ncp.max = 2 )
#   summary(b_use_to_pred_quant)
  # nb$ncp
#   n_imput<-nb$ncp
  # 
  res_sd_imput<-imputePCA(b_use_to_pred_quant,ncp=5 )
  liste_name_quanti <- colnames(b_use_to_pred_quant)
# res_sd_imput$completeObs
# 
  sd<-as.data.frame(res_sd_imput$completeObs)

  if(nrow(sd)==0){ 
    sd<-as.data.frame(res_sd_imput)
     }
  
# nrow(sd)
 b_use_to_pred_quant_impute_na<-as.data.frame(sd)
 
 # }
 
### prédiction de la var en cours
 b_use_to_pred_quant_impute_na_x_y <- cbind(b_use_to_pred_quant_impute_na,x_var_na$valeur_i)
 library(data.table)
 setnames(b_use_to_pred_quant_impute_na_x_y , old=c("x_var_na$valeur_i"), new=c("valeur_i"))
 
 
 b_use_to_pred_quant_impute_na_test <- b_use_to_pred_quant_impute_na_x_y[ row.names(b_use_to_pred_quant_impute_na_x_y) %in% row.names(x_var_na_var_x), ]
 b_use_to_pred_quant_impute_na_train <- b_use_to_pred_quant_impute_na_x_y[ !(row.names(b_use_to_pred_quant_impute_na_x_y) %in% row.names(x_var_na_var_x)), ]
 
 b_use_to_pred_quant_impute_na_train_x <-b_use_to_pred_quant_impute_na_train[,-(ncol(b_use_to_pred_quant_impute_na_train))]
 b_use_to_pred_quant_impute_na_train_y <-b_use_to_pred_quant_impute_na_train[,(ncol(b_use_to_pred_quant_impute_na_train))]
 
 b_use_to_pred_quant_impute_na_test_x <-b_use_to_pred_quant_impute_na_test[,-(ncol(b_use_to_pred_quant_impute_na_test))]
 b_use_to_pred_quant_impute_na_test_y <-b_use_to_pred_quant_impute_na_test[,(ncol(b_use_to_pred_quant_impute_na_test))]
 
 b_use_to_pred_quant_impute_na_y_real <- M1_DATA[M1_DATA$num_colonne==i,]$valeur_real
 b_use_to_pred_quant_impute_na_y_real <- as.data.frame(b_use_to_pred_quant_impute_na_y_real)
 row.names(b_use_to_pred_quant_impute_na_y_real) <- M1_DATA[M1_DATA$num_colonne==i,]$nom_ligne
 
 # install.packages('extraTrees')
 # options( java.parameters = "-Xmx4g" ) # 2g defines 2GB of heap mtry - 1 Gb n'est pas suffisant pour le MNIST
 library(extraTrees)
 
 
 set.seed(123)
 list_echant <- sample(1:nrow(b_use_to_pred_quant_impute_na_train_x), 
                       5000, replace = FALSE)
 
  list_echant <- 1:nrow(b_use_to_pred_quant_impute_na_train_x)
                      
 
 
 b_use_to_pred_quant_impute_na_train_echant <- b_use_to_pred_quant_impute_na_train[list_echant,]
 
 b_use_to_pred_quant_impute_na_train_x_echant <- b_use_to_pred_quant_impute_na_train_x[list_echant,]
 b_use_to_pred_quant_impute_na_train_y_echant <- b_use_to_pred_quant_impute_na_train_y[list_echant]
 # nb_mtry <-round(((ncol(b_use_to_pred_quant_impute_na_train_x_echant))/3),0)
 # nb_ntree <- 500
 # nb_numRandomCuts=2
 # nb_numThreads=3
 
 library(MASS)
 library(randomForest)
 # install.packages("caret")
 library(caret)
 
  rmse = function(actual, predicted) {
    sqrt(mean((actual - predicted)^2))
  }
 # 
 # cv_5 = trainControl(method = "cv", number = 5)
 # rf_grid =  expand.grid(mtry = 1:nb_mtry)
 # 
 # dim(b_use_to_pred_quant_impute_na_train_echant)
 # sum(is.na(b_use_to_pred_quant_impute_na_train_echant))
 # 
 # rf_fit = train(valeur_i ~ ., data = b_use_to_pred_quant_impute_na_train_echant,
 #                method = "rf",
 #                trControl = cv_5,
 #                tuneGrid = rf_grid)
 # 
 # 
 # multi_regression_model <- extraTrees(b_use_to_pred_quant_impute_na_train_x_echant, b_use_to_pred_quant_impute_na_train_y_echant, 
 #                                 ntree=nb_ntree, mtry=nb_mtry, 
 #                                 numRandomCuts=nb_numRandomCuts, 
 #                                 numThreads=nb_numThreads, nodesize=5)   
 
 # multi_regression_model <- extraTrees(b_use_to_pred_quant_impute_na_train_x_echant, b_use_to_pred_quant_impute_na_train_y_echant, 
 #                                      ntree=nb_ntree, mtry=((ncol(b_use_to_pred_quant_impute_na_train_x_echant))^0.5), 
 #                                      numRandomCuts=1, 
 #                                      numThreads=1, nodesize=1)   
 
# if (is.factor(base_rematcher[,i])==TRUE) {
  # install.packages("class")
 library(class)
 
  prediction <- knn(train = b_use_to_pred_quant_impute_na_train_x_echant,
                   test = b_use_to_pred_quant_impute_na_test_x , 
                   cl= b_use_to_pred_quant_impute_na_train_y_echant,
                   k = 5)
  # multi_class_model <- randomForest(valeur_i ~ ., data=b_use_to_pred_quant_impute_na_train_echant,
  #                                   importance=F, ntree=nb_ntree, mtry=nb_mtry,
  #                                   replace=T, keep.forest=T,nodesize=5)
  # 
 # }
  

  if (is.factor(base_rematcher[,i])==TRUE) {
  
  cible_predict <- matrix(prediction,ncol=1)
  realval <-  as.factor(as.character(b_use_to_pred_quant_impute_na_y_real$b_use_to_pred_quant_impute_na_y_real))
  
  # matrice de confusion
  matric_de_confusion <- table(realval, cible_predict)
  # taux de bonne prediction
  taux_de_bonne_prediction_test_global_exterieur <- sum(diag(matric_de_confusion))/sum(matric_de_confusion) 
  # taux d'erreur  
  erreur_test_global_exterieur <- ( 1 - taux_de_bonne_prediction_test_global_exterieur )
  }
  
  if (is.numeric(base_rematcher[,i])==TRUE) {
    cible_predict <- as.numeric(as.character(matrix(prediction,ncol=1)))
    realval <-  as.numeric(as.character(b_use_to_pred_quant_impute_na_y_real$b_use_to_pred_quant_impute_na_y_real))
    erreur_test_global_exterieur <- rmse(realval, cible_predict)
    
    # # matrice de confusion
    # matric_de_confusion <- table(b_use_to_pred_quant_impute_na_y_real$b_use_to_pred_quant_impute_na_y_real, cible_predict)
    # # taux de bonne prediction
    # taux_de_bonne_prediction_test_global_exterieur <- sum(diag(matric_de_confusion))/sum(matric_de_confusion) 
    # # taux d'erreur  
    # erreur_test_global_exterieur <- ( 1 - taux_de_bonne_prediction_test_global_exterieur )
  }
  
  cible_predict <- as.numeric(as.character(matrix(prediction,ncol=1)))
  realval <-  as.numeric(as.character(b_use_to_pred_quant_impute_na_y_real$b_use_to_pred_quant_impute_na_y_real))
  erreur_numeric_all <- rmse(realval, cible_predict)
  
  
 #   # install.packages("class")
 #   library(class)
 #   
 #   prediction <- knn(train = b_use_to_pred_quant_impute_na_train_x_echant,
 #                     test = b_use_to_pred_quant_impute_na_test_x , 
 #                     cl= b_use_to_pred_quant_impute_na_train_y_echant,
 #                     k = 5)
 #   
 #   cible_predict <- matrix(prediction,ncol=1)
 #   
 #   # matrice de confusion
 #   matric_de_confusion <- table(b_use_to_pred_quant_impute_na_y_real$b_use_to_pred_quant_impute_na_y_real, cible_predict)
 #   # taux de bonne prediction
 #   taux_de_bonne_prediction_test_global_exterieur <- sum(diag(matric_de_confusion))/sum(matric_de_confusion) 
 #   # taux d'erreur  
 #   erreur_test_global_exterieur <- ( 1 - taux_de_bonne_prediction_test_global_exterieur )
 # }
 
  
  # prediction <- predict(multi_regression_model, b_use_to_pred_quant_impute_na_test_x)
  
  M_erreur_num_all <- rbind(M_erreur_num_all, erreur_numeric_all)
  
 M_erreur <- rbind(M_erreur,erreur_test_global_exterieur)

 # x_var_na_final <- x_var_na
 
 x_var_na_final <- x_var_na[ row.names(x_var_na) %in% row.names(x_var_na_var_x), ]
 
 x_var_na_final$valeur_i_predict <- cible_predict
 base_rematcher[x_var_na_final$num_col,i] <- cible_predict
 
 print(paste("iteration " ,iki," , et colonne " ,i," , et erreur ", erreur_test_global_exterieur, " et erreur numeric all " , erreur_numeric_all, sep = " "))
 } else { 
   print(paste("iteration " ,iki," , et colonne " ,i," , no NA ", sep = " "))
 }

 }

M_total_erreur_transit <- matrix(c(iki, 
                                   sum(as.numeric(as.character(M_erreur)),na.rm = TRUE)),
                                 1,2)
M_total_erreur <- rbind(M_total_erreur, M_total_erreur_transit )


M_total_M_erreur_num_all_erreur_transit <- matrix(c(iki, 
                                   sum(as.numeric(as.character(M_erreur_num_all)),na.rm = TRUE)),
                                 1,2)
M_total_M_erreur_num_all_erreur <- rbind(M_total_M_erreur_num_all_erreur, M_total_M_erreur_num_all_erreur_transit )

}

M_total_erreur_save <- M_total_erreur
colnames(M_total_erreur) <- M_total_erreur[1,]
M_total_erreur <- M_total_erreur[-1,]
min(M_total_erreur[,2])



M_total_M_erreur_num_all_erreur_save <- M_total_M_erreur_num_all_erreur
colnames(M_total_M_erreur_num_all_erreur) <- M_total_M_erreur_num_all_erreur[1,]
M_total_M_erreur_num_all_erreur <- M_total_M_erreur_num_all_erreur[-1,]
min(M_total_M_erreur_num_all_erreur[,2])




# clustering H-Kmean

## aller recuperer mon code de EDF

