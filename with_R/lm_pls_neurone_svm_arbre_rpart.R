
reg_lin_multiple<- function(formule,table_apprenti,table_test,table_estimer)
{ 
  
  reg_lm=lm(formule,  data=table_apprenti)
  # summary(reg_lm)
  
  table_test$predict_1<-predict(reg_lm,newdata=table_test,type="response")
  table_estimer$estime<-predict(reg_lm,newdata=table_estimer,type="response")
  table_test <- 
    table_test %>% 
    mutate(erreur_carre = (mesure1-predict_1)^2)
  
  #View(table_test)
  
  erreur_prediction_test<-sum(table_test$erreur_carre) 
  
  assign("erreur_prediction_test", erreur_prediction_test, envir = .GlobalEnv)
  assign("table_est", table_estimer, envir = .GlobalEnv)
}

reg_pls<- function(formule,table_apprenti,table_test,table_estime_2)
{
  pls_fit<-plsr(formule,data=table_apprenti) # validation="CV"
  
  table_test$predict_pls <- predict(pls_fit,newdata=table_test,type="response",ncomp = 1)
  table_estime_2$estime<-predict(pls_fit,newdata=table_estime_2,type="response",ncomp = 1)
  
  #   names(table_test)[match("mesure1.1 comps",names(table_test))] <- "mesure1.1comps"
  
  
  table_test <- 
    table_test %>% 
    mutate(erreur_carre = (mesure1-predict_pls)^2)
  #View(verificat)
  
  erreur_prediction_test<-sum(table_test$erreur_carre) 
  
  assign("erreur_prediction_test", erreur_prediction_test, envir = .GlobalEnv)
  assign("table_est", table_estime_2, envir = .GlobalEnv)
}


reg_neurone<-function(formule,table_apprenti,table_test,table_estime_2)
{
  
  library(neuralnet)
  
  neurone <- nnet(formule,data=table_apprenti, size=2,decay=1,linout=TRUE,maxit=500, kernel="cible_expliquer")
  
  # print(neurone)
  
  
  table_test$predict_neurone <- predict(neurone,newdata=table_test)
  table_estime_2$estime <- predict(neurone,newdata=table_estime_2)
  
  
  table_test <- 
    table_test %>% 
    mutate(erreur_carre = (mesure1-predict_neurone)^2)
  
  ##View(table_test)
  
  erreur_prediction_test<-sum(table_test$erreur_carre) 
  
  assign("erreur_prediction_test", erreur_prediction_test, envir = .GlobalEnv)
  assign("table_est", table_estime_2, envir = .GlobalEnv)
}

reg_svm_cible_expliquer<- function(formule,table_apprenti,table_test,table_estime_2)
{
  reg_svm_lin<-svm(formule,data=table_apprenti,kernel="linear")
  
  
  table_test$predict_svm_lin <- predict(reg_svm_lin,newdata=table_test)
  table_estime_2$estime<-predict(reg_svm_lin,newdata=table_estime_2)
  
  table_test <- 
    table_test %>% 
    mutate(erreur_carre = (mesure1-predict_svm_lin)^2)
  #View(verificat)
  erreur_prediction_test<-sum(table_test$erreur_carre) 
  
  assign("erreur_prediction_test", erreur_prediction_test, envir = .GlobalEnv)
  assign("table_est", table_estime_2, envir = .GlobalEnv)
}

reg_svm_radial<- function(formule,table_apprenti,table_test,table_estime_2)
{
  reg_svm_rad<-svm(formule,data=table_apprenti,kernel="linear")
  
  
  table_test$predict_svm_rad <- predict(reg_svm_rad,newdata=table_test)
  table_estime_2$estime<-predict(reg_svm_rad,newdata=table_estime_2)
  
  table_test <- 
    table_test %>% 
    mutate(erreur_carre = (mesure1-predict_svm_rad)^2)
  #View(verificat)
  erreur_prediction_test<-sum(table_test$erreur_carre) 
  
  assign("erreur_prediction_test", erreur_prediction_test, envir = .GlobalEnv)
  assign("table_est", table_estime_2, envir = .GlobalEnv)
}

reg_arbre<-function(formule,table_apprenti,table_test,table_estime_2)  
{
  
  reg_arb=rpart(formule,data=table_apprenti,control=rpart.control(cp=0.00000000000001))
  
  
  table_test$predict_reg_arg <- predict(reg_arb,newdata=table_test)
  table_estime_2$estime<- predict(reg_arb,newdata=table_estime_2)
  
  
  table_test <- 
    table_test %>% 
    mutate(erreur_carre = (mesure1-predict_reg_arg)^2)
  #View(table_test)
  erreur_prediction_test<-sum(table_test$erreur_carre) 
  
  assign("erreur_prediction_test", erreur_prediction_test, envir = .GlobalEnv)
  assign("table_est", table_estime_2, envir = .GlobalEnv)
}

reg_polynomial_departement<- function(formule,table_apprenti,table_test,table_estime_2)
{
  # formule<-str_replace_all(formule, "vente", "vente**2")
  formule<-str_replace_all(formule, "vente", "poly(vente)")
  #  formule<-str_replace_all(formule, "SURFACE", "poly(SURFACE)")
  
  formule<- as.formula(paste(formule[2],formule[1],formule[3],collapse = "+"))
  
  reg_poly <- lm( formule, data=table_apprenti)
  
  
  table_test$predict_reg_poly<- predict(reg_poly,newdata=table_test)
  table_estime_2$estime<- predict(reg_poly,newdata=table_estime_2)
  
  
  table_test <- 
    table_test %>% 
    mutate(erreur_carre = (mesure1-predict_reg_poly)^2)
  #View(table_test)
  
  erreur_prediction_test<-sum(table_test$erreur_carre) 
  
  assign("erreur_prediction_test", erreur_prediction_test, envir = .GlobalEnv)
  assign("table_est", table_estime_2, envir = .GlobalEnv)
}



