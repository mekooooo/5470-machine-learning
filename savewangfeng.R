rm(list=ls())
options(warn=-1)
## Library ##
library(randomForest)
library(randomForestExplainer)
library(corrplot)
library(rpart)
library(knitr)
library(MASS)
library(factoextra)
library(dplyr)
library(kableExtra)
library(caret)
library(glmnet)
library(ggplot2)
library(plotmo)
library(cvTools)
library(data.table)

## Functions ##
crossvalidate<-function(data, Model, methods){
  # data is the training set with the "seasons" column
  # k is the number of folds we have
  # model is a string describing a linear regression model formula
  # methods is a string with the name of the score column we want to predict
  # random is a logical; do we have random effects in the model?
  
  # Initialize empty list for recording performances
  performances<-c()
  k<-length(unique(data$seasons))
  # One iteration per fold
  for (fold in 1:k)
  {
    # Create training set for this iteration
    # Subset all the datapoints where seasons does not match the current fold
    training_set <- data[data$seasons != fold,]
    # training_set<-data_train[data_train$seasons!=fold,]
    # Create test set for this iteration
    # Subset all the datapoints where seasons matches the current fold
    testing_set <- data[data$seasons == fold,]
    # testing_set<-data_train[data_train$seasons==fold,]
    ## Train model
    
    # If there is a random effect,
    # use lmer() to train model
    # else use lm()
    if(Model=="randomforest")
    {
      if(methods=="directly")
      {
        model<-randomForest(training_set[,-(1:4)],training_set$rank_in_finals,ntree=1000)
        predicted<-predict(model,newdata=testing_set[,-(1:4)])
        predicted[rank(predicted)>3]<-10
        MSPE<-mspe(testing_set$rank_in_finals,predicted)
        RMSPE<-rmspe(testing_set$rank_in_finals,predicted)
        MAPE<-mape(testing_set$rank_in_finals,predicted)
        rp<-rank(predicted,ties.method="first")
        rts<-rank(testing_set$rank_in_finals,ties.method="first")
        corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
        performances<-rbind(performances,c(MSPE=MSPE,RMSPE=RMSPE,MAPE=MAPE,correct_count=corders))
      }
      if(methods=="two-step")
      {
        model<-randomForest(training_set[,-(1:4)],training_set$top3_or_not,ntree=1000)
        trained<-training_set$top3_or_not
        predicted<-predict(model,newdata=testing_set[,-(1:4)])
        predicted<-as.numeric(rank(-predicted)<=3)
        model<-randomForest(cbind(training_set[,-(1:4)],new=trained),
                            training_set$rank_in_finals,ntree=1000)
        predicted<-predict(model,newdata=cbind(testing_set[,-(1:4)],new=predicted))
        predicted[rank(predicted)>3]<-10
        MSPE<-mspe(testing_set$rank_in_finals,predicted)
        RMSPE<-rmspe(testing_set$rank_in_finals,predicted)
        MAPE<-mape(testing_set$rank_in_finals,predicted)
        rp<-rank(predicted,ties.method="first")
        rts<-rank(testing_set$rank_in_finals,ties.method="first")
        corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
        performances<-rbind(performances,c(MSPE=MSPE,RMSPE=RMSPE,MAPE=MAPE,correct_count=corders))
      }
    }
    if(Model=="cart")
    {
      if(methods=="directly")
      {
        model<-rpart(rank_in_finals~.,data=training_set[,c(-1,-3,-4)],method="anova")
        predicted<-predict(model,newdata=testing_set[,-(1:4)])
        predicted[rank(predicted)>3]<-10
        MSPE<-mspe(testing_set$rank_in_finals,predicted)
        RMSPE<-rmspe(testing_set$rank_in_finals,predicted)
        MAPE<-mape(testing_set$rank_in_finals,predicted)
        rp<-rank(predicted,ties.method="first")
        rts<-rank(testing_set$rank_in_finals,ties.method="first")
        corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
        performances<-rbind(performances,c(MSPE=MSPE,RMSPE=RMSPE,MAPE=MAPE,correct_count=corders))
      }
      if(methods=="two-step")
      {
        model<-rpart(top3_or_not~.,data=training_set[,c(-1,-2,-4)],method="anova")
        trained<-training_set$top3_or_not
        predicted<-predict(model,newdata=testing_set[,-(1:4)])
        predicted<-as.numeric(rank(-predicted)<=3)
        model<-rpart(rank_in_finals~.,data=cbind(training_set[,c(-1,-3,-4)],new=trained),method="anova")
        predicted<-predict(model,newdata=cbind(testing_set[,-(1:4)],new=predicted))
        predicted[rank(predicted)>3]<-10
        MSPE<-mspe(testing_set$rank_in_finals,predicted)
        RMSPE<-rmspe(testing_set$rank_in_finals,predicted)
        MAPE<-mape(testing_set$rank_in_finals,predicted)
        rp<-rank(predicted,ties.method="first")
        rts<-rank(testing_set$rank_in_finals,ties.method="first")
        corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
        performances<-rbind(performances,c(MSPE=MSPE,RMSPE=RMSPE,MAPE=MAPE,correct_count=corders))
      }
    }
    if(Model=="ridge")
    {
      if(methods=="directly")
      {
        model<-cv.glmnet(x=as.matrix(training_set[,-(1:4)]),y=training_set$rank_in_finals,
                         type.measure="mse",nfolds=4,alpha=0)
        predicted<-predict(model,new=as.matrix(testing_set[,c(-(1:4))]))
        predicted[rank(predicted)>3]<-10
        MSPE<-mspe(testing_set$rank_in_finals,predicted)
        RMSPE<-rmspe(testing_set$rank_in_finals,predicted)
        MAPE<-mape(testing_set$rank_in_finals,predicted)
        rp<-rank(predicted,ties.method="first")
        rts<-rank(testing_set$rank_in_finals,ties.method="first")
        corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
        performances<-rbind(performances,c(MSPE=MSPE,RMSPE=RMSPE,MAPE=MAPE,correct_count=corders))
      }
      if(methods=="two-step")
      {
        model<-cv.glmnet(x=as.matrix(training_set[,-(1:4)]),y=training_set$top3_or_not,
                         type.measure="mse",nfolds=4,alpha=0)
        trained<-training_set$top3_or_not
        predicted<-predict(model,new=as.matrix(testing_set[,-(1:4)]))
        predicted<-as.numeric(rank(-predicted)<=3)
        model<-cv.glmnet(x=as.matrix(cbind(training_set[,-(1:4)],new=trained)),y=training_set$rank_in_finals,
                         type.measure="mse",nfolds=4,alpha=0)
        predicted<-predict(model,new=as.matrix(cbind(testing_set[,-(1:4)],new=predicted)))
        predicted[rank(predicted)>3]<-10
        MSPE<-mspe(testing_set$rank_in_finals,predicted)
        RMSPE<-rmspe(testing_set$rank_in_finals,predicted)
        MAPE<-mape(testing_set$rank_in_finals,predicted)
        rp<-rank(predicted,ties.method="first")
        rts<-rank(testing_set$rank_in_finals,ties.method="first")
        corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
        performances<-rbind(performances,c(MSPE=MSPE,RMSPE=RMSPE,MAPE=MAPE,correct_count=corders))
      }
    }
    if(Model=="lasso")
    {
      if(methods=="directly")
      {
        model<-cv.glmnet(x=as.matrix(training_set[,-(1:4)]),y=training_set$rank_in_finals,
                         type.measure="mse",nfolds=4,alpha=1)
        predicted<-predict(model,new=as.matrix(testing_set[,c(-(1:4))]))
        predicted[rank(predicted)>3]<-10
        MSPE<-mspe(testing_set$rank_in_finals,predicted)
        RMSPE<-rmspe(testing_set$rank_in_finals,predicted)
        MAPE<-mape(testing_set$rank_in_finals,predicted)
        rp<-rank(predicted,ties.method="first")
        rts<-rank(testing_set$rank_in_finals,ties.method="first")
        corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
        performances<-rbind(performances,c(MSPE=MSPE,RMSPE=RMSPE,MAPE=MAPE,correct_count=corders))
      }
      if(methods=="two-step")
      {
        model<-cv.glmnet(x=as.matrix(training_set[,-(1:4)]),y=training_set$top3_or_not,
                         type.measure="mse",nfolds=4,alpha=1)
        trained<-training_set$top3_or_not
        predicted<-predict(model,new=as.matrix(testing_set[,-(1:4)]))
        predicted<-as.numeric(rank(-predicted)<=3)
        model<-cv.glmnet(x=as.matrix(cbind(training_set[,-(1:4)],new=trained)),y=training_set$rank_in_finals,
                         type.measure="mse",nfolds=4,alpha=1)
        predicted<-predict(model,new=as.matrix(cbind(testing_set[,-(1:4)],new=predicted)))
        predicted[rank(predicted)>3]<-10
        MSPE<-mspe(testing_set$rank_in_finals,predicted)
        RMSPE<-rmspe(testing_set$rank_in_finals,predicted)
        MAPE<-mape(testing_set$rank_in_finals,predicted)
        rp<-rank(predicted,ties.method="first")
        rts<-rank(testing_set$rank_in_finals,ties.method="first")
        corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
        performances<-rbind(performances,c(MSPE=MSPE,RMSPE=RMSPE,MAPE=MAPE,correct_count=corders))
      }
    }
  }
  # Return the mean of the recorded RMSEs
  return(performances)
}

plotCoeffEvolution=function(penalizedGlm,type='L1')
{
  lambda=penalizedGlm$lambda
  coeff=as.matrix(penalizedGlm$beta)
  rowName=rownames(coeff)
  coeff=data.table(coeff)
  coeff[,name:=rowName]
  coeff=melt(coeff,id.vars = 'name')
  coeff[,variable:=rep(lambda,each=length(unique(name)))]
  ggplot(coeff,aes(x=variable,y=value,color=name))+
    geom_line()+
    xlab(paste0(type,' regularisation'))+
    ylab('Value of coefficient')+
    scale_x_log10()+
    theme_minimal()
}

## Cleaning ##
data<-read.table("data.csv",sep=",",header=T,stringsAsFactors=F)
colnames(data)<-c("name","rank_in_finals","top3_or_not","final_or_not","seasons",
                  "sex","no_of_top3","no_of_top1","in_group","start","time",
                  "professional","mean","median","mode","std","regular","best","worst")
data[is.na(data)]<-0
data$sex<-ifelse(data$sex=="男",1,0);data$rank_in_finals<-ifelse(!data$rank_in_finals,10,data$rank_in_finals)
data$final_or_not<-NULL
## Correlation Matrix ##
corrplot(cor(data[,-(1:4)]),order="hclust",addrect = 12)
## Drop mode and median ##
data$median<-data$mode<-NULL
pca<-prcomp(~.,data[,-(1:4)])
summary(pca)
pca
fviz_eig(pca,addlabels = T)
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) # Avoid text overlapping
data_train<-data[data$seasons<=5,];data_test<-data[data$seasons==6,]


# fviz_pca_ind(pca_train,col.ind = "cos2")
k<-crossvalidate(data_train,Model="randomforest",methods="directly")
cat("Model: randomforest\t\tMethods: directly\n\n");k;cat("\nmean:",mean(k[,1]))
k<-crossvalidate(data_train,Model="randomforest",methods="two-step")
cat("Model: randomforest\t\tMethods: two-step\n\n");k;cat("\nmean:",mean(k[,1]))
k<-crossvalidate(data_train,Model="ridge",methods="directly")
cat("Model: ridge regression\t\tMethods: directly\n\n");k;cat("\nmean:",mean(k[,1]))
k<-crossvalidate(data_train,Model="ridge",methods="two-step")
cat("Model: ridge regression\t\tMethods: two-step\n\n");k;cat("\nmean:",mean(k[,1]))
k<-crossvalidate(data_train,Model="lasso",methods="directly")
cat("Model: lasso\t\tMethods: directly\n\n");k;cat("\nmean:",mean(k[,1]))
k<-crossvalidate(data_train,Model="lasso",methods="two-step")
cat("Model: lasso\t\tMethods: two-step\n\n");k;cat("\nmean:",mean(k[,1]))

ranking_names<-c(data_test$name[data_test$rank_in_finals==1],
  data_test$name[data_test$rank_in_finals==2],
  data_test$name[data_test$rank_in_finals==3])
#########################################
########### Direct Prediction ###########
#########################################

## Ridge Regression ##
r_ini<-glmnet(x=as.matrix(data_train[,-(1:4)]),y=data_train$rank_in_finals,alpha=0)
plot(r_ini,xvar="lambda")
plotCoeffEvolution(r_ini,"L1")
plot_glmnet(r_ini)
ridge_cv<-cv.glmnet(x=as.matrix(data_train[,-(1:4)]),y=data_train$rank_in_finals,
                    type.measure="mse",foldid=data_train$seasons,alpha=0)
plot(ridge_cv)
cat("Lambda chosen by Mean-Square Error: ",ridge_cv$lambda.1se)
score_ridge<-predict(ridge_cv, new=as.matrix(data_test[,-(1:4)]))
cat("Scores:\n ",score_ridge)
# score_ridge[rank(score_ridge)>3]<-10
rp<-rank(score_ridge,ties.method="first")
rts<-rank(data_test$rank_in_finals,ties.method="first")
corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
rank<-rbind(c(1,2,3),c(rp[data_test$rank_in_finals==1],
                       rp[data_test$rank_in_finals==2],rp[data_test$rank_in_finals==3]))
row.names(rank)<-c("Actual Ranking","Prediction")
colnames(rank)<-ranking_names
kable(rank)
cat("Accuracy of Prediction:",corders,"/ 3") #3/3

## Lasso ##
l_ini<-glmnet(x=as.matrix(data_train[,-(1:4)]),y=data_train$rank_in_finals,alpha=1)
plot(l_ini)
plot_glmnet(l_ini)
plotCoeffEvolution(l_ini,"L2")
lasso_cv<-cv.glmnet(x=as.matrix(data_train[,-(1:4)]),y=data_train$rank_in_finals,
                    type.measure="mse",foldid=data_train$seasons,alpha=1)
plot(lasso_cv)
cat("Lambda chosen by Mean-Square Error: ",lasso_cv$lambda.1se)
score_lasso<-predict(lasso_cv, new=as.matrix(data_test[,-(1:4)]))
cat("Scores:\n ",score_lasso)
rp<-rank(score_lasso,ties.method="first")
rts<-rank(data_test$rank_in_finals,ties.method="first")
corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
rank<-rbind(c(1,2,3),c(rp[data_test$rank_in_finals==1],
                       rp[data_test$rank_in_finals==2],rp[data_test$rank_in_finals==3]))
row.names(rank)<-c("Actual Ranking","Prediction")
colnames(rank)<-ranking_names
kable(rank)
cat("Accuracy of Prediction:",corders,"/ 3") #3/3
imp_lasso<-c(names(coef(lasso_cv)[,1])[-1][rank(-varImp(lasso_cv$glmnet.fit,lasso_cv$lambda.1se)[,1])==1],
             names(coef(lasso_cv)[,1])[-1][rank(-varImp(lasso_cv$glmnet.fit,lasso_cv$lambda.1se)[,1])==2],
             names(coef(lasso_cv)[,1])[-1][rank(-varImp(lasso_cv$glmnet.fit,lasso_cv$lambda.1se)[,1])==3])

## Random Forest ##
rf_rank<-randomForest(x=data_train[,-(1:4)],y=data_train$rank_in_finals,importance = T)
score_rf<-predict(rf_rank,newdata=data_test[,-(1:4)])
cat("Scores:\n ",score_rf)
# score_rf[rank(score_rf)>3]<-10
rp<-rank(score_rf,ties.method="first")
rts<-rank(data_test$rank_in_finals,ties.method="first")
corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
rank<-rbind(c(1,2,3),c(rp[data_test$rank_in_finals==1],
                       rp[data_test$rank_in_finals==2],rp[data_test$rank_in_finals==3]))
row.names(rank)<-c("Actual Ranking","Prediction")
colnames(rank)<-ranking_names
kable(rank)
cat("Accuracy of Prediction:",corders,"/ 3")
importance(rf_rank,type=1)
imp_rf<-names(sort(rank(-rf_rank$importance[,1])))[1:3]

#########################################
########## Two-step Prediction ##########
#########################################

## Ridge Regression ##
model<-cv.glmnet(x=as.matrix(data_train[,-(1:4)]),y=data_train$top3_or_not,
                 type.measure="mse",nfolds=4,alpha=0)
trained<-data_train$top3_or_not
predicted<-predict(model,new=as.matrix(data_test[,-(1:4)]))
predicted<-as.numeric(rank(-predicted)<=3)
cat("Accuracy of Prediction on top3_or_not: ",sum(predicted[data_test$top3_or_not==1]),"/ 3")
model<-cv.glmnet(x=as.matrix(cbind(data_train[,-(1:4)],new=trained)),
                 y=data_train$rank_in_finals,
                 type.measure="mse",nfolds=4,alpha=0)
r_ini<-glmnet(x=as.matrix(cbind(data_train[,-(1:4)],new=trained)),
                  y=data_train$rank_in_finals,alpha=0)
plot(r_ini,xvar="lambda")
plotCoeffEvolution(r_ini,"L1")
plot_glmnet(r_ini)
plot(model)
cat("Lambda chosen by Mean-Square Error: ",model$lambda.min)
predicted<-predict(model,new=as.matrix(cbind(data_test[,-(1:4)],new=predicted)))
cat("Scores:\n ",predicted)
# predicted[rank(predicted)>3]<-10
# MSPE<-mspe(data_test$rank_in_finals,predicted)
# RMSPE<-rmspe(data_test$rank_in_finals,predicted)
# MAPE<-mape(data_test$rank_in_finals,predicted)
rp<-rank(predicted,ties.method="first")
rts<-rank(data_test$rank_in_finals,ties.method="first")
corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
rank<-rbind(c(1,2,3),c(rp[data_test$rank_in_finals==1],
                       rp[data_test$rank_in_finals==2],rp[data_test$rank_in_finals==3]))
row.names(rank)<-c("Actual Ranking","Prediction")
colnames(rank)<-ranking_names
kable(rank)
cat("Accuracy of Prediction:",corders,"/ 3")

## Lasso ##
model<-cv.glmnet(x=as.matrix(data_train[,-(1:4)]),y=data_train$top3_or_not,
                 type.measure="mse",nfolds=4,alpha=1)
trained<-data_train$top3_or_not
predicted<-predict(model,new=as.matrix(data_test[,-(1:4)]))
predicted<-as.numeric(rank(-predicted)<=3)
cat("Accuracy of Prediction on top3_or_not: ",sum(predicted[data_test$top3_or_not==1]),"/ 3")
model<-cv.glmnet(x=as.matrix(cbind(data_train[,-(1:4)],new=trained)),
                 y=data_train$rank_in_finals,
                 type.measure="mse",nfolds=4,alpha=1)
l_ini<-glmnet(x=as.matrix(cbind(data_train[,-(1:4)],new=trained)),
              y=data_train$rank_in_finals,alpha=1)
plot(l_ini)
plot_glmnet(l_ini)
plotCoeffEvolution(l_ini,"L2")
cat("Lambda chosen by Mean-Square Error: ",model$lambda.min)
predicted<-predict(model,new=as.matrix(cbind(data_test[,-(1:4)],new=predicted)))
# predicted[rank(predicted)>3]<-10
cat("Scores:\n ",predicted)
# MSPE<-mspe(data_test$rank_in_finals,predicted)
# RMSPE<-rmspe(data_test$rank_in_finals,predicted)
# MAPE<-mape(data_test$rank_in_finals,predicted)
rp<-rank(predicted,ties.method="first")
rts<-rank(data_test$rank_in_finals,ties.method="first")
corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
rank<-rbind(c(1,2,3),c(rp[data_test$rank_in_finals==1],
                       rp[data_test$rank_in_finals==2],rp[data_test$rank_in_finals==3]))
row.names(rank)<-c("Actual Ranking","Prediction")
colnames(rank)<-ranking_names
kable(rank)
cat("Accuracy of Prediction:",corders,"/ 3")
imp_lasso2<-c(names(coef(model)[,1])[-1][rank(-varImp(model$glmnet.fit,lasso_cv$lambda.min)[,1])==1],
             names(coef(model)[,1])[-1][rank(-varImp(model$glmnet.fit,lasso_cv$lambda.min)[,1])==2],
             names(coef(model)[,1])[-1][rank(-varImp(model$glmnet.fit,lasso_cv$lambda.min)[,1])==3])

## Random Forest ##
model<-randomForest(data_train[,-(1:4)],data_train$top3_or_not)
trained<-predict(model)
trained<-data_train$top3_or_not
predicted<-predict(model,newdata=data_test[,-(1:4)])
predicted<-as.numeric(rank(-predicted)<=3)
cat("Accuracy of Prediction on top3_or_not: ",sum(predicted[data_test$top3_or_not==1]),"/ 3")
model<-randomForest(cbind(data_train[,-(1:4)],new=trained),
                    data_train$rank_in_finals)
predicted<-predict(model,newdata=cbind(data_test[,-(1:4)],new=predicted))
# predicted[rank(predicted)>3]<-10
cat("Scores:\n ",predicted)
# MSPE<-mspe(data_test$rank_in_finals,predicted)
# RMSPE<-rmspe(data_test$rank_in_finals,predicted)
# MAPE<-mape(data_test$rank_in_finals,predicted)
rp<-rank(predicted,ties.method="first")
rts<-rank(data_test$rank_in_finals,ties.method="first")
corders<-sum(rts[which(rp<=3)]==rp[which(rp<=3)])
rank<-rbind(c(1,2,3),c(rp[data_test$rank_in_finals==1],
                       rp[data_test$rank_in_finals==2],rp[data_test$rank_in_finals==3]))
row.names(rank)<-c("Actual Ranking","Prediction")
colnames(rank)<-ranking_names
kable(rank)
cat("Accuracy of Prediction:",corders,"/ 3")
imp_rf2<-names(sort(rank(-model$importance[,1])))[1:3]

kable(data.frame(imp_lasso,imp_rf,imp_lasso2,imp_rf2))
