library(tidyverse)
library(dplyr)
library(readr)
library(xgboost)
library(caret)

Sys.setenv(TZ='America/Los_Angeles')
Sys.time()

#loading data
data.train=read_csv("training_data.csv")
data.test=read_csv("testing_data.csv")
data.oot=read_csv("oot_data.csv")
data.oot=data.oot[,colnames(data.test)]

labels.train = data.train$Fraud
data.train = data.train[-grep('Fraud', colnames(data.train))]
sum(labels.train==1)#541

labels.test = data.test$Fraud
data.test = data.test[-grep('Fraud', colnames(data.test))]
sum(labels.test==1)#135

labels.oot = data.oot$Fraud
data.oot = data.oot[-grep('Fraud', colnames(data.oot))]
sum(labels.oot==1)#338

labels.train=as.factor(labels.train)
levels(labels.train)=c("No","Yes")

labels.test=as.factor(labels.test)
levels(labels.test)=c("No","Yes")

labels.oot=as.factor(labels.oot)
levels(labels.oot)=c("No","Yes")

data.all=rbind(data.train, data.test)
labels.all=c(labels.train, labels.test)

labels.all=as.factor(labels.all)
levels(labels.all)=c("No","Yes")

#parameters
params = list(eta = 0.1, #learning rate (0.01-0.3 start from 0.3) cv lower-slower computation
              max_depth = 50, #cv larger dataset requires deeper tree
              nrounds=50, #max number of iteration cv
              subsample = 0.5,#(0.5-0.8)
              colsample_bytree = 0.5, #(0.5-0.9) number of featrues supplied to a tree
              eval_metric = "auc",
              gamma = 0,#if train error>>>test error, use this. try gamma=5,
              min_child_weight = 11, #cv, stopping criteria
              alpha=1,#cv
              objective = "binary:logistic"
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all", # save losses across all models
  classProbs = TRUE,    # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
##############################################
############       CV1        ##############
##############################################

# optimal:Fitting nrounds = 60, max_depth = 40, eta = 0.2, gamma = 0, colsample_bytree = 0.5, 
# min_child_weight = 1, subsample = 0.5 on full training set (1.5h)

#grid search for hyperparameters
xgb_grid_1 = expand.grid(subsample = c(0.5), 
                         colsample_bytree = c(0.5),#
                         max_depth = c(30,40,50),#
                         min_child_weight = seq(1,11,1), #
                         eta = c(0.1,0.15,0.2,0.25,0.3),#
                         nrounds=c(50,60,70,80),#
                         gamma=0 
                         #alpha=c(1)
)

start.time <- Sys.time()

xgb_train_1 = train(
  x = data.matrix(data.train),
  y = as.factor(labels.train),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

print(Sys.time()-start.time)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(max_depth), y = eta, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

#FDR on testing set
pred=predict(xgb_train_1, data.test, type="prob")
labels.test=as.data.frame(labels.test)
labels.test$prob=pred$Yes

labels.test%>%
  arrange(-prob)%>%
  head(round(0.02*nrow(labels.test),0))%>%
  filter(labels.test=="Yes")%>%
  summarise(FDR_test_2percent=n()/473) ##0.8679

#FDR on training set
pred=predict(xgb_train_1, data.train, type="prob")
labels.train=as.data.frame(labels.train)
labels.train$prob=pred$Yes

labels.train%>%
  arrange(-prob)%>%
  head(round(0.02*nrow(labels.train),0))%>%
  filter(labels.train=="Yes")%>%
  summarise(FDR_train_2percent=n()/541) ##0.9963

#FDR on oot set
pred=predict(xgb_train_1, data.oot, type="prob")
labels.oot=as.data.frame(labels.oot)
labels.oot$prob=pred$Yes

labels.oot%>%
  arrange(-prob)%>%
  head(round(0.02*nrow(labels.oot),0))%>%
  filter(labels.oot=="Yes")%>%
  summarise(FDR_oot_2percent=n()/338) ##0.7278

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
##############################################
############       CV2        ##############
##############################################

# optimal:Fitting nrounds = 70, max_depth = 40, eta = 0.2, gamma = 1, colsample_bytree = 0.5, 
# min_child_weight = 1, subsample = 0.5 on full training set (2.7min)

# 2nd optimal:Fitting nrounds = 90, max_depth = 30, eta = 0.2, gamma = 1, colsample_bytree = 0.9, 
# min_child_weight = 1, subsample = 0.65 on full training set (3h)
#grid search for hyperparameters
xgb_grid_3 = expand.grid(subsample = c(0.5,0.65,0.8), 
                         colsample_bytree = c(0.5,0.7,0.9),#
                         max_depth = c(30,40,50),#
                         min_child_weight = c(1,5,10), #
                         eta = c(0.1,0.2,0.3),#
                         nrounds=c(50,70,90),#
                         gamma=c(1)
                         #alpha=c(1)
)

start.time <- Sys.time()

xgb_train_3 = train(
  x = data.matrix(data.train),
  y = as.factor(labels.train),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_3,
  method = "xgbTree"
)

print(Sys.time()-start.time)


#FDR on testing set
pred=predict(xgb_train_final, data.test, type="prob")
labels.test=as.data.frame(labels.test)
labels.test$prob=pred$Yes

labels.test%>%
  arrange(-prob)%>%
  head(round(0.02*nrow(labels.test),0))%>%
  filter(labels.test=="Yes")%>%
  summarise(FDR_test_2percent=n()/135) ##0.8679-0.8943-0.9197-0.9630

#FDR on training set
pred=predict(xgb_train_final, data.train, type="prob")
labels.train=as.data.frame(labels.train)
labels.train$prob=pred$Yes

labels.train%>%
  arrange(-prob)%>%
  head(round(0.02*nrow(labels.train),0))%>%
  filter(labels.train=="Yes")%>%
  summarise(FDR_train_2percent=n()/541) ##0.9963-0.9945-0.9982-1

#FDR on oot set
pred=predict(xgb_train_final, data.oot, type="prob")
labels.oot=as.data.frame(labels.oot)
labels.oot$prob=pred$Yes

labels.oot%>%
  arrange(-prob)%>%
  head(round(0.02*nrow(labels.oot),0))%>%
  filter(labels.oot=="Yes")%>%
  summarise(FDR_oot_2percent=n()/338) ##0.7278-0.7249-0.7367-0.7366

###############################################################################
###############################################################################
# Refitting the optimal model to both training and testing dataset.
data.train=read_csv("training_data.csv")
data.test=read_csv("testing_data.csv")

labels.train = data.train$Fraud
data.train = data.train[-grep('Fraud', colnames(data.train))]

labels.test = data.test$Fraud
data.test = data.test[-grep('Fraud', colnames(data.test))]

data.all=rbind(data.train,data.test)
labels.all=c(labels.train, labels.test) 
summary(labels.all==1) #1012 fraud

labels.all=as.factor(labels.all)
levels(labels.all)=c("No","Yes")



confusionMatrix(predict(xgb_train_final, data.oot), labels.oot, positive = "Yes") #only missed 3 frauds in total

predict(xgb_train_final, data.oot)
labels.oot

impvarxgb = varImp(xgb_train_retrain)
plot(impvarxgb)

var_imp=impvarxgb$importance
var_imp$variable=rownames(var_imp)
var_imp%>%
  arrange(-Overall)%>%
  head(20)%>%
  ggplot(aes(x=reorder(variable,Overall), y=Overall))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y="Importance",
       x="Variable",
       title="Top 20 Important Variables")+
  theme(plot.title = element_text(hjust = 0.5))




getwd()
save(xgb_train_final, file="xgboost_Model.rda")


#estimating running time with xgboost package

#data.train=read_csv("training_data.csv")
#data.test=read_csv("testing_data.csv")

#labels.train = data.train$Fraud
#data.train = data.train[-grep('Fraud', colnames(data.train))]

#labels.test = data.test$Fraud
#data.test = data.test[-grep('Fraud', colnames(data.test))]

#data.all=rbind(data.train,data.test)
#labels.all=c(labels.train, labels.test) 

#parameters
#params = list(subsample = c(0.65), 
#              colsample_bytree = c(0.9),#
#              max_depth = c(30),#
#              min_child_weight = c(1), #
#              eta = c(0.2),#
#              nrounds=c(90),#
#              gamma=c(1),
#              eval_metric = "auc",
#              objective = "binary:logistic"
#)

# Model (24s on 96352 observations)
#start.time = Sys.time()
#xgb = xgboost(data = data.matrix(data.all), 
#              label = labels.all, params = params, nrounds=90)
#print(Sys.time()-start.time)

#feature_name=names(data.train)
#important_matrix=xgb.importance(model = xgb, feature_names = feature_name)
#print(important_matrix)

#xgb.plot.importance(importance_matrix = important_matrix)

#predict on oot
#pred=predict(xgb_train_final, data.oot, type="prob")
#labels.oot=as.data.frame(labels.oot)
#labels.oot$prob=pred$Yes

#labels.oot%>%
#  arrange(-prob)%>%
#  head(round(0.02*nrow(labels.oot),0))%>%
#  filter(labels.oot=="Yes")%>%
#  summarise(truefraud=n(),FDR_oot_2percent=n()/338) ##0.7278-0.7249-0.7367-0.7455621


