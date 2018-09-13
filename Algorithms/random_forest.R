library(readr)
library(dplyr)
library(randomForest)
library(caret)

train=read_csv('training_data.csv')
test=read_csv('testing_data.csv')
oot=read_csv('oot_data.csv')
oot=oot[,-1]
train$Fraud=as.factor(train$Fraud)
test$Fraud=as.factor(test$Fraud)
oot$Fraud=as.factor(oot$Fraud)
train=as.data.frame(train)
test=as.data.frame(test)
oot=as.data.frame(oot)
## Tune the Mtry

set.seed(1)
tune=tuneRF(x=train[,-1],y=train[,1],
            stepFactor = 2,
            plot = TRUE,
            ntreeTry = 200,
            trace = TRUE,
            improve = 0.05,
            doBest = TRUE)  # trace means whether to print the progress of the search

plot(tune)  # the best mtry=14
legend("topright", colnames(tune$err.rate),col=1:4,fill=1:4)

importance(tune)
varImpPlot(tune, sort = T, n.var=30, main = "Top 30 Important Vars")

prob_train=as.data.frame(tune$votes)

train$prob=prob_train$`1`

train%>%
  arrange(-prob)%>%
  slice(1:1340)%>%
  filter(Fraud==1)%>%
  summarise(caught=n()) #492

FRD_train_2percent=492/541 # 91%


pred_test = predict(tune,test,type = "prob")
pred_test=as.data.frame(pred_test)
test$prob=pred_test$`1`

test%>%
  arrange(-prob)%>%
  slice(1:587)%>%
  filter(Fraud==1)%>%
  summarise(caught=n()) ##411

FDR_test_2percent=441/473 ## 93%


pred_oot=predict(tune,oot,type = "prob")
pred_oot=as.data.frame(pred_oot)
oot$prob=pred_oot$`1`
oot%>%
  arrange(-prob)%>%
  slice(1:252)%>%
  filter(Fraud==1)%>%
  summarise(caught=n()) ##238

FDR_oot_2percent=238/338 ## 70.4%

## combine a big training set

big_train=rbind(train[,-65],test[,-65])

## Train on the bit training data set
big_rf = randomForest(Fraud~.,data = big_train,mtry = 14, importance = TRUE, ntree =200)

plot(big_rf)
legend("right", colnames(tune$err.rate),col=1:4,fill=1:4)

importance(big_rf)
varImpPlot(big_rf, sort = T, n.var=20, main = "Top 20 Important Vars")


pred_oot_big=predict(big_rf,oot,type = "prob")
pred_oot_big=as.data.frame(pred_oot_big)
oot$prob_2=pred_oot_big$`1`
oot%>%
  arrange(-prob_2)%>%
  slice(1:252)%>%
  filter(Fraud==1)%>%
  summarise(caught=n())  #252

FDR_oot_2percent_big=252/338 # 74.6% Caought every fraud at 2 percent

save(big_rf,file='RandomForest.rda')  ### object big_rf will be the final model to be used for charts.

load(file="RandomForest.rda")

