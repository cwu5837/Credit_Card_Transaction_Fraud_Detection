
# load final data
training=read.csv('training_data.csv')
testing=read.csv('testing_data.csv')
holdout=read.csv('oot_data.csv')
ks=read.csv('ks_scores.csv')
ks_top30=ks %>%
  arrange(-result) %>%
  head(30)
training=training %>%
  select(as.character(ks_top30$names),Fraud)
testing=testing %>%
  select(as.character(ks_top30$names),Fraud)
holdout=holdout %>%
  select(as.character(ks_top30$names),Fraud)

# adjust for unbalanced data
library(ROSE)
library(caret)
down_train <- downSample(x = training, y = as.factor(training$Fraud))   
sampletrain=ROSE(Fraud~.,data=training)$data
sum(sampletrain$Fraud)

# prepare model
fullmod=glm(Fraud~.,data=training,family=binomial)  # change effective variable
summary(fullmod)
nothing=glm(Fraud~1,data=training,family = binomial)
summary(nothing)

# backwards
backwards=step(fullmod)  # backwards is default
formula(backwards)  # results for backwards selection
summary(backwards)  # model results for backwards selection

## forwards
forwards=step(nothing,scope=list(lower=formula(nothing),upper=formula(fullmod)),direction='forward')
formula(forwards)

## bothway
bothways=step(nothing,list(lower=formula(nothing),upper=formula(fullmod)),direction='both',trace=0,silent=TRUE)
formula(bothways)
summary(bothways)

## prediction for training 
probs=predict(bothways,type='response')
probs=predict(fullmod,type='response')
summary(probs)
trainresult=rep(0,67012)
trainresult[probs>0.5]=1
table(trainresult,training$Fraud)

## prediction for testing
probstest=predict(bothways,testing,type='response')
probstest2=predict(bothways,testing,type='response')
testresult=rep(0,29340)
testresult[probstest>0.5]=1
table(testresult,testing$Fraud)
summary(probstest2)

## prediction for holdout set
holdmodel=glm(Fraud ~ amount_sum_28 + Amount + same_card_3 + 
                amount_sum_3 + same_card_7 + same_merch_3 + same_card_14 + 
                amount_sum_7 + amount_avg_7 + amount_max_7 + amount_avg_14 + 
                merch_amount_mode_14 + merch_amount_max_28 + merch_amount_median_28 + 
                merch_amount_mode_28 + merch_amount_avg_28 + merch_amount_median_3 + 
                merch_amount_avg_7 + merch_amount_avg_14 + merch_amount_max_14, holdout,family=binomial)
probsholdout=predict(holdmodel,holdout,type='response')
holdoutresult=rep(0,12586)
holdoutresult[probsholdout>0.5]=1
table(holdoutresult,holdout$Fraud)

## FDR for train/test/holdout
quantile(probs,0.98)
trainpre_true=data.frame(training,probs)
trainpre_true=trainpre_true %>%
  mutate(pred_fraud=ifelse(probs>0.032,1,0))
nrow(trainpre_true[trainpre_true$pred_fraud==1 & trainpre_true$Fraud==1,])/nrow(trainpre_true[trainpre_true$Fraud==1,])

testpre_true=data.frame(testing,probstest)
testpre_true=testpre_true %>%
  mutate(pred_fraud=ifelse(probstest>0.0356,1,0))
quantile(probstest,0.98)
nrow(testpre_true[testpre_true$pred_fraud==1 & testpre_true$Fraud==1,])/nrow(testpre_true[testpre_true$Fraud==1,])

holdoutpre_true=data.frame(holdout,probsholdout)
holdoutpre_true=holdoutpre_true %>%
  mutate(pred_fraud=ifelse(probsholdout>0.2718,1,0))
quantile(probsholdout,0.98)
nrow(holdoutpre_true[holdoutpre_true$pred_fraud==1 & holdoutpre_true$Fraud==1,])/nrow(holdoutpre_true[holdoutpre_true$Fraud==1,])
