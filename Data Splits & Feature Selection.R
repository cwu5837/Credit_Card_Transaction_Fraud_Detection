setwd("~/Documents/fraud/project 3")
library(dplyr)
final_data<-read.csv("final_data.csv")
model_data<-final_data%>%
  filter(Recordnum<=84095)
oot_data<-final_data%>%
  filter(Recordnum>84095)
set.seed(1)
ii<-seq(1,83766,1)
training<-sample(ii,0.8*83766)
train_data<-model_data[training,]
test_data<-model_data[-training,]

# write.csv(oot_data,"oot_data.csv")
# write.csv(train_data,"train_data.csv")
# write.csv(test_data,"test_data.csv")


t(final_data%>%
  select(everything())%>%
  summarise_all(funs(sum(is.na(.)))))


# KS feature selection on training data
data_1=train_data%>%
  filter(Fraud==1)

data_0=train_data%>%
  filter(Fraud==0)

result=numeric()

for(i in 3:105){
  test=(ks.test(data_0[,i],data_1[,i])$statistic)*100
  result=c(result,test)
}

names=colnames(train_data[3:105]) #extract all variables 

df = data.frame(names,result) #combine results and variables

ks_scores<-df%>%
  arrange(-result)%>%
  slice(1:63)


vars<-as.character(ks_scores$names)
training_data=train_data%>%
  select(2,vars)
testing_data=test_data%>%
  select(2,vars)

# write.csv(training_data,"training_data.csv")
# write.csv(testing_data,"testing_data.csv")
# write.csv(ks_scores,"ks_socres.csv")
# write.csv(df, "variable description.csv")
