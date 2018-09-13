library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
data=read_xlsx('card transactions.xlsx')
summary(data)

  

# recordnum: 1-95007

# cardnum
cardnum=data %>%
  group_by(Cardnum) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20)
ggplot(cardnum,aes(x=reorder(Cardnum,count),y=count))+
  geom_bar(stat = 'identity')+
  labs(title='Distribution of Top 20 Card Number',x='Card Number')+
  theme(text = element_text(size=15),
         plot.title = element_text(hjust = 0.5))+
  coord_flip()

# date
## frequency
data %>%
  group_by(Date) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(Date,count),y=count))+
    geom_bar(stat = 'identity')+
    labs(title='Distributin of Top 20 Date',x='Date')+
    theme(text = element_text(size=15),
         plot.title = element_text(hjust = 0.5))+
    coord_flip()

## monthly payments
monthdate=data %>%
  mutate(month=months(Date)) %>%
  group_by(month) %>%
  summarize(count=n())
monthdate=monthdate[order(match(monthdate$month,month.name)),]

  ggplot(monthdate,aes(x=month,y=count)) +
  geom_bar(stat='identity')+
  labs(title='Monthly Transaction Payments',x='Month')+
  theme( axis.text.x = element_text(angle = 45, hjust = 1),
         plot.title = element_text(hjust = 0.5),
         text = element_text(size=15))

## weekly payments
  weekdaydate=data %>%
    mutate(weekday=weekdays(Date)) %>%
    group_by(weekday) %>%
    summarize(count=n())
  weekdaydate$weekday=factor(weekdaydate$weekday,levels=c("Sunday", "Monday", 
                                                     "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(weekdaydate,aes(x=weekday,y=count))+
  geom_bar(stat = 'identity')+
  labs(title='Transaction Payments by Day of Week',x='Day of Week')+
  theme(
         plot.title = element_text(hjust = 0.5),
         text = element_text(size=15))

weekdate=data %>%
  group_by(week=cut(Date,'week')) %>%
  summarize(count=n())
weekdate$week=as.POSIXct(as.character(weekdate$week),format="%Y-%m-%d")
mode(weekdate$week)
ggplot(weekdate,aes(x=week,y=count,group=1))+
  geom_line()+
  labs(title='Weekly Transaction Payments',x='Date')+
  theme( 
         plot.title = element_text(hjust = 0.5),
         text = element_text(size=15))

## daily payments
data %>%
  group_by(Date) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=Date,y=count,group=1))+
  geom_line()+
  labs(title='Daily Transaction Payments',x='Date')+
  theme( 
    plot.title = element_text(hjust = 0.5),
    text = element_text(size=15))


# merchnum
data %>%
  filter(!is.na(Merchantnum)) %>%
  group_by(Merchantnum) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(Merchantnum,count),y=count))+
  geom_bar(stat = 'identity')+
  labs(title='Distribution of Top 20 Merchant Number(NA excluded)',x='Merchant Number')+
  theme(
          text = element_text(size=14))+
  coord_flip()+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))
                     

# merch.description
data %>%
  group_by(`Merch Description`) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(`Merch Description`,count),y=count))+
  geom_bar(stat = 'identity')+
  labs(title='Distribution of Top 20 Merchant Description',x='Merchant Description')+
  theme(
        text = element_text(size=15))+
  coord_flip()

# merch.state
data %>%
  filter(!is.na(`Merchant State`)) %>%
  group_by(`Merchant State`) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(`Merchant State`,count),y=count))+
  geom_bar(stat = 'identity')+
  labs(title='Distribution of Top 20 Merchant State(NA excluded)',x='Merchant Number')+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=14))+
  coord_flip()

# merch.zip
data %>%
  filter(!is.na(`Merchant Zip`)) %>%
  group_by(`Merchant Zip`) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(`Merchant Zip`,count),y=count))+
  geom_bar(stat = 'identity')+
  labs(title='Distribution of Top 20 Merchant Zip(NA excluded)',x='Merchant Zip')+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=14))+
  coord_flip()

#transtype
data %>%
  filter(!is.na(Transtype)) %>%
  group_by(Transtype) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  ggplot(aes(x=reorder(Transtype,count),y=count))+
  geom_bar(stat = 'identity')+
  labs(title='Distribution of Transtype',x='Transtype')+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=14))+
  coord_flip()

# amount
data %>%
  arrange(-Amount) %>%
  select(Amount) %>%
  head(5)
ggplot(data,aes(x=Amount))+
  geom_histogram(bins=50)+
  scale_x_continuous(limits = c(0,50))+
  labs(title='Distribution of Amount',x='Amount')+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=14))
ggplot(data,aes(x='Amount',y=Amount))+
  geom_boxplot()+
  labs(title='Boxplot Distribution of Amount',x='',y='')+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=14))
  
ggplot(data,aes(x=log(Amount)))+
  geom_histogram(bins=50)+
  scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
  labs(title='Distribution of log(Amount)',x='log(Amount)')+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=14))

# fraud
data %>%
  group_by(Fraud) %>%
  summarise(prop=n()/96708,count=n())


