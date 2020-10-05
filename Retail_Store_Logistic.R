#==================================Retail Store Project=======================
#---------------------------Logistic Regression Model-------------------------

# Read Data
s_train=read.csv("store_train.csv")
s_test=read.csv("store_test.csv")

str(s_train)

# Combine Data Sets:
s_test$store=NA
s_train$data='train'
s_test$data='test'
s_all=rbind(s_train,s_test)

library(dplyr)
glimpse(s_all)



##if you look at variables storecode ,statealpha, country, state you will notice
# that it has lot more unique values,having none of the individual values 
# having frequency higher than a good number , we'll drop these variables
length(unique(s_all$countyname))
length(unique(s_all$storecode))
length(unique(s_all$Areaname))
length(unique(s_all$countytownname))
length(unique(s_all$state_alpha))
length(unique(s_all$store_Type))

# We will ignore columns or variables like countyname,storecode,Areaname,
# countytownname for their High-Cardinality. Further we will ignore data column 
# for obvious reason.

s_all <- s_all %>% 
  select(-c(countyname,storecode,Areaname,countytownname))

# We also remove country and State because these are numeric codes only.

s_all <- s_all %>% 
  select(-c(country, State))
  

# Now we create the dummies for character variables
char_logical=sapply(s_all,is.character)
cat_cols=names(s_all)[char_logical]
cat_cols=cat_cols[!(cat_cols %in% c('data'))]
cat_cols

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

for(col in cat_cols){
  s_all=CreateDummies(s_all,col,50)
}


# Remove NAs
sum(is.na(s_all))

s_all=s_all[!((is.na(s_all$store)) & s_all$data=='train'), ]

for(col in names(s_all)){
  if(sum(is.na(s_all[,col]))>0 & !(col %in% c("data","store"))){
    s_all[is.na(s_all[,col]),col]=mean(s_all[s_all$data=='train',col],na.rm=T)
  }
}

# Divide the data in train and test

s_train=s_all %>% filter(data=='train') %>% select(-data)
s_test=s_all %>% filter(data=='test') %>% select(-data,-store)

# Partition the data in training and validation out of s_train

set.seed(123)
s=sample(1:nrow(s_train),0.8*nrow(s_train))
s_train1=s_train[s,]
s_train2=s_train[-s,]

# Univariate Analysis:

names(s_train1)
str(s_train1)

# Prepropessing of training data for modeling of numeric variables

# For sale0
hist(s_train1$sales0)
library(psych)
skew(s_train2$sales0)
s_train1$log_sales0 <- log(s_train1$sales0)
skew(log(s_train1$sales0))
hist(s_train1$log_sales0)
quantile(s_train1$log_sales0)
ul_log_sales0 <- 6.82 + 1.5*(6.82 - 6.47)
sum(s_train1$log_sales0 > ul_log_sales0)
s_train1$log_sales0 <- ifelse(s_train1$log_sales0 > ul_log_sales0, ul_log_sales0, s_train1$log_sales0)
boxplot(s_train1$log_sales0)

# For sales1
hist(s_train1$sales1)
skew(s_train1$sales1)
s_train1$log_sales1 <- log(s_train1$sales1)
skew(s_train1$log_sales1)
hist(s_train1$log_sales1)
quantile(s_train1$log_sales1)
ul_log_sales1 <- 6.47 + 1.5*(6.47 - 6.12)
sum(s_train1$log_sales1 > ul_log_sales1)
s_train1$log_sales1 <- ifelse(s_train1$log_sales1 > ul_log_sales1, ul_log_sales1, s_train1$log_sales1)
boxplot(s_train1$log_sales1)


# For sales2
hist(s_train1$sales2)
skew(s_train1$sales2)
s_train1$log_sales2 <- log(s_train1$sales2)
skew(s_train1$log_sales2)
hist(s_train1$log_sales2)
quantile(s_train1$log_sales2)
ul_log_sales2 <- 6.6 + 1.5*(6.6 - 6.22)
sum(s_train1$log_sales2 > ul_log_sales2)
s_train1$log_sales2 <- ifelse(s_train1$log_sales2 > ul_log_sales2, ul_log_sales2, s_train1$log_sales2)
boxplot(s_train1$log_sales2)

# For sales3
hist(s_train1$sales3)
skew(s_train1$sales3)
s_train1$log_sales3 <- log(s_train1$sales3)
skew(s_train1$log_sales3)
hist(s_train1$log_sales3)
quantile(s_train1$log_sales3)
ul_log_sales3 <- 7.09 + 1.5*(7.09 - 6.76)
sum(s_train1$log_sales3 > ul_log_sales3)
s_train1$log_sales3 <- ifelse(s_train1$log_sales3 > ul_log_sales3, ul_log_sales3, s_train1$log_sales3)
boxplot(s_train1$log_sales3)

# For sales4
hist(s_train1$sales4)
skew(s_train1$sales4)
s_train1$log_sales4 <- log(s_train1$sales4)
skew(s_train1$log_sales4)
hist(s_train1$log_sales4)
quantile(s_train1$log_sales4)
ul_log_sales4 <- 7.235 + 1.5*(7.25 - 6.86)
sum(s_train1$log_sales4 > ul_log_sales4)
s_train1$log_sales4 <- ifelse(s_train1$log_sales4 > ul_log_sales4, ul_log_sales4, s_train1$log_sales4)
boxplot(s_train1$log_sales4)


# Remove original sales columns
s_train1 <- s_train1 %>% 
  select(-c(sales0:sales4))

#Lets remove vars which have redundant information first on the basis of AIC by
# Step function

for_step=lm(store~.-Id,data=s_train1)
step(for_step)

# Now remove redundant variables on the basis of VIF

for_vif=lm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
             state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
             state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
             state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
             state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC + 
             state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
             state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
             state_alpha_NH + state_alpha_MA + log_sales0 + log_sales1 + 
             log_sales2 + log_sales4, data = s_train1)

library(car)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
             state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
             state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
             state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
             state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC + 
             state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
             state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
             state_alpha_NH + state_alpha_MA + log_sales1 + 
             log_sales2 + log_sales4, data = s_train1)

sort(vif(for_vif),decreasing = T)[1:3]


for_vif=lm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
             state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
             state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
             state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
             state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC + 
             state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
             state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
             state_alpha_NH + state_alpha_MA + log_sales1 + 
             log_sales4, data = s_train1)

sort(vif(for_vif),decreasing = T)[1:3]


for_vif=lm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
             state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
             state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
             state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
             state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC + 
             state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
             state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
             state_alpha_NH + state_alpha_MA + 
             log_sales4, data = s_train1)

sort(vif(for_vif),decreasing = T)[1:3]


# Now apply glm model and remove the insignificant variables


fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
          state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA + state_alpha_NC + 
          state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
          state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)


fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_SD + state_alpha_AL + state_alpha_FL + 
          state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
          state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)


fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_AL + state_alpha_FL + 
          state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
          state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  state_alpha_FL + 
          state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
          state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_MI + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
          state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
          state_alpha_VA + state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_KS + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_PA + state_alpha_WI + state_alpha_AR + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ CouSub + population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_PA + state_alpha_WI + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_PA + state_alpha_WI + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_PA + state_alpha_OK + 
          state_alpha_PR + state_alpha_MS + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_PA + state_alpha_OK + 
          state_alpha_PR + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_OK + 
          state_alpha_PR + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_TX + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_OK + 
          state_alpha_PR + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_OK + 
          state_alpha_PR +  
          state_alpha_IN + state_alpha_TN + state_alpha_IA +  
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA +  
          state_alpha_OK + 
          state_alpha_PR +  
          state_alpha_IN + state_alpha_TN + 
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_PR +  
          state_alpha_IN + state_alpha_TN + 
          state_alpha_IL + state_alpha_MO + state_alpha_KY + 
          state_alpha_GA + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_PR +  
          state_alpha_IN + state_alpha_TN + 
          state_alpha_IL + state_alpha_KY + 
          state_alpha_GA + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)


fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_PR +  
          state_alpha_IN + state_alpha_TN + 
          state_alpha_KY + 
          state_alpha_GA + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)
fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_PR +  
          state_alpha_IN + state_alpha_TN + 
          state_alpha_GA + state_alpha_VT + 
          state_alpha_NH + state_alpha_MA + 
          log_sales4, data = s_train1, family = "binomial")

summary(fit)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_PR + state_alpha_IN + 
          state_alpha_TN + state_alpha_VT + state_alpha_NH + 
          state_alpha_MA + log_sales4, data = s_train1, 
        family = "binomial")

summary(fit)


s_train1$score=predict(fit,newdata=s_train1,type = "response")
head(s_train1$store)
head(s_train1$score)

library(ggplot2)
ggplot(s_train1,aes(y=store,x=score,color=factor(store)))+
  geom_jitter()

cutoff=0.2
predicted=as.numeric(s_train1$score>cutoff)
TP=sum(predicted==1 & s_train1$store==1)
FP=sum(predicted==1 & s_train1$store==0)
FN=sum(predicted==0 & s_train1$store==1)
TN=sum(predicted==0 & s_train1$store==0)
head(predicted)
head(s_train1$store)
# lets also calculate total number of real positives and negatives in the data
P=TP+FN
N=TN+FP

# total number of observations
total=P+N

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)

for (cutoff in cutoffs){
  predicted=as.numeric(s_train1$score>cutoff)
  TP=sum(predicted==1 & s_train1$store==1)
  FP=sum(predicted==1 & s_train1$store==0)
  FN=sum(predicted==0 & s_train1$store==1)
  TN=sum(predicted==0 & s_train1$store==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

head(cutoff_data)
# lets remove the dummy data containing top row
cutoff_data=cutoff_data[-1,]
View(cutoff_data)
## ------------------------------------------------------------------------
cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP) %>% 
  mutate(Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2)) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N))

library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,Accuracy) %>%
  gather(Criterion,Value,c(Sn,Sp,Accuracy))

ggplot(filter(cutoff_viz),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()

# for Validation data preperation

s_train2$log_sales0 <- log(s_train2$sales0)
s_train2$log_sales1 <- log(s_train2$sales1)
s_train2$log_sales2 <- log(s_train2$sales2)
s_train2$log_sales3 <- log(s_train2$sales3)
s_train2$log_sales4 <- log(s_train2$sales4)

s_train2 <- s_train2 %>% 
  select(-c(sales0:sales4))


s_train2$score=predict(fit,newdata = s_train2,type = "response")

library(caret)
s_train2$pred_test <- as.numeric(s_train2$score>.39)
s_train2$pred_test <- as.factor(s_train2$pred_test)
s_train2$store <- as.factor(s_train2$store)
confusionMatrix(s_train2$pred_test,s_train2$store, positive = "1")


library(pROC)

auc_score=auc(roc(s_train2$store, s_train2$score))

auc_score


plot(roc(s_train2$store,s_train2$score))


# Making Final MOdel with complete training data

s_train$log_sales4 <- log(s_train$sales4)

fit=glm(store ~ population + state_alpha_WV + state_alpha_CO + 
          state_alpha_LA + state_alpha_PR + state_alpha_IN + 
          state_alpha_TN + state_alpha_VT + state_alpha_NH + 
          state_alpha_MA + log_sales4, data = s_train, 
        family = "binomial")

summary(fit)

# Preprocessing of test data

s_test$log_sales4 <- log(s_test$sales4)
# Prediction with test data on final fit model
s_test$score=predict(fit,newdata = s_test,type = "response")

