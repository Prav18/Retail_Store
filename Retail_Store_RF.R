#========================Retail Store Project==================================
#---------------------------Random Forest Model--------------------------------

# Read Data
s_train <- read.csv("store_train.csv")
s_train <- read.csv("store_train.csv")

library(dplyr)

glimpse(s_train)
glimpse(s_train)


# combine our two datasets so that we do not need to prepare data separately for them
s_train$store <- NA
s_train$data <- "train"
s_train$data <- "train"
s <- rbind(s_train,s_train)

glimpse(s)

# Converting Target Variable as factor
s$store=as.factor(s$store)

glimpse(s)



#look at our categorical variables by writing following lines of codes
names(s)[sapply(s,function(x) is.character(x))]

# Now we will check for High-Cardinality in the categorical variables

length(unique(s$countyname))
length(unique(s$storecode))
length(unique(s$Areaname))
length(unique(s$countytownname))
length(unique(s$state_alpha))
length(unique(s$store_Type))

# ignore columns or variables like countyname,storecode,Areaname,countytownname 
# for their High-Cardinality. 

s=s %>% select(-countyname,-storecode,-Areaname,-countytownname)

glimpse(s)

# THe varialbes country and State are numeric codes we remove them too.

s <- s %>% 
  select(-c(country, State))

lapply(s,function(x) sum(is.na(x)))

# From above we can see that We do have missing values in columns like 
# population & store. Next we impute those missing values with the mean of train 
# data as shown below.

for(col in names(s)){
  if(sum(is.na(s[,col]))>0 & !(col %in% c("data","store"))){
    s[is.na(s[,col]),col]=mean(s[s$data=='train',col],na.rm=T)
  }
}


# We can always cross check if those NAs has been replaced with mean or not by 
# using lapply function again.

lapply(s,function(x) sum(is.na(x)))

# Now we are done with data preparation , lets separate the data next.

s_train <- s %>% 
  filter(data=="train") %>% 
  select(-data)


s_train <- s %>% 
  filter(data=="train") %>% 
  select(-data,-store)


set.seed(222)
s <- sample(1:nrow(s_train),0.8*nrow(s_train))
s_train1 <- s_train[s,]
s_train2 <- s_train[-s,]

# Prepropessing of training data for modeling of numeric variables

# For sale0
hist(s_train1$sales0)
library(psych)
skew(s_train1$sales0)
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

# Model Building 
library(randomForest)
set.seed(468)
model_rf <- randomForest(store~.-Id-store_Type-CouSub,data=s_train1)

model_rf

# Model Validation 
#   
# Lets see performance of this model on the validation data s_train2 that we 
# kept aside.
# for Validation data preperation

s_train2$log_sales0 <- log(s_train2$sales0)
s_train2$log_sales1 <- log(s_train2$sales1)
s_train2$log_sales2 <- log(s_train2$sales2)
s_train2$log_sales3 <- log(s_train2$sales3)
s_train2$log_sales4 <- log(s_train2$sales4)

s_train2 <- s_train2 %>% 
  select(-c(sales0:sales4))

val_score <- predict(model_rf,newdata=s_train2,type='response')

library(caret)

confusionMatrix(val_score,s_train2$store, positive = "1")


val_prob_score=predict(model_rf,newdata=s_train2,type='prob')

# In order to check the performance of our model let us calculate its auc score. 
# For that we need to first import a package named ‘pROC’.


library(pROC)

auc_score=auc(roc(s_train2$store,val_prob_score[,1]))

auc_score


plot(roc(s_train2$store,val_prob_score[,1]))

# Final model with complete training data

# for train data preperation

s_train$log_sales0 <- log(s_train$sales0)
s_train$log_sales1 <- log(s_train$sales1)
s_train$log_sales2 <- log(s_train$sales2)
s_train$log_sales3 <- log(s_train$sales3)
s_train$log_sales4 <- log(s_train$sales4)

s_train <- s_train %>% 
  select(-c(sales0:sales4))

set.seed(123)

model_rf_final <- randomForest(store~.-Id-store_Type-CouSub,data=s_train)

model_rf_final
# Prediction for test data set
s_test$log_sales0 <- log(s_test$sales0)
s_test$log_sales1 <- log(s_test$sales1)
s_test$log_sales2 <- log(s_test$sales2)
s_test$log_sales3 <- log(s_test$sales3)
s_test$log_sales4 <- log(s_test$sales4)

s_test <- s_test %>% 
  select(-c(sales0:sales4))

test_score=predict(model_rf_final,newdata = s_train,type='prob')[,1]

test_score

# Submission of result

d=importance(model_rf_final)
d=as.data.frame(d)
d$VariableNames=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

varImpPlot(model_rf_final)


