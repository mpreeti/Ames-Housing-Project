train<-read.csv(file.choose(),header=TRUE)
test<-read.csv(file.choose(),header=TRUE)

#column names 81 & 80
names(train)
names(test)

#var n obs [1460,81/1459,80]
dim(train)
dim(test)

#deleting id var from train n test
train$Id<-NULL
train$Id
#saved id in label for future use
test_label<-test$Id
test$Id<-NULL


#chking datatypes of var[1460,80/1459,79]
str(train) #int and factor level checked
str(test)

#data summary like min,max,median,mean NA's
summary(train) #checking NA's and cat,num var
summary(test) 

#merging train n test to treat missing val

test$SalePrice<-NA
names(test)
test$SalePrice

all<-rbind(train,test)
dim(all)
str(all)
summary(all)

#boxplot of target var--outliers exist
boxplot(train$SalePrice)

#Studying target var
hist(train$SalePrice)#right skewed
summary(train$SalePrice)
quantile(train$SalePrice)
train$SP<-log(train$SalePrice) #removing skewness by log transformation
hist(train$SP)
#checking data normality--p val<0.05 so data is not normal
shapiro.test(train$SalePrice) #saleprice suffers skewness,kurtosis
qqnorm(train$SalePrice,col="red")
qqline(train$SalePrice,col="blue")

#dropping var hvng NA more than 50%
colSums(is.na(all))
all_drop<-all[,-which(colMeans(is.na(all))>0.5)]
colSums(is.na(all_drop))

#categorical var-2919/39
char<-all_drop[,sapply(all_drop,is.factor)]
str(char)
colSums(is.na(char))
dim(char)

#numeric var-2919/37
num<-all_drop[,!sapply(all_drop,is.factor)]
str(num)
colSums(is.na(num))
dim(num)


#filling cat missing val
helperFunc <- function(x){sample(levels(x), sum(is.na(x)), replace = TRUE,
         prob = as.numeric(table(x))/sum(!is.na(x)))   
}

char[sapply(char, is.na)] <- unlist(sapply(char, helperFunc))

colSums(is.na(char))
summary(char)
#The helper function(new_char) is doing 
#as.numeric(table(x)) is just the frequencies of all the unique values in x and I divide it 
#by sum(!is.na(x)) which is the length of x without NAs. This way a vector of probabilities i
#s created

#filling num missing values
library(Hmisc)
num$GarageArea <- with(num, impute(GarageArea, median))
num$GarageCars<-with(num,impute(GarageCars,median))
num$LotFrontage<-with(num,impute(LotFrontage,median))

num$MasVnrArea<-with(num,impute(MasVnrArea,mean))
num$BsmtFinSF1<-with(num,impute(BsmtFinSF1,mean))
num$BsmtFinSF2<-with(num,impute(BsmtFinSF2,mean))

num$BsmtFullBath<-with(num,impute(BsmtFullBath,mean))
num$BsmtHalfBath<-with(num,impute(BsmtHalfBath,mean))

num$GarageYrBlt<-with(num,impute(GarageYrBlt,median))
num$BsmtUnfSF<-with(num,impute(BsmtUnfSF,median))
num$TotalBsmtSF<-with(num,impute(TotalBsmtSF,median))

summary(num)
str(num)

library(VIM)
aggr_plot <- aggr(num, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(num), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

data_ames<-cbind(num,char2)
summary(data_ames)
dim(data_ames)

## 2919,76

train1= data_ames [1:1460,]
test1=data_ames [1461:2919,]

str(train1)
test1

#converting char to num
must_convert<-sapply(char,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
char2<-sapply(char[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric


summary(char2)

#create dummy var
#install.packages("dummies")
#library(dummies)
#new_char<-dummy.data.frame(char,names=c("MSZoning","Street","LotShape","LandContour","Utilities",
#                                       "LotConfig","LandSlope","Neighborhood","Condition1","Condition2",
#                                        "BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st",
#                                        "Exterior2nd","MasVnrType","ExternalQual","ExterCond","Foundaton",
#                                        "BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
#                                        "Heating",'HeatingQC','CentralAir','Electrical','KitchenQual',
#                                        'Functional','FireplaceQu','Garagetype','GarageFinish','GarageQual',
#                                        'GarageCond','PavedDrive','SaleType','SaleCondition'))

#str(new_char)
#summary(new_char)

str(test1$SalePrice)

test2<-test1[-c(37)]
#test1$SalePrice<-NULL
str(test2)
train$Sp<-train$SalePrice
train2<-train1[-c(37)]

## train2 & test2- do not contain SalePrice variable

#PCA
pca.train<-train2
pca.train

#scaling gthe data as mean=0 and sd==1
scaled.data<-scale(pca.train)
colMeans(scaled.data)


prin_comp<-prcomp(pca.train,scale.=T)
names(prin_comp)
prin_comp$center
prin_comp$sdev
prin_comp$rotation

dim(prin_comp$x)

biplot(prin_comp,scale=0)

std_dev<-prin_comp$sdev
pr_var<-std_dev^2
pr_var[1:10]

prop_varex<-pr_var/sum(pr_var)
prop_varex[1:20]

plot(prop_varex,xlab = "Principal Compnent",ylab = "Proportion of variance explained",type = 'b')

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


#PCA
pca.test<-test2
str(pca.test)

#scaling gthe data as mean=0 and sd==1
scaled.data<-scale(pca.test)
colMeans(scaled.data)


prin_comp<-prcomp(pca.test)
names(prin_comp)
prin_comp$center
prin_comp$sdev
prin_comp$rotation

dim(prin_comp$x)

biplot(prin_comp,scale=0)
warnings()
std_dev<-prin_comp$sdev
pr_var<-std_dev^2
pr_var[1:10]

prop_varex<-pr_var/sum(pr_var)
prop_varex[1:20]

plot(prop_varex,xlab = "Principal Compnent",ylab = "Proportion of variance explained",type = 'b')

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# we use eigan vector as backend process in R, it is siilar to clusterisation,
#PCR is similar to MLR
# PCA is used in pattern recognition problems

pc1<-princomp(formula=~.,data=train2,cor=T)
summary(pc1)

#display coefficients of linear combinarions
# loadings will tell you which of the variables are most important

pc1$loadings
biplot(pc1)

## this biplot will show the maximum explanation by 1st 2 components.

# storescore of first PC in variable called "performance"

score01<-pc1$scores
pc1$performance<-pc1$scores[,1]
head(pc1)

# verify that principle components are uncorrelated which means information-in each PC is unique

round(cor(pc1$scores))

sort(pc1$scores,decreasing= F)


## lets work on skewed data

## we use to used the following code 
### to identify any variables with a skewness greater than 0.75

library(e1071)
column_classes = sapply(names(train1), function(x){class(train1[[x]])})
numeric_columns = names(column_classes[column_classes == "integer"])
skew = sapply(numeric_columns, function(x){skewness(train1[[x]], na.rm = T)})
skew = skew[skew > 0.75]


train_trim_log = train1
for(x in names(skew)) 
{
  train_trim_log[[x]] = log(train_trim_log[[x]] + 1)
  
}
# do the same for the test set
test_logtrnsfrm = test1
for(x in names(skew)) 
{
  if(x != "SalePrice")
  {
    test_logtrnsfrm[[x]] = log(test_logtrnsfrm[[x]] + 1)
  }
}



## Model- Basic Random Forest[rmse=29161.26,rsquared=0.8762633/rmse=1410233,rsquared=0.8869253]
library(caret)
install.packages("ranger")
library(ranger)

set.seed(45)
myControl = trainControl(method = "cv", number = 5, verboseIter = FALSE)
model_rf = train(SalePrice ~ ., 
                 data = train_trim_log,
                 tuneLength = 1,
                 method = "ranger",
                 importance = 'impurity',
                 trControl = myControl)
model_rf


## Model2-linear regression [rmse=34326.67,rsquared=0.8150471/rmse=1419324,rsquared=0.8754644]

model_lm = train(SalePrice ~ ., 
                 data = train_trim_log,
                 method = "lm",
                 trControl = myControl)
model_lm



##compared the performance of both models 
##### root mean square error (RMSE) as my performance measure.
####use of the resamples function ensures that the same training data is used in both models and hence a like-for-like comparison is being made between them

model_list <- list(lm = model_lm, rf = model_rf)
resamples = resamples(model_list)
summary(resamples)


##Box plot diagrams are generated for ease of comparison



bwplot(resamples, metric = "RMSE")
rm(resamples, model_list)

## this box plot illustrates the random forest model-91%
###orms much better than the linear regression model_87%



### Model 3 - random forest with two mtry values[rmse=34297,rsquared=0.8526475]
### One way to improve the performance of random forest models
###to experiment with their tuning parameters. 

model_rf2 = train(SalePrice ~ ., 
                  data = train_trim_log,
                  tuneLength = 2,
                  method = "ranger",
                  importance = 'impurity',
                  trControl = myControl)


model_rf2

## lets compare the model with the more basic random forest model produced earlier.

model_list <- list(rf = model_rf, rf2 = model_rf2)
resamples = resamples(model_list)
summary(resamples)



## rmse= RF 33155,RF2 34277
##rsquared= RF 0.9112024,RF2 0.8886548

## rmse= RF= 0.1570633,RF2 =0.1485248
##rsquared= RF=0.9056803 ,RF2 =0.9077767


bwplot(resamples, metric = "RMSE")

## As the box plot illustrates the second random forest model does
### indeed perform slightly better than the first


### Model 4- stochastic gradient boosting machine [rmse=34836,rsquared=0.8225675]
## rmse= 0.1377929, rsquared=0.8819581 

model_gbm = train(SalePrice ~ ., 
                  data = train_trim_log,
                  tuneLength = 2,
                  method = "gbm",
                  trControl = myControl)


model_gbm


## Model 5 - linear model with a trimmed training set


##firstly plotting the ground floor living area against the sale price
## rmse 25649, rsquared=88.94126

library(dplyr)

ggplot(train ,aes(y = SalePrice, x = GrLivArea)) + geom_point()

## This plot presented four outliers where the ground floor living area was more than 4000 square feet
# therefore removed such records from the training set

train_trim = filter(train1, GrLivArea<= 4000)
head(train_trim)

## lets built a linear regression model using this trimmed training 
## and assessed its performance in the usual way

model_lm2 = train(SalePrice ~ ., 
                  data = train_trim,
                  method = "lm",
                  trControl = myControl)

model_lm2

## Model 6 - regularised linear regression(combination of lasso regression & and ridge regression)
## rmse= 36057, rsquared=80.05751
## best score

## train_trim- rmse=25527, rsquared=89.02973

glmnetTuningGrid = expand.grid(alpha = seq(0, 1, 0.2),
                               lambda = seq(0, 1, 0.2))
model_glmnet1 = train(SalePrice ~ ., 
                      data = train_trim_log,
                      method = "glmnet",
                      trControl = myControl,
                      tuneGrid = glmnetTuningGrid)

model_glmnet1


### lets see combined effect- best performing model****

model_list = list(lm = model_lm, gbm = model_gbm, glmnet1 = model_glmnet1)
resamples = resamples(model_list)
summary(resamples)

###NOTE THE RESULT
#          RMSE        RSQUARED
#lm        46064.84    0.8753197
#gbm      35623.48    0.8955521
#glmnet1   47160.62    0.8728985

##          RMSE        RSQUARED
#lm        0.1793713    0.9105029
#gbm      0.1461482    0.9057996
#glmnet1   0.1749087    0.9202067

### Model 7 - support vector machine

library(e1071)
library(Hmisc)

## now we add model like GLM to start SVM

model_svm<-svm(SalePrice~.,
               data= train_trim_log,
               type="C",
              probability=TRUE,
        kernel="linear")
model_svm



### Model 8 - regularised linear regression using trimmed and 
## log-transformed training set--rmse= 0.1416872, rsquared=0.8747737


glmnetTuningGrid = expand.grid(alpha = seq(0, 1, 0.2),
                               lambda = seq(0, 1, 0.2))
model_glmnet1 = train(SalePrice ~ ., 
                      data = train_trim_log,
                      method = "glmnet",
                      trControl = myControl,
                      tuneGrid = glmnetTuningGrid)

model_glmnet1

##

model_list = list(lm2 = model_lm2, glmnet1 = model_glmnet1)
resamples = resamples(model_list)
summary(resamples)


### Model 9 - PCA and linear model

nearZeroVar(train_trim, saveMetrics = T) #variables with zero or near-zero variance 

out = c("Id", "Street", "Alley", "LandContour", "Utilities", "LandSlope", "Condition2", 
        "RoofMatl", "BsmtCond", "BsmtFinType2", "BsmtFinSF2", "Heating", "LowQualFinSF", 
        "KitchenAbvGr", "Functional", "EnclosedPorch", "x3SsnPorch", "PoolArea", 
        "PoolQC", "MiscFeature", "MiscValue")
train_trim_log2 = train_trim_log[, !(names(train_trim_log) %in% out)]

## Given that PCA only works on numeric variables 
## we also removed any non-numeric variables from the training set.


train_trim_log2_numeric = train_trim_log2[, names(train_trim_log2) %in% numeric_columns]


## we conducted PCA on this training set - returning the top 15 principal components.

pca = prcomp(train_trim_log2_numeric, scale = T, center = T)
eigenvalues = factoextra::get_eigenvalue(pca)
pcaVar = factoextra::get_pca_var(pca) #PCA variables
pcaVar = as.data.frame(c(pcaVar))[,1:15] #table of correlations between variables and top 15 principal components

## To identify which features to combine we produced a table of 
## strongly (positively or negatively) correlated features.

var = pcaVar[FALSE, ]
k = 1
for(i in colnames(pcaVar))
{
  for(j in rownames(pcaVar))
  {
    if(abs(pcaVar[j , i]) >= 0.5) #strong +ve/-ve correlation between variables j and i
    {
      var[k, i] = j
      k = k + 1
    }
  }
  k = 1
}
var = as.data.frame(var)


#Create new feature called TotQual - product of Overallqual and Overallcond
train_newfeatures = mutate(train_trim_log2, TotQual = (train_trim_log2$OverallCond)*(train_trim_log2$OverallQual))


## Linear regression - rmse=0.1479954, rsquared=0.8648377-log transformed data

model_lm3 = train(SalePrice ~ ., 
                  data = train_trim_log,
                  method = "lm",
                  trControl = myControl)

model_lm3

## Linear regression - rmse=0.1401691, rsquared=0.8794683- trimmed data

model_lm4 = train(SalePrice ~ ., 
                  data = train_trim_log2,
                  method = "lm",
                  trControl = myControl)

model_lm4


##

model_list = list(lm3 = model_lm3, lm4 = model_lm4)
resamples = resamples(model_list)
summary(resamples)


##     Rmse      r squared
# lm3 0.2125006  0.9187570
# lm4 0.1679893  0.8984563



model_list = list(glmnet1 = model_glmnet1, lm4 = model_lm4)
resamples = resamples(model_list)
summary(resamples)


##     Rmse      r squared
# Glm1 0.1717080  0.9061420
# lm4 0.1679893  0.8984563





















































































