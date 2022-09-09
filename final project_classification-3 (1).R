
## Installation of the packages
install.packages("nnet")
install.packages("rpart")
install.packages("e1071")
install.packages("cvms")

#Libraries attachments
library(rpart)
library(ggplot2)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(glmnet)
library(Metrics)
library(caret)
library(fastDummies)
library(stringr)
library(nnet)
library(e1071)
library(cvms)

##Getting csv files from
d1=read.table("student-ma.csv",sep=";",header=TRUE)
d2=read.table("student-po.csv",sep=";",header=TRUE)
d3<-merge(d1,d2, all=TRUE) 

print(nrow(d3)) 

#merging csv files
colnames(d3)=c('school','sex','age','address','family_size','parents_status','mother_education','father_education',
          'mother_job','father_job','reason','guardian','commute_time','study_time','failures','school_support',
          'family_support','paid_classes','activities','nursery','desire_higher_edu',
          'internet','romantic','family_quality',
          'free_time','go_out','weekday_alcohol_usage',
          'weekend_alcohol_usage','health','absences','period1_score','period2_score','final_score')

str(d3)
##adding another feature Final Status based on the final score
d3<- d3 %>%
  mutate(final_status = case_when(
   (final_score >=15 & final_score<=20) ~ "good",
   (final_score >=8 & final_score<=14) ~ "fair",
   (final_score >=0 & final_score<=7) ~ "poor",
  ))


sum(is.na(d3))

##numerical variables dependencies on final_score
num_cols <- unlist(lapply(d3, is.numeric))         # Identify numeric columns
num_cols
## Adding the numeric columns to the dataset
d3_num <- d3[ , num_cols]                        # Subset numeric columns of data
str(d3_num)
cors<-cor(d3_num,use="pairwise")
par(mfrow=c(1,1))
##correlation plot of students numerics (motheredu, father education, failures and study time)
corrplot(cors,type="upper",col=brewer.pal(n=8,name="RdYlBu"),method="pie", 
         title="Corrplot of Student data", mar=c(0,0,1,0))

##Final Status histogram
ggplot(data = d3, aes(x = final_status)) +
  geom_histogram(bins = 12, stat = "count")+labs(x="Final Status ",y="No of Students", 
        title = "Grade Distribution")+theme(
          plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
          legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
          legend.text = element_text(color = "black"),
          legend.position="bottom",
          axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
          axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
          legend.box.background = element_rect(color="cadetblue", size=1),
          legend.box.margin = margin(1, 1),
          legend.key = element_rect(fill = "white", colour = "cadetblue"))



## Score based on the mothers_job
ggplot(d3, aes(mother_job, fill=final_status,color=final_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position ='dodge') + 
  labs(x="Mothers job",y="Proportion of student counts", 
        title = "Effect of mothers job on grade")+theme(
          plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
          legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
          legend.text = element_text(color = "black"),
          legend.position="bottom",
          axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
          axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
          legend.box.background = element_rect(color="cadetblue", size=1),
          legend.box.margin = margin(1, 1),
          legend.key = element_rect(fill = "white", colour = "cadetblue"))

test <- chisq.test(table(d3$final_status, d3$mother_job))
test
ifelse(test$p.value > 0.05, "fail to reject null hypothesis", "Reject the null hypothesis")

## Score based on the father_job
ggplot(d3, aes(father_job, fill=final_status,color=final_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position ='dodge') + 
  labs(x="Father job",y="Proportion of students counts", 
       title = "Effect of Fathers job on grade")+theme(
         plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
         legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
         legend.text = element_text(color = "black"),
         legend.position="bottom",
         axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
         axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
         legend.box.background = element_rect(color="cadetblue", size=1),
         legend.box.margin = margin(1, 1),
         legend.key = element_rect(fill = "white", colour = "cadetblue"))

test1<- chisq.test(table(d3$final_status, d3$father_job))
test1
ifelse(test1$p.value > 0.05, "fail to reject null hypothesis", "Reject the null hypothesis")

##Desire to learn higher education
ggplot(d3, aes(desire_higher_edu, fill=final_status,color=final_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position ='dodge') + 
  labs(x="Desire to learn more",y="Proportion of the students", 
       title = "Student Desire to study more statistics")+theme(
         plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
         legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
         legend.text = element_text(color = "black"),
         legend.position="bottom",
         axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
         axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
         legend.box.background = element_rect(color="cadetblue", size=1),
         legend.box.margin = margin(1, 1),
         legend.key = element_rect(fill = "white", colour = "cadetblue"))
##Failures 
ggplot(data = d3, aes(x = failures)) +
  geom_histogram(bins = 20, stat = "count", colo='black', fill='cadetblue') +
  labs(x="No of Failures",y="Frequency", 
      title = "Student Previous Failures")+theme(
        plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
        legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
        legend.text = element_text(color = "black"),
        legend.position="bottom",
        axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
        axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
        legend.box.background = element_rect(color="cadetblue", size=1),
        legend.box.margin = margin(1, 1),
        legend.key = element_rect(fill = "white", colour = "cadetblue"))

## Romantic relationship effect on grade
ggplot(d3, aes(final_status, fill=romantic,color=romantic)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position ='dodge') + 
  labs(x="Romantic Relationship",y="Proportion of student counts", 
       title = "Romantic Relation effect on Students Grade")+theme(
         plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
         legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
         legend.text = element_text(color = "black"),
         legend.position="bottom",
         axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
         axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
         legend.box.background = element_rect(color="cadetblue", size=1),
         legend.box.margin = margin(1, 1),
         legend.key = element_rect(fill = "white", colour = "cadetblue"))

## chi square test
test <- chisq.test(table(d3$final_status, d3$romantic))
test
ifelse(test$p.value > 0.05, "fail to reject null hypothesis", "Reject the null hypothesis")





##internet influence on student
ggplot(d3, aes(final_status, fill=internet,color=internet)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position ='dodge') + 
  labs(x="Internet",y="Proportion of student counts", 
       title = "Internet effect on Students Grade")+theme(
         plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
         legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
         legend.text = element_text(color = "black"),
         legend.position="bottom",
         axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
         axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
         legend.box.background = element_rect(color="cadetblue", size=1),
         legend.box.margin = margin(1, 1),
         legend.key = element_rect(fill = "white", colour = "cadetblue"))

## chi square test
test1 <- chisq.test(table(d3$final_status, d3$internet))
test1
ifelse(test1$p.value > 0.05, "fail to reject null hypothesis", "Reject the null hypothesis")

##Activities effect on student
ggplot(d3, aes(activities, fill=final_status,color=final_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position ='dodge') + 
  labs(x="Activities",y="Proportion of student counts", 
       title = "Activities on Students Grade")+theme(
         plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
         legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
         legend.text = element_text(color = "black"),
         legend.position="bottom",
         axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
         axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
         legend.box.background = element_rect(color="cadetblue", size=1),
         legend.box.margin = margin(1, 1),
         legend.key = element_rect(fill = "white", colour = "cadetblue"))
## chi square test
test2 <- chisq.test(table(d3$final_status, d3$activities))
test2
ifelse(test2$p.value > 0.05, "fail to reject null hypothesis", "Reject the null hypothesis")


##Address effect on Student grade
ggplot(d3, aes(address, fill=final_status,color=final_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position ='dodge') + 
  labs(x="Address",y="Proportion of student counts", 
       title = "Address effect on Students Grade")+theme(
         plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
         legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
         legend.text = element_text(color = "black"),
         legend.position="bottom",
         axis.text.y = element_text( color="cadetblue", size=10,face="bold"),
         axis.text.x = element_text( color="cadetblue", size=10,face="bold"),
         legend.box.background = element_rect(color="cadetblue", size=1),
         legend.box.margin = margin(1, 1),
         legend.key = element_rect(fill = "white", colour = "cadetblue"))

## chi square test
test <- chisq.test(table(d3$final_status, d3$address))
test
ifelse(test$p.value > 0.05, "fail to reject null hypothesis", "Reject the null hypothesis")


##compare mothers and fathers education with final score
##mothers education model
medu<- lm(  d3$final_score ~ d3$mother_education)
coef(medu)
summary(medu)
AIC(medu)
BIC(medu)

##Father education
fedu<- lm(  d3$final_score ~ d3$father_education)
coef(fedu)
summary(fedu)
AIC(fedu)
BIC(fedu)



##compare mothers and fathers job with final score
##mothers job model
mjob<- lm(  d3$final_score ~ d3$mother_job)
coef(mjob)
summary(mjob)
AIC(mjob)
BIC(mjob)

##Father jon
fjob<- lm(  d3$final_score ~ d3$father_job)
coef(fjob)
summary(fjob)
AIC(fjob)
BIC(fjob)



#
cor(d3$final_score, d3$failures)
plot(x=d3$failures, y=d3$final_score, xlab = "Failures", ylab = "Rank", col = "blue")

cor(d3$age, d3$final_score)
plot(x=d3$age, y=d3$final_score, xlab = "Age", ylab = "Rank", col = "red")

#box plot of study time vs final_score
boxplot(d3$study_time ~ d3$final_score, data = d3,  xlab = "Student score", ylab = "Study Time", 
        main = "Box plot of Rank vs Study Time" ,col = "cadetblue")


##converting all the character features to factor
d3 <- subset( d3, select = -c(period1_score, period2_score))
d3 <- d3 %>% mutate_if(is.character,as.factor)


#######################Modelling###############################
set.seed(199)
library(caret)
##test train split
trainindex <-createDataPartition(d3$final_score, p = .7,
                                 list = FALSE,
                                 times = 1)
length(trainindex)
train <- d3[trainindex,]
test <- d3[-trainindex,]
trainnew <- select(train,-final_status)
###Linear model for final_score
lm.model <- lm(final_score ~ ., data = trainnew)
lm.model
lm.y <- predict.lm(lm.model, test)
lm.trainy<-predict.lm(lm.model,trainnew)
lm.trainy <- round(lm.trainy)
lm.trainy <- as.numeric(lm.trainy)
lm.trainy <- cut(lm.trainy, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
lm.trainy <- as.factor(lm.trainy)
lm.trainy
train$final_status<-as.factor(train$final_status)
regtrain <- confusionMatrix(lm.trainy, train$final_status)
regtrain


##testing data prediction
mean((lm.y - test$final_score)^2)
lm.y <- round(lm.y)
lm.y <- as.numeric(lm.y)
#converting the score to status
lm.y <- cut(lm.y, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
lm.y <- as.factor(lm.y)
lm.y
test$final_status<- as.factor(test$final_status)
#confusion matrix evaluation of final_status and linear model calculated status
regconfusion <- confusionMatrix(lm.y, test$final_status)
accuracyReg <- regconfusion$overall[1]
accuracyReg
regconfusion



###3 fold cross validation of linear regression
set.seed(199)
d3 <- d3[sample(nrow(d3)),]
folds <- cut(seq(1,nrow(d3)),breaks=3,labels=FALSE)
accuracyReg <- rep(0,3)
for(i in 1:3){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- d3[testIndexes, ]
  trainData <- d3[-testIndexes, ]
  lm.model <- lm(final_score ~ ., data = trainData)
  lm.y <- predict.lm(lm.model, testData)
  testData$final_score <- cut(testData$final_score, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
  testData$final_score <- as.factor(testData$final_score)
  lm.y <- round(lm.y)
  lm.y <- as.numeric(lm.y)
  lm.y <- cut(lm.y, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
  lm.y <- as.factor(lm.y)
  regconfusion <- confusionMatrix(lm.y, testData$final_score)
  accuracyReg[i] <- regconfusion$overall[1]
  print(accuracyReg)
  regconfusion
}


##Lasso Regularization to reduce over fitting

x=model.matrix (final_score~. -final_status , d3)[,-1]
x
y=d3$final_score
set.seed (199)
train=createDataPartition(d3$final_score, p = .7,
                          list = FALSE,
                          times = 1)
train

test=(-train)
y.test=y[test]
y.train=y[train]
y.train
##$#################lasso model
lasso.mod = glmnet (x[train,], y[train], alpha =1)
cv.out = cv.glmnet (x[train,], y[train], alpha =1)
bestlam =cv.out$lambda.min

##converting y training values to vectors of status
y.train <- round(y.train)
y.train <- as.numeric(y.train)
y.train <- cut(y.train, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
y.train <- as.factor(y.train)
y.train = factor(y.train,levels(y.train)[c(1,2,3)])


#confusion matrix of training data
lasso.predtrain = predict (lasso.mod, s=bestlam, newx=x[train,])
predLassoValuestrain <- round(lasso.predtrain[,1])
predLassoValuestrain <- as.numeric(predLassoValuestrain)
predLassoValuestrain <- cut(predLassoValuestrain, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
predLassoValuestrain <- as.factor(predLassoValuestrain)
lassoregconfusiontrain <- confusionMatrix(predLassoValuestrain, y.train)
lassoregconfusiontrain

##converting y testing values to vectors of status
y.test <- round(y.test)
y.test <- as.numeric(y.test)
y.test <- cut(y.test, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
y.test <- as.factor(y.test)
y.test = factor(y.test,levels(y.test)[c(1,2,3)])

##confusion matrix of testing data
lasso.pred = predict (lasso.mod, s=bestlam, newx=x[test,])
lasso.pred[,1]
predLassoValues <- round(lasso.pred[,1])
predLassoValues <- as.numeric(predLassoValues)
predLassoValues <- cut(predLassoValues, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
predLassoValues <- as.factor(predLassoValues)
lassoregconfusion <- confusionMatrix(predLassoValues, y.test)
accuracyLasso <- lassoregconfusion$overall[1]
accuracyLasso
lassoregconfusion

#############ridge regression model to reduce over fitting
ridge.mod = glmnet (x[train,], y[train], alpha =0)
cv.out = cv.glmnet (x[train,], y[train], alpha =0)
bestlam =cv.out$lambda.min


#training predictions and confusion matrix
ridge.predtrain = predict (ridge.mod, s=bestlam, newx=x[train,])
predridgeValuestrain <- round(ridge.predtrain[,1])
predridgeValuestrain <- as.numeric(predridgeValuestrain)
predridgeValuestrain <- cut(predridgeValuestrain, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
predridgeValuestrain <- as.factor(predridgeValuestrain)
risgeregconfusiontrain <- confusionMatrix(predridgeValuestrain, y.train)
risgeregconfusiontrain


##testing predictions confusion matrix
ridge.pred = predict (ridge.mod, s=bestlam, newx=x[test,])
predridgeValues <- round(ridge.pred[,1])
predridgeValues <- as.numeric(predridgeValues)
predridgeValues <- cut(predridgeValues, c(-Inf, 8, 14, Inf), labels =c("poor","fair","good"))
predridgeValues <- as.factor(predridgeValues)
risgeregconfusion <- confusionMatrix(predridgeValues, y.test)
accuracyridge <- risgeregconfusion$overall[1]
accuracyridge
risgeregconfusion


##Logistic Regression,naive bayes and decsion trees 
set.seed(199)
library(dplyr)
d3<- d3 %>% mutate_if(is.character,as.factor)


  testIndexes <- createDataPartition(d3$final_status, p = .7,
                                     list = FALSE,
                                     times = 1)
  testData <- d3[testIndexes, ]
  trainData <- d3[-testIndexes, ]

  
  #model of logisitic ,naive bayes and decision trees
  logistic <- multinom(final_status~. -final_score, data = trainData)
  nbmodel <- naiveBayes(final_status~. -final_score, data = trainData)
  rpmodel <- rpart(final_status~. -final_score, data =trainData)
  
  ##training data predictions of all the three models
  predictionNBtrain<- predict(nbmodel, trainData,type="class")
  predictionRPtrain <- predict(rpmodel, trainData, type = 'class')
  predictionlogistictrain <- predict(logistic, trainData,type="class")
  
  ##confusion matrix of all three models of training
  nboutputtrain <- confusionMatrix(trainData$final_status, predictionNBtrain)
  logisticoutputtrain <- confusionMatrix(trainData$final_status, predictionlogistictrain)
  rpoutputtrain<- confusionMatrix(trainData$final_status, predictionRPtrain)
  
  nboutputtrain
  logisticoutputtrain 
  rpoutputtrain
  
  
  ##predictions of testingdata
  predictionNB <- predict(nbmodel, testData,type="class")
  predictionRP <- predict(rpmodel, testData, type = 'class')
  predictionlogistic <- predict(logistic, testData,type="class")
  
  ##confusion matrix of all the three models of testing
  nboutput <- confusionMatrix(testData$final_status, predictionNB)
  logisticoutput <- confusionMatrix(testData$final_status, predictionlogistic)
  rpoutput <- confusionMatrix(testData$final_status, predictionRP)

  
  nboutput
  logisticoutput 
  rpoutput

##lasso for logistic regression
xlog=model.matrix (final_status~. -final_score, d3)[,-1]
ylog=d3$final_status
d3$final_status
set.seed (199)
trainlas=createDataPartition(d3$final_status, p = .7,
                             list = FALSE,
                             times = 1)
testlas=(-trainlas)
y.testlas=ylog[testlas]
y.trainlas=ylog[trainlas]


##$#################lasso model
lasso.modlog = glmnet (xlog[trainlas,], ylog[trainlas], alpha =1,family="multinomial",type.measure="class")
cv.outlog = cv.glmnet (xlog[trainlas,], ylog[trainlas], alpha =1,family="multinomial",type.measure="class")
bestlam =cv.out$lambda.min
lasso.modlog
cv.outlog 

#confusion matrix of training data
las = predict (lasso.modlog, s=bestlam, newx=xlog[trainlas,],type="class")
las <- as.factor(las)
lassoregconfusiontrain <- confusionMatrix(las, y.trainlas)
lassoregconfusiontrain



##confusion matrix of testing data
lastest = predict (lasso.modlog, s=bestlam, newx=xlog[testlas,],type="class")
lastest <- as.factor(lastest)
lassoregconfusiontest <- confusionMatrix(lastest, y.testlas)
lassoregconfusiontest


##$#################ridge model
rdige.modlog = glmnet (xlog[trainlas,], ylog[trainlas], alpha =0,family="multinomial")
cv.rdige = cv.glmnet (xlog[trainlas,], ylog[trainlas], alpha =0,family="multinomial",type.measure="class")
bestlam =cv.rdige$lambda.min



#confusion matrix of training data
rid = predict (cv.rdige, s=bestlam, newx=xlog[trainlas,],type="class")
rid <- as.factor(rid)
ridgeregconfusiontrain <- confusionMatrix(rid, y.trainlas)
ridgeregconfusiontrain 



##confusion matrix of testing data
ridest = predict (cv.rdige, s=bestlam, newx=xlog[testlas,],type="class")
ridest <- as.factor(ridest)
ridgeregconfusiontest <- confusionMatrix(ridest, y.testlas)
ridgeregconfusiontest$overall[1]
ridgeregconfusiontest

regtrain$overall[1]
regconfusion$overall[1]

#accuracies dataframes
Naive <-c(nboutputtrain$overall[1],nboutput$overall[1])
linear <-c(regtrain$overall[1],regconfusion$overall[1])
logistic <-c(logisticoutputtrain$overall[1],logisticoutput$overall[1])
decisiontrees <-c( rpoutputtrain$overall[1], rpoutput$overall[1])
Lasso <-c(lassoregconfusiontrain$overall[1],lassoregconfusiontest$overall[1])
ridge <-c(ridgeregconfusiontrain$overall[1],ridgeregconfusiontest$overall[1])
dataaccuracy <- data.frame(Naive,linear,logistic,decisiontrees,Lasso,ridge)

##accuracyplot
barplot(as.matrix(dataaccuracy),
        main="Model Accurarcies",
        ylab="Accuracies",
        beside=TRUE,
        legend=TRUE,
        ylim=c(0,1)
)

#https://rpubs.com/kaz_yos/alasso
#https://www.geeksforgeeks.org/multiple-barplots-in-r/
#https://rpubs.com/Vasanth_Kailasam/StudentAnalytics
#http://www.sthda.com/english/articles/38-regression-model-validation/158-regression-model-accuracy-metrics-r-square-aic-bic-cp-and-more/
#https://towardsdatascience.com/building-a-logistic-regression-in-python-step-by-step-becd4d56c9c8#:~:text=Logistic%20Regression%20is%20a%20Machine,%2C%20failure%2C%20etc.).