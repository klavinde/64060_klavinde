borrower.df <- read.csv("C://Users/krist/Desktop/KENT/MAchine Learning/Assignment_2/UniversalBank.csv")
install.packages("caret")
library(caret)
install.packages("ISLR")
library(ISLR)
head(borrower.df)
summary(borrower.df)
library(dplyr)
m_borrower.df <- select(borrower.df,Age,Experience,Income,Family,CCAvg,Education,Mortgage,Personal.Loan,Securities.Account,CD.Account,Online,CreditCard)
set.seed(123)
head(m_borrower.df)
train.index <- sample(row.names(m_borrower.df), 0.6*dim(m_borrower.df)[1])
valid.index <- setdiff(row.names(m_borrower.df), train.index)
train.df <- m_borrower.df[train.index, ]
valid.df <- m_borrower.df[valid.index, ]
## new borrower
new_borrower.df <- data.frame(Age=40,Experience=10,Income=84,Family=2,CCAvg=2,Education=1,Mortgage=0,Securities.Account=0,CD.Account=0,Online=1, CreditCard=1)
train.norm.df <- train.df
valid.norm.df <- valid.df
m_borrower.norm.df <- m_borrower.df
norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2])
m_borrower.norm.df [, 1:2] <- predict(norm.values, m_borrower.norm.df[, 1:2])
new.norm.df <- predict(norm.values, new_borrower.df)
install.packages("FNN")
library(FNN)
summary(train.norm.df)
summary(new.norm.df)
nn <- knn(train = train.norm.df[, 1:2], test = new.norm.df[,1:2],
          cl = train.norm.df[, 12], k = 1)
print(nn)
##Customer would not take loan
library(caret)
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2], 
                  cl = train.norm.df[, 12], k = i)
  accuracy.df[i, 12] <- confusionMatrix(knn.pred,valid.norm.df[, 12])$overall[1] 
}
Accuracy.df
nn3 <- knn(train = train.norm.df[, 1:2], test = new.norm.df[,1:2],
          cl = train.norm.df[, 12], k = 3)
print(nn3)
##K vlaue of 3 would still support that Customer would not take loan.
set.seed(15)
Test_Index = createDataPartition(m_borrower.df$Personal.Loan,p=0.2, list=FALSE)
Test_Data = m_borrower.df[Test_Index,]
TraVal_Data = m_borrower.df[-Test_Index,]
Train_Index = createDataPartition(TraVal_Data$Personal.Loan,p=0.75, list=FALSE)
Train_Data = TraVal_Data[Train_Index,]
Validation_Data = TraVal_Data[-Train_Index,]
summary(Train_Data)
norm.values2 <- preProcess(Train_Data[, 1:2], method=c("center", "scale"))
Train_Data[, 1:2] <- predict(norm.values2, Train_Data[, 1:2])
Validation_Data [, 1:2] <- predict(norm.values2, Validation_Data[, 1:2])
m_borrower.norm.df [, 1:2] <- predict(norm.values2, m_borrower.norm.df[, 1:2])
new.norm2.df <- predict(norm.values2, new_borrower.df)
install.packages("FNN")
library(FNN)
summary(Train_Data)
summary(new.norm2.df)
nn <- knn(train = Train_Data[, 1:2], test = new.norm2.df[,1:2],
          cl = Train_Data[, 12], k = 1)
print(nn)
for(i in 1:14) {
  knn.pred2 <- knn(Train_Data[, 1:2], Validation_Data [, 1:2], 
                  cl = Train_Data[, 12], k = i)
  accuracy.df[i, 12] <- confusionMatrix(knn.pred2,Validation_Data[, 12])$overall[1] 
}
Accuracy.df
## Repartitioning data would support prospective customer not taking loan.