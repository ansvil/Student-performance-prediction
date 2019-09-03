setwd("C:\\Users\\Анастасия\\Documents\\r_projects\\diploma\\data")
t1<-train[is.na(train)== TRUE]
length(t1)
Sys.setlocale("LC_ALL","Russian_Russia") 
library (caret)
library(RWeka)
T3 <- read.table("stud_2019_1.csv", sep=";", header=TRUE)
T3$STUD_MARK[is.na(T3$STUD_MARK) == TRUE] <- 4
T3$IS_UCH[T3$IS_UCH == 1] <- 'l'
T3$IS_UCH[T3$IS_UCH == 0] <- 'o'
T3 <- T3[which(T3$IS_UCH == 'o' | T3$IS_UCH == 'l') ,]
T4 <- T3[which(T3$SHORTNAME == "ФЭН"),]
T4 <- subset(T4, select = IS_INVAL : IS_UCH)
set.seed(1000)
trainIdx <- sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE)
train <-T4[ trainIdx, ]
test <- T4[-trainIdx, ]
ctrl <- trainControl(method = "repeatedcv",repeats =
                       3,classProbs = TRUE,summaryFunction = twoClassSummary)
#train$IS_UCH<-as.factor(train$IS_UCH)
#added to defeat 
row.has.na <- apply(train, 1, function(x){any(is.na(x))})
#predictors_no_NA <- train[!row.has.na, ]
train_bayes <- train(IS_UCH ~ . ,data = train,method =
                       "naive_bayes",trControl = ctrl)
predictionsTrain <- predict(train_bayes, train)
table(train$IS_UCH, predictionsTrain)
Acc <- mean(predict(train_bayes) == train$IS_UCH)
paste("Точность на обучающей=", round(100*Acc, 2), "%",
      sep="")
predictionsTest <- predict(train_bayes, test)
tt <- table(test$IS_UCH, predictionsTest)
Acc1 <- (tt[1]+tt[4])/(tt[1]+tt[2]+tt[3]+tt[4])
paste("Точность на тестовой=", round(100*Acc1, 2), "%",
      sep="")