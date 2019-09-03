setwd("C:\\Users\\Анастасия\\Documents\\r_projects\\diploma\\data")
library (caret)
library(RWeka)
T3 <- read.table("stud_2019_2.csv", sep=";", header=TRUE)
#is.na(T3$STUD_MARK_DESCR)== TRUE
T3$STUD_MARK[is.na(T3$STUD_MARK) == TRUE] <- 4
T3$IS_UCH[T3$IS_UCH == 1] <- 'l'
T3$IS_UCH[T3$IS_UCH == 0] <- 'o'
T3 <- T3[which(T3$IS_UCH == 'o' | T3$IS_UCH == 'l') ,]

data_stuff <- function(x,y)
{
  T4 <- T3[which(T3$SHORTNAME == x),] 
  if(!is.null(y)) 
  {
    T4 <- T4[which(T4$KEYS_PLUS == y),] 
  }
  size<- dim(T4)[1]
  set.seed(1000)
  trainIdx <- sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE )
  dataTrain <- T4[trainIdx,]
  dataTest <- T4[-trainIdx,]
  ctrl <- trainControl(method = "repeatedcv",repeats =
                         3,classProbs = TRUE,summaryFunction = twoClassSummary)
  train_JRip <- train(IS_UCH ~ .,data = train,method =
                        "JRip",trControl = ctrl)
  
  predictionsTrain <- predict(train_JRip, train)
  table(train$IS_UCH, predictionsTrain)
  AccTrain <- mean(predict(train_JRip) == train$IS_UCH)
  predictionsTest <- predict(train_JRip, test)
  tt <- table(test$IS_UCH, predictionsTest)
  AccTest <-  (tt[1]+tt[4])/(tt[1]+tt[2]+tt[3]+tt[4]) 
  ans<-c(AccTrain,AccTest,size)
  return (ans)
}
##переменные для работы с функцией
names<-c("АВТФ","ФЛА","МТФ","ФМА","ФПМИ","РЭФ","ФТФ","ФЭН", "ФБ","ФГО","ЮФ")
names_iter<-c(1:length(names))
keys<-list(
  c("09.03.01","09.03.02","09.03.04","10.03.01","10.05.03","12.03.01","12.03.04","27.03.04"),
  c("05.03.06","15.03.03","16.03.01","17.05.01","20.03.01","24.03.03","24.03.04","24.05.07","25.03.01","27.03.04"),
  c("15.03.04","18.03.01","18.03.02","15.03.02","15.03.05","22.03.01","23.03.03","28.03.02"),
  c("13.03.02","15.03.04","38.03.02","19.03.04"),
  c("01.03.02","02.03.03"),
  c("11.03.01","11.03.04","28.03.01","11.03.02","11.03.03"),
  c("03.03.02","12.03.05","12.03.03","12.03.02","16.03.01","55.05.03"),
  c("13.03.01","13.03.02","15.03.04","38.03.02","20.03.01"),
  c("38.03.01","38.03.02","38.03.05","38.05.01","43.03.01","43.03.02"),
  c("37.03.01","39.03.01","41.03.01","42.03.01","42.03.02","45.03.01","45.03.02"),
  c("40.03.01")
)
sink("out_JRip.csv")
cat("Факультет","Специальность","Размер выборки","Точность на обучающей","Точность на тестовой","\n",sep=";")

for(i in names_iter)
{
  name<-names[i]
  cat(name, ";")
  cat("  ", ";")
  ans <- data_stuff(name,NULL)
  cat(ans[3], ";")
  cat( round(100*ans[1], 2), round(100*ans[2], 2),"\n",sep=";")
  for(j in keys[[i]])
  {
    cat("  ",";")
    cat(j,";")
    ans <- data_stuff(name,j)
    cat(ans[3], ";")
    cat( round(100*ans[1], 2), round(100*ans[2], 2),"\n",sep=";")
  }
}
sink()

T4 <- T3[which(T3$SHORTNAME == "ФМА"),]
T4 <- subset(T4, select = IS_INVAL : IS_UCH)
set.seed(1000)
trainIdx <- sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE)
train <-T4[ trainIdx, ]
test <- T4[-trainIdx, ]
ctrl <- trainControl(method = "repeatedcv",repeats =
                       3,classProbs = TRUE,summaryFunction = twoClassSummary)
train_JRip <- train(IS_UCH ~ .,data = train,method =
                      "JRip",trControl = ctrl)
predictionsTrain <- predict(train_JRip, train)
table(train$IS_UCH, predictionsTrain)
Acc <- mean(predict(train_JRip) == train$IS_UCH)
paste("Точность на обучающей=", round(100*Acc, 2), "%",
      sep="")
predictionsTest <- predict(train_JRip, test)
tt <- table(test$IS_UCH, predictionsTest)
Acc1 <- (tt[1]+tt[4])/(tt[1]+tt[2]+tt[3]+tt[4])
paste("Точность на тестовой=", round(100*Acc1, 2), "%",
      sep="")
summary(train_JRip)
xx<-varImp(train_JRip)
sink("test.csv")
cat(xx$importance)
sink()
