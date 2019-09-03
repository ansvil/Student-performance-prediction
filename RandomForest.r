## script5.txt
## Деревья решений
## Случайный лес: функция randomForest из пакета randomForest
##
## (2/3 набора данных используется для обучения и 1/3 – для тестирования)
## install.packages("randomForest")
##
setwd("C:\\Users\\Анастасия\\Documents\\r_projects\\diploma\\data") ###
library (randomForest)
library (caret)
##
T3 <- read.table("stud_2019_2.csv", sep=";", header=TRUE, row.name=NULL)
## прочитан файл 'data.frame': 18768 obs. of 11 variables:
##
## Оставляем только тех студентов, которые отчислены или закончили
T3 <- T3[which(T3$IS_UCH == 0 | T3$IS_UCH == 1) ,]
##
T3$STUD_MARK[is.na(T3$STUD_MARK) == TRUE] <- 4
##
## Продублируем IS_UCH и перекодируем его (иначе алгоритм не работает)
##
T3$IS_UCH1 <- factor(T3$IS_UCH)
##T3$IS_UCH1[T3$IS_UCH1 == 1] <- "Закончил"
##T3$IS_UCH1[T3$IS_UCH1 == 0] <- "Бросил "
##
## Делаем его фактором
##
T3$IS_UCH1 <-factor(T3$IS_UCH1)
data_stuff <- function(x,y)
{
  T4 <- T3[which(T3$SHORTNAME == x),] 
  if(!is.null(y)) 
  {
    T4 <- T4[which(T4$KEYS_PLUS == y),] 
  }
  T4 <- T4[, c(6:11, 13)]
  size<- dim(T4)[1]
  trainIdx <- sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE )
  dataTrain <- T4[trainIdx,]
  dataTest <- T4[-trainIdx,]
  TrainForest <- randomForest(IS_UCH1 ~ ., data = dataTrain, nodesize=25, ntree = 300)
  predictForest <- predict(TrainForest, dataTrain)
  ForestTrain <- mean(predict(TrainForest) == dataTrain$IS_UCH1)
  predictionsTest <- predict(TrainForest, dataTest)
  tt <- table(dataTest$IS_UCH1, predictionsTest)
  ForestTest <- (tt[1]+tt[4])/(tt[1]+tt[2]+tt[3]+tt[4])
  ans<-c(ForestTrain,ForestTest,size)	
  return (ans)
}
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
sink("out_randomForest_notFactor.csv")
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


##цикл для важности регрессоров
my_varimp <- function(x,y)
{
  T4 <- T3[which(T3$SHORTNAME == x),] 
  if(!is.null(y)) 
  {
    T4 <- T4[which(T4$KEYS_PLUS == y),] 
  }
  T4 <- T4[, c(6:11, 13)]
  size<- dim(T4)[1]
  trainIdx <- sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE )
  dataTrain <- T4[trainIdx,]
  dataTest <- T4[-trainIdx,]
  TrainForest <- randomForest(IS_UCH1 ~ ., data = dataTrain, nodesize=25, ntree = 300)
  ##if(dim(TrainForest$cptable)[1]>1)
  ##{
    xx<-varImp(TrainForest)[1]
    
    row_name<-rownames(xx)
    pred_names<-c("EGE","HOSTEL","IS_CELEV","IS_INVAL","STUD_MARK","SC_LVL")
    if(isTRUE(all.equal(row_name,pred_names)))
    {
      ans<-unlist(xx)
    }
    else
    {
      tmp<-c()
      tmp1<-unlist(xx)
      for(i in c(1:length(pred_names)))
      {
        for(j in c(1:length(row_name)))
        {
          if(pred_names[i]==row_name[j]) 
          {
            tmp<-c(tmp,tmp1[j])
          }
        }
      }
      ans<-tmp
    }
  ##}
  ##else
  ##{
  ##  ans<-rep("null_tree",6)
  ##}
  return (ans)
}

sink("varimp_RandomForest_notFactor.csv")
cat("Факультет","Специальность","EGE","HOSTEL","IS_CELEV","IS_INVAL","STUD_MARK","SC_LVL","\n",sep=";")

for(i in names_iter)
{
  name<-names[i]
  cat(name, ";")
  cat("  ", ";")
  ans <- my_varimp(name,NULL)
  for (k in c(1:length(ans)))
  {
    cat(ans[k],";",sep="")
  }
  cat( "\n")
  for(j in keys[[i]])
  {
    cat("  ",";")
    cat(j,";")
    ans <- my_varimp(name,j)
    for (k in c(1:length(ans)))
    {
      cat(ans[k],";",sep="")
    }
    cat( "\n")
  }
}
sink()

## выбираем нужные наблюдения
##
##T4 <- T3 ## для всего университета
## Набор данных для ФПМИ
T4 <- T3[which(T3$SHORTNAME == "ФПМИ"),] ## для отдельных факультетов
##T4 <- T4[which(T4$KEYS_PLUS == "01.03.02"),] ## для отд. специальностей ПМ
##T4 <- T4[which(T4$KEYS_PLUS == "02.03.03"),] ## для отд. специальностей ПМИ

##T4 <- T4 [c("SC_LVL", "IS_UCH")] ## для отдельных факторов

T4 <- T4[, c(6:11, 13)]
##T4 <- subset(T4, select = IS_INVAL : IS_UCH, IS_UCH1)
## Оставлены только нужные столбцы
##
trainIdx <- sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE )
##
## случайно выбрали 2/3 наблюдений
##
dataTrain <- T4[trainIdx,]
## Обучающая выборка
##
dataTest <- T4[-trainIdx,]
## Тестирующая выборка
##
TrainForest <- randomForest(IS_UCH1 ~ ., data = dataTrain, nodesize=25, ntree = 300,importance=TRUE)
##TrainForest <- randomForest(IS_UCH1 ~ ., data = dataTrain)
##	чего-то посчитали
##	IS_UCH1 ~ . влияние всех факторов на IS_UCH
##	в dataTrain и факторы и класс IS_UCH
importance(TrainForest)
library(caret)

xx<-varImp(TrainForest)
predictForest <- predict(TrainForest, dataTrain)
## 	точность на обучающей выборке
ForestTrain <- mean(predict(TrainForest) == dataTrain$IS_UCH1)
##
##	оценили на тестовой выборке
##
predictionsTest <- predict(TrainForest, dataTest)
tt <- table(dataTest$IS_UCH1, predictionsTest)
## 	точность на тестовой выборке 
ForestTest <- (tt[1]+tt[4])/(tt[1]+tt[2]+tt[3]+tt[4])
paste("Точность=", round(100*ForestTest, 2), "%", sep="")
##	