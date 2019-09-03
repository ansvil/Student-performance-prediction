## script2.txt
## Svm
## Процентного разделения
## (2/3 набора данных используется для обучения и 1/3 – для тестирования)
##
setwd("C:\\Users\\Анастасия\\Documents\\r_projects\\diploma\\data") 
library (e1071)
library (caret)
##
T3 <- read.table("stud_2019_1.csv", sep=";", header=TRUE, row.name=NULL)
## прочитан файл 'data.frame': 18768 obs. of 11 variables:
##
## 1 вариант – для отчисленных до первой сессии, поставить признак STUD_MARK=4
##
T3$STUD_MARK[is.na(T3$STUD_MARK) == TRUE] <- 4
T3$IS_UCH <-factor(T3$IS_UCH)
##
## Оставляем только тех студентов, которые отчислены или закончили
T3 <- T3[which(T3$IS_UCH == 0 | T3$IS_UCH == 1) ,]
##функция для рассчета
data_stuff <- function(x,y)
{
  T4 <- T3[which(T3$SHORTNAME == x),] 
  if(!is.null(y)) 
  {
    T4 <- T4[which(T4$KEYS_PLUS == y),] 
  }
  size<- dim(T4)[1]
  trainIdx <- sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE )
  dataTrain <- T4[trainIdx,]
  dataTest <- T4[-trainIdx,]
  svmModelLinear <- svm(IS_UCH ~ ., data = dataTrain, type = "C-classification", cost=1, kernel = "radial")
  predictionsTrain <- predict(svmModelLinear, T4[trainIdx,])
  table(dataTrain$IS_UCH, predictionsTrain)
  AccTrain <- mean(predict(svmModelLinear) == dataTrain$IS_UCH)
  predictionsTest <- predict(svmModelLinear, dataTest)
  tt <- table(dataTest$IS_UCH, predictionsTest)
  AccTest <- (tt[1]+tt[5])/(tt[1]+tt[2]+tt[4]+tt[5])
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
sink("out_svm1.csv")
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

##
## выбираем нужные наблюдения
##
##T4 <- T3 ## для всего университета
##ФТФ
T4 <- T3[which(T3$SHORTNAME == "АВТФ"),] 
##T4 <- T4[which(T4$KEYS_PLUS == "03.03.02"),] 
##T4 <- T4[which(T4$KEYS_PLUS == "12.03.05"),]
##T4 <- T4[which(T4$KEYS_PLUS == "12.03.03"),]
##T4 <- T4[which(T4$KEYS_PLUS == "12.03.02"),]
##T4 <- T4[which(T4$KEYS_PLUS == "16.03.01"),]
##T4 <- T4[which(T4$KEYS_PLUS == "55.05.03"),]

##T4 <- T4 [c("IS_INVAL", "IS_UCH")] ## для отдельных факторов
##
T4 <- subset(T4, select = IS_INVAL : IS_UCH)
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
svmModelLinear <- svm(IS_UCH ~ ., data = dataTrain, type = "C-classification", cost=1, kernel = "radial")
##varImp(svmModelLinear)
## радиальное ядро
## IS_UCH ~ . влияние всех факторов на IS_UCH
## в dataTrain и факторы и класс IS_UCH
## svmModelLinear - структура
##svmModelLinear <- svm(IS_UCH ~ ., data = dataTrain, type = "C-classification", cost=1, kernel = "linear")
## альтернатива - линейное ядро (точность значительно ниже)
##
predictionsTrain <- predict(svmModelLinear, T4[trainIdx,])
## оценили на обучающей выборке
## predictionsTrain - структура
##
table(dataTrain$IS_UCH, predictionsTrain)
AccTrain <- mean(predict(svmModelLinear) == dataTrain$IS_UCH)
paste("Точность на обучающей выборке=", round(100*AccTrain, 2), "%", sep="")
## точность на обучающей выборке
##
predictionsTest <- predict(svmModelLinear, dataTest)
## оценили на тестовой выборке
## predictionsTest - структура
##
tt <- table(dataTest$IS_UCH, predictionsTest)
## точность на тестовой выборке
AccTest <- (tt[1]+tt[5])/(tt[1]+tt[2]+tt[4]+tt[5])
paste("Точность на тестовой выборке=", round(100*AccTest, 2), "%", sep="")
##

##подсчет точности
file_names<-c("out_svm1.csv","out_svm2.csv","out_cart.csv","out_randomForest.csv",
              "out_svm1_notFactor.csv","out_svm2_notFactor.csv","out_cart_notFactor.csv","out_randomForest_notFactor.csv")
sink("Средняя_точность.csv")
cat("Алгоритм" ,"Среднее точности на обучающей выборке", "Среднее точности на тестовой выборке", "\n", sep=";")
for(file in file_names)
{ 
  acc_data<-read.table(file, sep=";", header=TRUE, row.name=NULL)
  train<-mean(acc_data$Точность.на.обучающей)
  test<-mean(acc_data$Точность.на.тестовой)
  cat(file, train,test,"\n",sep=";")
}
sink()
acc_data<-read.table("out_svm2_notFactor.csv", sep=";", header=TRUE, row.name=NULL)
mean_acc<-mean(acc_data$Точность)
mean_acc
