## перекрестная проверка с делением исходной выборки на 10 равных частей
## (9 подвыборок использовались для обучения и одна – для тестирования)
##
setwd("C:\\Users\\Анастасия\\Documents\\r_projects\\diploma\\data") ###
library (e1071)
##
T3 <- read.table("stud_2019_1.csv", sep=";", header=TRUE, row.name=NULL)
##
## 1 вариант – для отчисленных до первой сессии, поставить признак STUD_MARK=4
##
T3$STUD_MARK[is.na(T3$STUD_MARK) == TRUE] <- 4
T3$IS_UCH <-factor(T3$IS_UCH)
##
## Оставляем только тех студентов, которые отчислены или закончили
T3 <- T3[which(T3$IS_UCH == 0 | T3$IS_UCH == 1) ,]
##
data_stuff <- function(x,y)
{
  T4 <- T3[which(T3$SHORTNAME == x),] 
  if(!is.null(y)) 
  {
    T4 <- T4[which(T4$KEYS_PLUS == y),] 
  }
  size<- dim(T4)[1]
  svmModelLinear <- svm(IS_UCH ~ ., data = T4, cross=10, type = "C-classification", kernel = "radial")
  pred<-predict(svmModelLinear)
  tt<-table(Факт=T4$IS_UCH, Прогноз=pred)
  Acc <- mean(predict(svmModelLinear) == T4$IS_UCH)
  ans<-c(Acc,size)
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
sink("out_svm2.csv")
cat("Факультет","Специальность","Размер выборки","Точность","\n",sep=";")

for(i in names_iter)
{
  name<-names[i]
  cat(name, ";")
  cat("  ", ";")
  ans <- data_stuff(name,NULL)
  cat(ans[2], ";")
  cat( round(100*ans[1], 2),"\n",sep=";")
  for(j in keys[[i]])
  {
    cat("  ",";")
    cat(j,";")
    ans <- data_stuff(name,j)
    cat(ans[2], ";")
    cat( round(100*ans[1], 2),"\n",sep=";")
  }
}
sink()

## выбираем нужные наблюдения
##
##T4 <- T3 ## для всего университета
##ФБ
T4 <- T3[which(T3$SHORTNAME == "АВТФ"),] 
##T4 <- T4[which(T4$KEYS_PLUS == "38.03.01"),] 
##T4 <- T4[which(T4$KEYS_PLUS == "38.03.02"),]
##T4 <- T4[which(T4$KEYS_PLUS == "38.03.05"),]
##T4 <- T4[which(T4$KEYS_PLUS == "38.05.01"),]
##T4 <- T4[which(T4$KEYS_PLUS == "43.03.01"),]
##T4 <- T4[which(T4$KEYS_PLUS == "43.03.02"),]

##T4 <- T4 [c("IS_INVAL", "IS_UCH")] ## для отдельных факторов
##
T4 <- subset(T4, select = IS_INVAL : IS_UCH)
## Оставлены только нужные столбцы
##
svmModelLinear <- svm(IS_UCH ~ ., data = T4, cross=10, type = "C-classification", kernel = "radial")
## IS_UCH ~ . влияние всех факторов на IS_UCH
## в dataTrain и факторы и класс IS_UCH
## svmModelLinear - структура
##
pred<-predict(svmModelLinear)
table(Факт=T4$IS_UCH, Прогноз=pred)
## результат
##str(T4)
Acc <- mean(predict(svmModelLinear) == T4$IS_UCH)
paste("Точность=", round(100*Acc, 2), "%", sep="")
##