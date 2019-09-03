
##	Деревья решений
##	Алгоритм CART: функция rpart из пакета rpart
##
##	Процентного разделения 
##	(2/3 набора данных используется для обучения и 1/3 – для тестирования) 
##	install.packages("rpart")
##	install.packages("rpart.plot")
##	install.packages("rattle")
##	install.packages("RColorBrewer")
##
setwd("C:\\Users\\Анастасия\\Documents\\r_projects\\diploma\\data") ###
library (rpart)
library (rpart.plot)
library (rattle)
library (RColorBrewer)
library (DMwR)
library (caret)
##
T3 <- read.table("stud_2019_1.csv", sep=";", header=TRUE, row.name=NULL)
## 	прочитан файл	'data.frame':	18768 obs. of  11 variables:
##
##     1 вариант – для отчисленных до первой сессии, поставить признак STUD_MARK=4
##
T3$STUD_MARK[is.na(T3$STUD_MARK) == TRUE] <- 4
##
##T3$IS_INVAL <-factor(T3$IS_INVAL)
##T3$IS_CELEV <-factor(T3$IS_CELEV)
##T3$STUD_MARK <-factor(T3$STUD_MARK)
##T3$HOSTEL <-factor(T3$HOSTEL)
##T3$SC_LVL <-factor(T3$SC_LVL)
##T3$EGE <-factor(T3$EGE)
T3$IS_UCH <-factor(T3$IS_UCH)
##
##    Оставляем только тех студентов, которые отчислены или закончили
T3 <- T3[which(T3$IS_UCH == 0  |  T3$IS_UCH == 1) ,]
## 
##	выбираем нужные наблюдения
##
##T4 <- T3	## для всего университета
data_stuff <- function(x,y)
{
  T4 <- T3[which(T3$SHORTNAME == x),] 
  if(!is.null(y)) 
    {
    T4 <- T4[which(T4$KEYS_PLUS == y),] 
    }
  else(y<-" ")
  T4 <- subset(T4, select = IS_INVAL : IS_UCH)
  size<-dim(T4)[1]
  trainIdx <-  sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE )
  dataTrain <-  T4[trainIdx,]
  dataTest <-  T4[-trainIdx,]
  Tree <-  rpart(IS_UCH ~ ., data = dataTrain, method = "class", control = rpart.control(minbucket = 25))
  if(dim(Tree$cptable)[1]>1)
  {
    path<-"C:\\Users\\Анастасия\\Documents\\r_projects\\diploma\\data\\cart_result_notFactor\\"
    img_name<-paste(path,x,"_",y,".png",sep="")
    png(filename = img_name)
    prettyTree(Tree)	 ##визуализация
    dev.off()
  }
  predictionsTrain <- predict(Tree, dataTrain, type = "class")
  tt <- table(dataTrain$IS_UCH, predictionsTrain)
  TreeTrain <- (tt[1]+tt[5])/(tt[1]+tt[2]+tt[4]+tt[5])
  predictionsTest <- predict(Tree, dataTest, type = "class")
  tt <- table(dataTest$IS_UCH, predictionsTest)
  TreeTest <- (tt[1]+tt[5])/(tt[1]+tt[2]+tt[4]+tt[5])
  ans<-c(TreeTrain, TreeTest,size)
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
sink("out_cart_notFactor.csv")
cat("Факультет","Специальность","Размер выборки","Точность на обучающей","Точность на тестовой","\n",sep=";")
##цикл для подсчета точности
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
##цикл для важности регрессоров
my_varimp <- function(x,y)
{
  T4 <- T3[which(T3$SHORTNAME == x),] 
  if(!is.null(y)) 
  {
    T4 <- T4[which(T4$KEYS_PLUS == y),] 
  }
  else(y<-" ")
  T4 <- subset(T4, select = IS_INVAL : IS_UCH)
  size<-dim(T4)[1]
  trainIdx <-  sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE )
  dataTrain <-  T4[trainIdx,]
  dataTest <-  T4[-trainIdx,]
  Tree <-  rpart(IS_UCH ~ ., data = dataTrain, method = "class", control = rpart.control(minbucket = 25))
  if(dim(Tree$cptable)[1]>1)
  {
    xx<-varImp(Tree)
  
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
  }
  else
  {
    ans<-rep("null_tree",6)
  }
  return (ans)
}

sink("varimp_cart.csv")
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


##АВТФ
T4 <- T3[which(T3$SHORTNAME == "АВТФ"),] 
##T4 <- T4[which(T4$KEYS_PLUS == "09.03.01"),] 
T4 <- T4[which(T4$KEYS_PLUS == "09.03.02"),]
##T4 <- T4[which(T4$KEYS_PLUS == "09.03.04"),]
##T4 <- T4[which(T4$KEYS_PLUS == "10.03.01"),]
##T4 <- T4[which(T4$KEYS_PLUS == "10.05.03"),]
##T4 <- T4[which(T4$KEYS_PLUS == "12.03.01"),]
##T4 <- T4[which(T4$KEYS_PLUS == "12.03.04"),]
##T4 <- T4[which(T4$KEYS_PLUS == "27.03.04"),]
##
##T4 <- T4[, c(5:6, 8:11)]	## берем выборочно столбцы
T4 <- subset(T4, select = IS_INVAL : IS_UCH)
##	Оставлены только нужные столбцы
##
trainIdx <-  sample (nrow (T4), 2*nrow(T4)/3, replace = FALSE )
##
##	случайно выбрали 2/3 наблюдений
##
dataTrain <-  T4[trainIdx,]
##	Обучающая выборка
##	 
dataTest <-  T4[-trainIdx,]
##	Тестирующая выборка
##
Tree <-  rpart(IS_UCH ~ ., data = dataTrain, method = "class", control = rpart.control(minbucket = 25))
if(dim(Tree$cptable)[1]>1)
{
 varImp(Tree) 
}

xx<-varImp(Tree)
row_name<-rownames(xx)
tmp<-unlist(xx)
sink("test.csv")
for(i in c(1:length(tmp)))
{
  cat(row_name[i],tmp[i],sep=" ")
}
sink()
library(readr)
mystr<-read_file("test.txt")
sink("test.csv")
cat(mystr)
sink()
##	чего-то посчитали
##	IS_UCH ~ . влияние всех факторов на IS_UCH
##	в dataTrain и факторы и класс IS_UCH
##	Tree - структура
##
##prp(Tree)	## визуализация
##prettyTree(Tree, compress = TRUE)	## визуализация

if(dim(Tree$cptable)[1]>1)
  {
    path<-"C:\\Users\\Анастасия\\Documents\\r_projects\\diploma\\data\\cart_result\\"
    img_name<-paste(path,"fami",".png",sep="")
    png(filename = img_name)
    prettyTree(Tree)	 
    dev.off()
}
##
predictionsTrain <- predict(Tree, dataTrain, type = "class")
##	оценили на обучающей выборке
##	predictionsTrain - структура
##
tt <- table(dataTrain$IS_UCH, predictionsTrain)
## 	точность на обучающей выборке
TreeTrain <- (tt[1]+tt[5])/(tt[1]+tt[2]+tt[4]+tt[5])
paste("Точность на обучающей выборке=", round(100*TreeTrain, 2), "%", sep="")
##
predictionsTest <- predict(Tree, dataTest, type = "class")
##	оценили на тестовой выборке
##
tt <- table(dataTest$IS_UCH, predictionsTest)
## 	точность на тестовой выборке
TreeTest <- (tt[1]+tt[5])/(tt[1]+tt[2]+tt[4]+tt[5])
paste("Точность на тестовой выборке=", round(100*TreeTest, 2), "%", sep="")
##
