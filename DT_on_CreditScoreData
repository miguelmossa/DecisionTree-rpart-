data <-read.csv2("credsco2.csv", header = TRUE, sep = ";",quote = "\"",
                         dec = ".")

set.seed(12345)
## view the first few rows of the data
head(data)
summary(data)
names(data)


data$Dictamen <- factor(data$Dictamen)
data$Vivienda <- factor(data$Vivienda)
data$E_civil <- factor(data$E_civil)
data$Cliente <- factor(data$Cliente)
data$Nuevo <- factor(data$Nuevo)
data$CIM <- factor(data$CIM)
data$T_trabajo <- factor(data$T_trabajo)
data$Patrim_si.no <- factor(data$Patrim_si.no)


library(xtable)
methods(xtable)
xtable(summary(data)[1:2,1])

summary(data)


## We first split the available data into learning and test sets, selecting randomly 2/3 and 1/3 of the data
## We do this for a honest estimation of prediction performance

N <- nrow(data)
learn <- sample(1:N, round(2*N/3))
Learning<-data[learn,]
Test<-data[-learn,]
nlearn <- length(learn)
ntest <- N - nlearn

#Library rpart to build a decision tree

library(rpart)
DT = rpart(Dictamen ~ ., data=Learning, parms=list(split='gini'),
           control=rpart.control(cp=0.001, xval=10, maxdepth=15))
printcp(DT)

#Optimal tree
alfa <- DT$cptable[which.min(DT$cptable[,4]),1]
alfa
prunetree <- prune(DT,cp=alfa)
attributes(prunetree)

#plottig the optimal decision tree 
library(rattle)
library(rpart.plot)
fancyRpartPlot(prunetree)
asRules(prunetree)


#prediction on test and learning data
pred.test = predict(prunetree,newdata=Test,type="class")
pred.learning=predict(prunetree,newdata=Learning,type="class")
#confusion matrices 
CM.test = table(Test$Dictamen,pred.test)
CM.test
CM.learning = table(Learning$Dictamen,pred.learning)
CM.learning

#Measures of error

plot(DT$cptable[,2],DT$cptable[,3],type="l")
lines(DT$cptable[,2],DT$cptable[,4],col="blue")
legend("topright",c("R(T)training","R(T)cv"),col=c("black","blue"),lty=1)

error_rate.test=(CM.test[1,2]+CM.test[2,1]) /sum(CM.test)
error_rate.learning= (CM.learning[1,2]+CM.learning[2,1])/sum(CM.learning)

accuracy_positive.learning=CM.learning[2,2]/(sum(CM.learning[,2]))
accuracy_negative.learning=CM.learning[1,1]/(sum(CM.learning[,1]))

accuracy_positive.test=CM.test[2,2]/(sum(CM.test[,2]))
accuracy_negative.test=CM.test[1,1]/(sum(CM.test[,1]))

average_accuracy.learning=(accuracy_positive.learning+accuracy_negative.learning)/2
average_accuracy.test=(accuracy_positive.test+accuracy_negative.test)/2

recall.learning=CM.learning[2,2]/(sum(CM.learning[2,]))
recall.test=CM.test[2,2]/(sum(CM.test[2,]))

measures.error<-rbind(error_rate.test,error_rate.learning,accuracy_positive.learning,accuracy_negative.learning,accuracy_positive.test,accuracy_negative.test,average_accuracy.learning,average_accuracy.test,recall.learning,recall.test)
xtable(measures.error)


# confusion matrix using DIFFERENT threshold and type=prob ( test data) 
pred1<-predict(prunetree,newdata=Test,type="prob")
threshold <- 0.859
pred2<- factor( ifelse(pred1[, "2"] > threshold, "2", "1") )
pred2<- relevel(pred2, "2")   # you may or may not need this; I did
CM1 = table(Test$Dictamen,pred2)

#another posibility to calculate CM
CMlibrary(caret)
confusionMatrix(pred2,Test$Dictamen)

##ROC Curve
library(pROC)
probsTrain <- predict(prunetree,Test, type = "prob")
rocCurve   <- roc(response = Test$Dictamen,
                  predictor = probsTrain[, "2"],
                  levels = rev(levels(Test$Dictamen)))
attributes(rocCurve)
rocCurve$auc
plot(rocCurve)
