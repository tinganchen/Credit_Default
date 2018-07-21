#========================
#2005/04~09台灣信用卡用戶資料----------------------------------------------#
#========================

(credit = read.csv("creditcard_default_kaggle.csv",header=T))

	#樣本數 3(萬),變數25個
dim(credit)
names(credit)
summary(credit)

#=================
#1.logistic回歸模型------------------------------------------------------# 
#=================
glm <- glm(credit[,25]~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
	PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6,family=binomial(link=logit),
	data=credit) 
summary(glm) 

	#逐步法
logit.step<-step(glm,direction="both") #forward/backward.
summary(logit.step) 
coefficients(logit.step)


	#使用訓練與測試82法則, 則取訓練筆數:測試筆數=8:2=24000:6000

	#進行資料整理與分割(train & test)
default.01<-as.numeric(credit[,25])
index<-1:dim(credit)[1]
train.index <- sample(index,length(index)*0.8)

train.X<-credit[train.index,2:12]
train.Y<-default.01[train.index]
test.X<-credit[-train.index,2:12]
test.Y<-default.01[-train.index]

train_set<-data.frame(train.X,train.Y)
names(train_set)

	#訓練.logistic回歸模型
train_glm <- glm(train.Y~ .  ,family=binomial(link=logit),data=train_set) 
summary(train_glm) 

	#逐步法
train_logit.step<-step(train_glm,direction="both") 
summary(train_logit.step) 
coefficients(train_logit.step)

	#估計測試集的預測機率	
pred<-predict(train_glm,newdata=test.X,type="response")

#---------------------------------------------------------------
#2.訓練後邏輯斯模型正確預測率  #利用ROC圖檢測模型預測能力
#---------------------------------------------------------------
1-mean(round(pred) != test.Y)

library(ROCR)

	#ROC圖
testmodel=prediction(pred,test.Y)
plot(performance(testmodel,'tpr','fpr'))  #true/false positive rate
abline(0,1, lty = 8, col = "grey",lwd=2)
text(0.5,0.55,"AUC=0.73")

	#AUC
auc <- performance(testmodel, "auc")
auc #=0.7170608   # 0.5 < AUC < 1，優於隨機猜測。


#=========================================================
#3.用??% 作為分群的依據時,模型正確預測率(預測被停真的被停)最高?--------------#
#=========================================================

Pro.02<-seq(0.01,0.9,0.01)
Default_predicted_rate<-numeric(length(Pro.02))
for(i in 1:length(Pro.02))
{
Index<-which(pred>=Pro.02[i])
predict.02<-rep(0,length(test.Y))
predict.02[Index]<-1
Confusion.matrix<-table(Truth=test.Y,Prediction=predict.02)
a<-which(test.Y!=1 | predict.02!=1)
b<-which(predict.02!=1)
Default_predicted_rate[i]<-(length(test.Y)-length(a))/(length(test.Y)-length(b))
print(c(i,Default_predicted_rate[i]))
}

	#最高模型正確預測率
max(Default_predicted_rate)

	#以圖形展現不同預測停卡率(停卡高低風險群界線)對正確預測率的變化
plot(Pro.02,Default_predicted_rate,pch=19,lwd=1,
		ylab="測試集模型正確預測率",xlab="預測停卡率")
lines(Pro.02,Default_predicted_rate,lwd=3,lty=2,col=4)
abline(h=0.8,col=2,lwd=3)
abline(v=0.61,col=6,lwd=3,lty=2)
text(0.4,0.78,"模型正確預測率=80%")
text(0.6,0.35,"停卡率61%")

#-----------------------------------------------------------------
#當收集到客戶資料,以下值帶入模型，若求得預測退卡機率>61%=>則歸類為易被停卡的觀察名單.
#-----------------------------------------------------------------
	#測試集中有2.3%的客戶預測退卡機率>61%=>觀察名單.
d<-which(pred>=0.61)
length(d)/length(pred) #=2.3%


#==============
#4.SVM vs. 決策樹 ---------------------------------------------------------#
#==============
library(e1071)
library(rpart)

	## <svm>
svm.model <- svm(train.Y ~ ., data = train_set, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, test.X) 

A=table(pred = round(svm.pred), true = test.Y)
A[1,2]/sum(A[,2]) #計算svm 正確預測率.

	## <rpart>
rpart.model <- rpart(train.Y ~ ., data = train_set)
rpart.pred <- predict(rpart.model, test.X) #

B=table(pred = round(rpart.pred), true = test.Y)
B[1,2]/sum(B[,2])#計算tree 正確預測率.



#==========================
#5.--樸素貝葉斯 (Naive Bayes)----------------------------------------------##
#==========================
nb=naiveBayes(train.Y~.,data=train_set) #categorical variables.
test_set<-data.frame(test.X,test.Y)

nb.pred <- predict(nb,test_set,type=c("raw")) 
classification <- ceiling(nb.pred[,2]-nb.pred[,1]) #判斷分類，分到機率大的。

1-mean(classification != test.Y) #正確預測率.

#====================================
#6. K鄰近算法（KNN）                  ----------------------------------------##
#====================================

#找尋好的K值
library(class)

k<-1:50
Rate<-numeric(length(k))
for(i in k)
{
set.seed(1)
KNN_k<-knn(train=train.X, test=test.X, cl=train.Y, k=i)
Confusion.matrix<-table(Truth=test.Y,Prediction=KNN_k)
ifelse(sum(Confusion.matrix[,2])!=0,
       Rate[i]<-sum(diag(Confusion.matrix))/sum(Confusion.matrix),
       Rate[i]<-0)
print(i)
}
Rate  #最佳K值=14
max(Rate)

	#圖示法, 展示最佳K值
plot(k,Rate,type="l",lty=2,lwd=2,col=3,ylab="命中率",xlab="最近鄰數量(k)")
points(k,Rate,pch=19,lwd=2)
abline(h=0.7938333,col=2,lwd=3)
abline(v=14,col=6,lwd=3,lty=2)
text(25,0.792,"0.79")
text(13,0.76,"最佳k值=14")

#  
library(kknn)

knn.model <- kknn(train.Y+test.Y~. , train_set,test_set, na.action = na.omit(),
	k=14, distance=2 ,kernel="optimal" , ykernel = NULL, scale=TRUE,)

fit <- fitted(knn.model)
table(test.Y, round(fit))

1-mean((test.Y != round(fit)))#正確預測率.







