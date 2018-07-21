#========================
#2005/04~09台灣信用卡用戶資料----------------------------------------------#
#========================

(credit = read.csv("creditcard_default_kaggle.csv",header=T))

	#樣本數 3(萬),變數25個
dim(credit)
names(credit)
	
	#敘述統計量
summary(credit)

	
	#logistic回歸模型

glm <- glm(credit[,25]~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
	PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6,family=binomial(link=logit),
	data=credit) 
summary(glm) 


	#逐步法
		#Coefficients:
		#              Estimate Std. Error z value Pr(>|z|)    
		#(Intercept) -7.357e-01  1.178e-01  -6.248 4.15e-10 ***
		#LIMIT_BAL   -1.812e-06  1.374e-07 -13.182  < 2e-16 ***
		#SEX         -9.298e-02  3.051e-02  -3.048  0.00231 ** 
		#EDUCATION   -1.202e-01  2.074e-02  -5.793 6.92e-09 ***
		#MARRIAGE    -1.671e-01  3.150e-02  -5.305 1.13e-07 ***
		#AGE          7.584e-03  1.767e-03   4.291 1.78e-05 ***
		#PAY_0        5.983e-01  1.779e-02  33.634  < 2e-16 ***
		#PAY_2        6.816e-02  1.989e-02   3.427  0.00061 ***
		#PAY_3        7.883e-02  2.000e-02   3.943 8.06e-05 ***
		#PAY_5        3.718e-02  1.734e-02   2.145  0.03199 *  

	#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


logit.step<-step(glm,direction="both") #forward/backward.
summary(logit.step) 
coefficients(logit.step)

		#停卡機率=exp(b0+b1x1+...)/(1+exp(b0+b1x1+...))

		#男性(sex=1)被停卡的odds,是女性的 exp(0.09298)=1.1倍 (高出1成)
		      

		#停卡機率較高: (信用額度低)、男性、教育程度高、已婚、年紀大、有過拖延付款記錄的人



	#計算樣本被停卡的比例=22.12%

table(credit[,25])/dim(credit)[1]



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
		#Coefficients:
		#              Estimate Std. Error z value Pr(>|z|)    
		#(Intercept) -7.507e-01  1.316e-01  -5.705 1.16e-08 ***
		#LIMIT_BAL   -1.861e-06  1.543e-07 -12.058  < 2e-16 ***
		#SEX         -8.758e-02  3.422e-02  -2.559 0.010488 *  
		#EDUCATION   -1.200e-01  2.315e-02  -5.182 2.19e-07 ***
		#MARRIAGE    -1.674e-01  3.518e-02  -4.758 1.96e-06 ***
		#AGE          7.577e-03  1.986e-03   3.816 0.000136 ***
		#PAY_0        5.890e-01  1.986e-02  29.654  < 2e-16 ***
		#PAY_2        9.487e-02  2.205e-02   4.303 1.68e-05 ***
		#PAY_3        9.277e-02  2.036e-02   4.556 5.21e-06 ***
		#---
		#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

train_logit.step<-step(train_glm,direction="both") 
summary(train_logit.step) 
coefficients(train_logit.step)


	#將測試集的預測機率估計出來
	
pred<-predict(train_glm,newdata=test.X,type="response")
pred[1:10]

	#測試集平均被停卡率(預測)=22.1% -> 接近樣本停卡率=22.12%
mean(pred) 

#---------------------------------------------------------------
#	訓練後邏輯斯模型正確預測率=81.18%  #利用ROC圖檢測模型預測能力
#---------------------------------------------------------------
1-mean(round(pred) != test.Y)

#
library(ROCR)

	#ROC圖
testmodel=prediction(pred,test.Y)
plot(performance(testmodel,'tpr','fpr'))  #true/false positive rate
abline(0,1, lty = 8, col = "grey",lwd=2)
text(0.5,0.55,"AUC=0.73")

	#ROC圖曲線下面積[越大->預測越好]
auc <- performance(testmodel, "auc")
auc #=0.7170608   # 0.5 < AUC < 1，優於隨機猜測。


#==============================================
#用樣本平均22.12%作為分群的依據(停卡高低風險群)--->[主觀] ------------------------------#
#==============================================

	#追蹤停卡機率超過樣本停卡率=22.12%的客戶名單(停卡高風險群)

Index<-which(pred>=0.2212)
predict.01<-rep(0,length(test.Y))
predict.01[Index]<-1

mean(predict.01) #3成7的測試集的客戶屬於停卡高風險群


	#我們真正關心的是, 在我們預測會被停卡的客戶中, 實際上真的被停的比例是多少? 
	#答案是38.06%
	
	#建立混淆矩陣
Confusion.matrix<-table(Truth=test.Y,Prediction=predict.01)
Confusion.matrix

	#模型預測正確率=69.15%
#(i)
specification <- 
	(Confusion.matrix[1,1]+Confusion.matrix[2,2])/sum(Confusion.matrix[,])
specification
#(ii)
1-mean(test.Y!=predict.01)

Confusion.matrix[2,2]/sum(Confusion.matrix[,2]) #答案是38.06%



#=========================================================
#用??% 作為分群的依據(停卡高低風險群)時,模型總正確預測率最高?--->[客觀]-------------------#
#=========================================================

Pro<-seq(0.01,0.9,0.01)
for(i in 1:length(Pro))
{
Index<-which(pred>=Pro[i])
predict.01<-rep(0,length(test.Y))
predict.01[Index]<-1
Accurately_predicted_rate[i]<-1-mean(test.Y != predict.01) #specification.
print(c(i,Accurately_predicted_rate[i]))
}

	#最高模型正確預測率=82.2%
max(Accurately_predicted_rate)

	#82.2%對應到的是i=41-->即 pro=0.01+0.01*41= 42%
	#-->用42% 作為分群的依據(停卡高低風險群)時,模型正確預測率最高,為82.2%



	#以圖形展現不同預測停卡率(停卡高低風險群界線)對正確預測率的變化
plot(Pro,Accurately_predicted_rate,pch=19,lwd=3,
		ylab="針對測試集的模型正確預測率",xlab="預測停卡率")
lines(Pro,Accurately_predicted_rate,lwd=1,lty=2,col=4)
abline(h=0.6915,col=2)
abline(v=0.2212,col=6,lwd=2,lty=2)
text(0.15,0.72,"模型正確預測率=69.15%")
text(0.2,0.35,"界線22.12%")

abline(h=0.822,col=2,lwd=4,lty=1)
abline(v=0.42,col=6,lwd=4,lty=2)
text(0.3,0.8,"最高模型正確預測率=82.2%")
text(0.45,0.35,"界線42%")


#=============================================================
#停卡機率超過42%, 在我們預測會被停卡的客戶中, 實際上真的被停的比例是多少? 
	#答案是67.53%>>38.06%
#=============================================================
Index<-which(pred>=0.42)
predict.01<-rep(0,length(test.Y))
predict.01[Index]<-1

	#建立混淆矩陣
Confusion.matrix<-table(Truth=test.Y,Prediction=predict.01)
Confusion.matrix
#(i)
Confusion.matrix[2,2]/sum(Confusion.matrix[,2]) #答案是67.53%

#(ii)
a<-which(test.Y!=1 | predict.01!=1)
b<-which(predict.01!=1)
(length(test.Y)-length(a))/(length(test.Y)-length(b))

#=========================================================
#用??% 作為分群的依據時,模型正確預測率(預測被停真的被停)最高?--->[客觀]-----------#
#  53%                                            74.26%
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

	#最高模型正確預測率=74.26%
max(Default_predicted_rate)

	#74.26%對應到的是i=52-->即 pro=0.01+0.01*52= 53%
	#-->用53% 作為分群的依據(停卡高低風險群)時,模型正確預測率最高,為74.26%



	#以圖形展現不同預測停卡率(停卡高低風險群界線)對正確預測率的變化
plot(Pro.02,Default_predicted_rate,pch=19,lwd=1,
		ylab="測試集模型正確預測率",xlab="預測停卡率")
lines(Pro.02,Default_predicted_rate,lwd=3,lty=2,col=4)
abline(h=0.8,col=2,lwd=3)
abline(v=0.61,col=6,lwd=3,lty=2)
text(0.4,0.78,"模型正確預測率=80%")
text(0.6,0.35,"停卡率61%")

abline(h=0.6753,col=2,lwd=4,lty=1)
abline(v=0.42,col=6,lwd=4,lty=2)
text(0.3,0.65,"最高模型正確預測率=67.53%")
text(0.4,0.35,"界線42%")

#-----------------------------------------------------------------
#當收集到客戶資料,以下值帶入模型，若求得預測退卡機率>53%=>則歸類為易被停卡的觀察名單.
#-----------------------------------------------------------------
		#LIMIT_BAL   
		#SEX         
		#EDUCATION   
		#MARRIAGE    
		#AGE        
		#PAY_0       
		#PAY_2        
		#PAY_3        
		#PAY_5        

	#測試集中有5.67%的客戶預測退卡機率>53%=>觀察名單.
d<-which(pred>=0.53)
length(d)/length(pred) #=5.67%


#==============
#SVM vs. 決策樹 ---------------------------------------------------------#
#==============
library(e1071)
library(rpart)

	## <svm>
svm.model <- svm(train.Y ~ ., data = train_set, cost = 100, gamma = 1)
#svm.pred <- predict(svm.model, test.X) 

## compute <svm> confusion matrix
#table(pred = round(svm.pred), true = test.Y)

#mean(round(svm.pred) != test.Y) #計算svm 錯誤預測率.

	## <rpart>
rpart.model <- rpart(train.Y ~ ., data = train_set)
rpart.pred <- predict(rpart.model, test.X) #

	## compute <rpart> confusion matrix
A=table(pred = round(rpart.pred), true = test.Y)

1-mean(round(rpart.pred) != test.Y) 

A[1,2]/sum(A[,2])#計算tree 正確預測率.



#==========================
#5.--樸素貝葉斯 (Naive Bayes)----------------------------------------------##
#==========================
nb=naiveBayes(train.Y~.,data=train_set) #categorical variables.
	#class(nb)
	#str(nb)

test_set<-data.frame(test.X,test.Y)
nb.pred <- predict(nb,test_set,type=c("raw")) #判斷分類，分到機率大的。

classification <- ceiling(nb.pred[,2]-nb.pred[,1])#判斷分類，分到機率大的。

1-mean(classification != test.Y) #正確預測率.


#====================================
#6. K鄰近算法（KNN）                  ----------------------------------------##
#====================================

	#算法描述：
	#（1） 計算已知類別數據及中的點與當前點的距離；
	#（2） 按距離遞增次序排序
	#（3） 選取與當前點距離最小的k個點
	#（4） 確定前K個點所在類別出現的頻率
	#（5） 返回頻率最高的類別作為當前類別的預測

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

pcol <- as.character(as.numeric(test.Y))

#pairs(testset[7:9], pch = pcol, col = c("green3", "red")
#	[(testset$race != round(fit))+1])

1-mean((test.Y != round(fit)))#正確預測率.







