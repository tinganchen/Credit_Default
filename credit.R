#========================
#2005/04~09�x�W�H�Υd�Τ���----------------------------------------------#
#========================

(credit = read.csv("creditcard_default_kaggle.csv",header=T))

	#�˥��� 3(�U),�ܼ�25��
dim(credit)
names(credit)
summary(credit)

#=================
#1.logistic�^�k�ҫ�------------------------------------------------------# 
#=================
glm <- glm(credit[,25]~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
	PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6,family=binomial(link=logit),
	data=credit) 
summary(glm) 

	#�v�B�k
logit.step<-step(glm,direction="both") #forward/backward.
summary(logit.step) 
coefficients(logit.step)


	#�ϥΰV�m�P����82�k�h, �h���V�m����:���յ���=8:2=24000:6000

	#�i���ƾ�z�P����(train & test)
default.01<-as.numeric(credit[,25])
index<-1:dim(credit)[1]
train.index <- sample(index,length(index)*0.8)

train.X<-credit[train.index,2:12]
train.Y<-default.01[train.index]
test.X<-credit[-train.index,2:12]
test.Y<-default.01[-train.index]

train_set<-data.frame(train.X,train.Y)
names(train_set)

	#�V�m.logistic�^�k�ҫ�
train_glm <- glm(train.Y~ .  ,family=binomial(link=logit),data=train_set) 
summary(train_glm) 

	#�v�B�k
train_logit.step<-step(train_glm,direction="both") 
summary(train_logit.step) 
coefficients(train_logit.step)

	#���p���ն����w�����v	
pred<-predict(train_glm,newdata=test.X,type="response")

#---------------------------------------------------------------
#2.�V�m���޿贵�ҫ����T�w���v  #�Q��ROC���˴��ҫ��w����O
#---------------------------------------------------------------
1-mean(round(pred) != test.Y)

library(ROCR)

	#ROC��
testmodel=prediction(pred,test.Y)
plot(performance(testmodel,'tpr','fpr'))  #true/false positive rate
abline(0,1, lty = 8, col = "grey",lwd=2)
text(0.5,0.55,"AUC=0.73")

	#AUC
auc <- performance(testmodel, "auc")
auc #=0.7170608   # 0.5 < AUC < 1�A�u���H���q���C


#=========================================================
#3.��??% �@�����s���̾ڮ�,�ҫ����T�w���v(�w���Q���u���Q��)�̰�?--------------#
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

	#�̰��ҫ����T�w���v
max(Default_predicted_rate)

	#�H�ϧήi�{���P�w�����d�v(���d���C���I�s�ɽu)�勵�T�w���v���ܤ�
plot(Pro.02,Default_predicted_rate,pch=19,lwd=1,
		ylab="���ն��ҫ����T�w���v",xlab="�w�����d�v")
lines(Pro.02,Default_predicted_rate,lwd=3,lty=2,col=4)
abline(h=0.8,col=2,lwd=3)
abline(v=0.61,col=6,lwd=3,lty=2)
text(0.4,0.78,"�ҫ����T�w���v=80%")
text(0.6,0.35,"���d�v61%")

#-----------------------------------------------------------------
#��������Ȥ���,�H�U�ȱa�J�ҫ��A�Y�D�o�w���h�d���v>61%=>�h�k�������Q���d���[��W��.
#-----------------------------------------------------------------
	#���ն�����2.3%���Ȥ�w���h�d���v>61%=>�[��W��.
d<-which(pred>=0.61)
length(d)/length(pred) #=2.3%


#==============
#4.SVM vs. �M���� ---------------------------------------------------------#
#==============
library(e1071)
library(rpart)

	## <svm>
svm.model <- svm(train.Y ~ ., data = train_set, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, test.X) 

A=table(pred = round(svm.pred), true = test.Y)
A[1,2]/sum(A[,2]) #�p��svm ���T�w���v.

	## <rpart>
rpart.model <- rpart(train.Y ~ ., data = train_set)
rpart.pred <- predict(rpart.model, test.X) #

B=table(pred = round(rpart.pred), true = test.Y)
B[1,2]/sum(B[,2])#�p��tree ���T�w���v.



#==========================
#5.--��������� (Naive Bayes)----------------------------------------------##
#==========================
nb=naiveBayes(train.Y~.,data=train_set) #categorical variables.
test_set<-data.frame(test.X,test.Y)

nb.pred <- predict(nb,test_set,type=c("raw")) 
classification <- ceiling(nb.pred[,2]-nb.pred[,1]) #�P�_�����A������v�j���C

1-mean(classification != test.Y) #���T�w���v.

#====================================
#6. K�F���k�]KNN�^                  ----------------------------------------##
#====================================

#��M�n��K��
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
Rate  #�̨�K��=14
max(Rate)

	#�ϥܪk, �i�̨ܳ�K��
plot(k,Rate,type="l",lty=2,lwd=2,col=3,ylab="�R���v",xlab="�̪�F�ƶq(k)")
points(k,Rate,pch=19,lwd=2)
abline(h=0.7938333,col=2,lwd=3)
abline(v=14,col=6,lwd=3,lty=2)
text(25,0.792,"0.79")
text(13,0.76,"�̨�k��=14")

#  
library(kknn)

knn.model <- kknn(train.Y+test.Y~. , train_set,test_set, na.action = na.omit(),
	k=14, distance=2 ,kernel="optimal" , ykernel = NULL, scale=TRUE,)

fit <- fitted(knn.model)
table(test.Y, round(fit))

1-mean((test.Y != round(fit)))#���T�w���v.






