#========================
#2005/04~09�x�W�H�Υd�Τ���----------------------------------------------#
#========================

(credit = read.csv("creditcard_default_kaggle.csv",header=T))

	#�˥��� 3(�U),�ܼ�25��
dim(credit)
names(credit)
	
	#�ԭz�έp�q
summary(credit)

	
	#logistic�^�k�ҫ�

glm <- glm(credit[,25]~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
	PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6,family=binomial(link=logit),
	data=credit) 
summary(glm) 


	#�v�B�k
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

	#Signif. codes:  0 ��***�� 0.001 ��**�� 0.01 ��*�� 0.05 ��.�� 0.1 �� �� 1


logit.step<-step(glm,direction="both") #forward/backward.
summary(logit.step) 
coefficients(logit.step)

		#���d���v=exp(b0+b1x1+...)/(1+exp(b0+b1x1+...))

		#�k��(sex=1)�Q���d��odds,�O�k�ʪ� exp(0.09298)=1.1�� (���X1��)
		      

		#���d���v����: (�H���B�קC)�B�k�ʡB�Ш|�{�װ��B�w�B�B�~���j�B���L�쩵�I�ڰO�����H



	#�p��˥��Q���d�����=22.12%

table(credit[,25])/dim(credit)[1]



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
		#Signif. codes:  0 ��***�� 0.001 ��**�� 0.01 ��*�� 0.05 ��.�� 0.1 �� �� 1

train_logit.step<-step(train_glm,direction="both") 
summary(train_logit.step) 
coefficients(train_logit.step)


	#�N���ն����w�����v���p�X��
	
pred<-predict(train_glm,newdata=test.X,type="response")
pred[1:10]

	#���ն������Q���d�v(�w��)=22.1% -> ����˥����d�v=22.12%
mean(pred) 

#---------------------------------------------------------------
#	�V�m���޿贵�ҫ����T�w���v=81.18%  #�Q��ROC���˴��ҫ��w����O
#---------------------------------------------------------------
1-mean(round(pred) != test.Y)

#
library(ROCR)

	#ROC��
testmodel=prediction(pred,test.Y)
plot(performance(testmodel,'tpr','fpr'))  #true/false positive rate
abline(0,1, lty = 8, col = "grey",lwd=2)
text(0.5,0.55,"AUC=0.73")

	#ROC�Ϧ��u�U���n[�V�j->�w���V�n]
auc <- performance(testmodel, "auc")
auc #=0.7170608   # 0.5 < AUC < 1�A�u���H���q���C


#==============================================
#�μ˥�����22.12%�@�����s���̾�(���d���C���I�s)--->[�D�[] ------------------------------#
#==============================================

	#�l�ܰ��d���v�W�L�˥����d�v=22.12%���Ȥ�W��(���d�����I�s)

Index<-which(pred>=0.2212)
predict.01<-rep(0,length(test.Y))
predict.01[Index]<-1

mean(predict.01) #3��7�����ն����Ȥ��ݩ󰱥d�����I�s


	#�ڭ̯u�����ߪ��O, �b�ڭ̹w���|�Q���d���Ȥᤤ, ��ڤW�u���Q������ҬO�h��? 
	#���׬O38.06%
	
	#�إ߲V�c�x�}
Confusion.matrix<-table(Truth=test.Y,Prediction=predict.01)
Confusion.matrix

	#�ҫ��w�����T�v=69.15%
#(i)
specification <- 
	(Confusion.matrix[1,1]+Confusion.matrix[2,2])/sum(Confusion.matrix[,])
specification
#(ii)
1-mean(test.Y!=predict.01)

Confusion.matrix[2,2]/sum(Confusion.matrix[,2]) #���׬O38.06%



#=========================================================
#��??% �@�����s���̾�(���d���C���I�s)��,�ҫ��`���T�w���v�̰�?--->[���[]-------------------#
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

	#�̰��ҫ����T�w���v=82.2%
max(Accurately_predicted_rate)

	#82.2%�����쪺�Oi=41-->�Y pro=0.01+0.01*41= 42%
	#-->��42% �@�����s���̾�(���d���C���I�s)��,�ҫ����T�w���v�̰�,��82.2%



	#�H�ϧήi�{���P�w�����d�v(���d���C���I�s�ɽu)�勵�T�w���v���ܤ�
plot(Pro,Accurately_predicted_rate,pch=19,lwd=3,
		ylab="�w����ն����ҫ����T�w���v",xlab="�w�����d�v")
lines(Pro,Accurately_predicted_rate,lwd=1,lty=2,col=4)
abline(h=0.6915,col=2)
abline(v=0.2212,col=6,lwd=2,lty=2)
text(0.15,0.72,"�ҫ����T�w���v=69.15%")
text(0.2,0.35,"�ɽu22.12%")

abline(h=0.822,col=2,lwd=4,lty=1)
abline(v=0.42,col=6,lwd=4,lty=2)
text(0.3,0.8,"�̰��ҫ����T�w���v=82.2%")
text(0.45,0.35,"�ɽu42%")


#=============================================================
#���d���v�W�L42%, �b�ڭ̹w���|�Q���d���Ȥᤤ, ��ڤW�u���Q������ҬO�h��? 
	#���׬O67.53%>>38.06%
#=============================================================
Index<-which(pred>=0.42)
predict.01<-rep(0,length(test.Y))
predict.01[Index]<-1

	#�إ߲V�c�x�}
Confusion.matrix<-table(Truth=test.Y,Prediction=predict.01)
Confusion.matrix
#(i)
Confusion.matrix[2,2]/sum(Confusion.matrix[,2]) #���׬O67.53%

#(ii)
a<-which(test.Y!=1 | predict.01!=1)
b<-which(predict.01!=1)
(length(test.Y)-length(a))/(length(test.Y)-length(b))

#=========================================================
#��??% �@�����s���̾ڮ�,�ҫ����T�w���v(�w���Q���u���Q��)�̰�?--->[���[]-----------#
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

	#�̰��ҫ����T�w���v=74.26%
max(Default_predicted_rate)

	#74.26%�����쪺�Oi=52-->�Y pro=0.01+0.01*52= 53%
	#-->��53% �@�����s���̾�(���d���C���I�s)��,�ҫ����T�w���v�̰�,��74.26%



	#�H�ϧήi�{���P�w�����d�v(���d���C���I�s�ɽu)�勵�T�w���v���ܤ�
plot(Pro.02,Default_predicted_rate,pch=19,lwd=1,
		ylab="���ն��ҫ����T�w���v",xlab="�w�����d�v")
lines(Pro.02,Default_predicted_rate,lwd=3,lty=2,col=4)
abline(h=0.8,col=2,lwd=3)
abline(v=0.61,col=6,lwd=3,lty=2)
text(0.4,0.78,"�ҫ����T�w���v=80%")
text(0.6,0.35,"���d�v61%")

abline(h=0.6753,col=2,lwd=4,lty=1)
abline(v=0.42,col=6,lwd=4,lty=2)
text(0.3,0.65,"�̰��ҫ����T�w���v=67.53%")
text(0.4,0.35,"�ɽu42%")

#-----------------------------------------------------------------
#��������Ȥ���,�H�U�ȱa�J�ҫ��A�Y�D�o�w���h�d���v>53%=>�h�k�������Q���d���[��W��.
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

	#���ն�����5.67%���Ȥ�w���h�d���v>53%=>�[��W��.
d<-which(pred>=0.53)
length(d)/length(pred) #=5.67%


#==============
#SVM vs. �M���� ---------------------------------------------------------#
#==============
library(e1071)
library(rpart)

	## <svm>
svm.model <- svm(train.Y ~ ., data = train_set, cost = 100, gamma = 1)
#svm.pred <- predict(svm.model, test.X) 

## compute <svm> confusion matrix
#table(pred = round(svm.pred), true = test.Y)

#mean(round(svm.pred) != test.Y) #�p��svm ���~�w���v.

	## <rpart>
rpart.model <- rpart(train.Y ~ ., data = train_set)
rpart.pred <- predict(rpart.model, test.X) #

	## compute <rpart> confusion matrix
A=table(pred = round(rpart.pred), true = test.Y)

1-mean(round(rpart.pred) != test.Y) 

A[1,2]/sum(A[,2])#�p��tree ���T�w���v.



#==========================
#5.--��������� (Naive Bayes)----------------------------------------------##
#==========================
nb=naiveBayes(train.Y~.,data=train_set) #categorical variables.
	#class(nb)
	#str(nb)

test_set<-data.frame(test.X,test.Y)
nb.pred <- predict(nb,test_set,type=c("raw")) #�P�_�����A������v�j���C

classification <- ceiling(nb.pred[,2]-nb.pred[,1])#�P�_�����A������v�j���C

1-mean(classification != test.Y) #���T�w���v.


#====================================
#6. K�F���k�]KNN�^                  ----------------------------------------##
#====================================

	#��k�y�z�G
	#�]1�^ �p��w�����O�ƾڤΤ����I�P���e�I���Z���F
	#�]2�^ ���Z�����W���ǱƧ�
	#�]3�^ ����P���e�I�Z���̤p��k���I
	#�]4�^ �T�w�eK���I�Ҧb���O�X�{���W�v
	#�]5�^ ��^�W�v�̰������O�@�����e���O���w��

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

pcol <- as.character(as.numeric(test.Y))

#pairs(testset[7:9], pch = pcol, col = c("green3", "red")
#	[(testset$race != round(fit))+1])

1-mean((test.Y != round(fit)))#���T�w���v.






