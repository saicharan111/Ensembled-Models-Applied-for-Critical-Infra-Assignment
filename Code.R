install.packages("readxl")
install.packages("caret")
library(readxl)
library(caret)

# df1,df2,df3 stroes the results of C5.0,SVM and RandomForest Respectively.

df1 <- data.frame(Iteration=integer(),
                 Accuracy=double(),
                 FPR=double(), FNR=double(), FalsePos=integer(), FalseNeg=integer())
df2 <- data.frame(Iteration=integer(),
                 Accuracy=double(),
                 FPR=double(),FNR=double(),FalsePos=integer(), FalseNeg=integer())
df3 <- data.frame(Iteration=integer(),
                 Accuracy=double(),
                 FPR=double(),FNR=double(),FalsePos=integer(), FalseNeg=integer())

df4 <- data.frame(Iteration=integer(),
                  Accuracy=double(),
                  FPR=double(),FNR=double(),FalsePos=integer(), FalseNeg=integer())

df5 <- data.frame(Iteration=integer(),
                  Accuracy=double(),
                  FPR=double(),FNR=double(),FalsePos=integer(), FalseNeg=integer())


HW_TESLA <- read_excel("/Users/charan/R-Repo/HW_TESLA.xlt")
HW_TESLA$STATIC=as.factor(HW_TESLA$STATIC)
View(HW_TESLA)

for(i in 1:10)
{
set.seed(4150)
k<-sample(nrow(HW_TESLA))
HW_TESLA<-HW_TESLA[k,]
#View(HW_TESLA)
#print("Selecting train and test data set")
set.seed(4150)
splitIndex <- createDataPartition(HW_TESLA$STATIC, p = .75,list = FALSE,times = 1)
trainSplit      <- HW_TESLA[splitIndex,]
testSplit       <- HW_TESLA[-splitIndex,]
rm( splitIndex )
#print("Done")
#View(trainSplit)

### Common Parametres setting
control <- trainControl(method="repeatedcv", number=3, repeats=3)
seed <- 4150
metric <- "Accuracy"

### C5.0 ###
fit.c50 <- train(STATIC~., data=trainSplit, method="C5.0", metric=metric, trControl=control)
### Testing C50 ####
c50Pred<-predict(fit.c50,testSplit)
a1<-confusionMatrix(c50Pred,testSplit$STATIC)
print(sprintf("C 5.0 Results for Iteration: %d", i))
print(sprintf("Accuracy: %f", a1$overall[1]))
print(sprintf("False Positive Ratio: %f", a1$table[1,2]/(a1$table[1,2]+a1$table[2,2])))
print(sprintf("False Negative Ratio: %f", a1$table[2,1]/(a1$table[2,1]+a1$table[1,1])))
print(sprintf("Number of False Positive: %d", a1$table[1,2]))
print(sprintf("Number of False Negatives: %d", a1$table[2,1]))
df1[i,1]<-i
df1[i,2]<-a1$overall[1]
df1[i,3]<-a1$table[1,2]/(a1$table[1,2]+a1$table[2,2])
df1[i,4]<-a1$table[2,1]/(a1$table[2,1]+a1$table[1,1])
df1[i,5]<-a1$table[1,2]
df1[i,6]<-a1$table[2,1]


### SVM ###
fit.SVM <- train(STATIC~., data=trainSplit, method="svmRadial", metric=metric, trControl=control)
### Testing SVM ####
SVMPred<-predict(fit.SVM,testSplit)
a2<-confusionMatrix(SVMPred,testSplit$STATIC)
print(sprintf("SVM Results for Iteration: %d", i))
print(sprintf("Accuracy: %f", a2$overall[1]))
print(sprintf("False Positive Ratio: %f", a2$table[1,2]/(a2$table[1,2]+a2$table[2,2])))
print(sprintf("False Negative Ratio: %f", a2$table[2,1]/(a2$table[2,1]+a2$table[1,1])))
print(sprintf("Number of False Positive: %d", a2$table[1,2]))
print(sprintf("Number of False Negatives: %d", a2$table[2,1]))
df2[i,1]<-i
df2[i,2]<-a2$overall[1]
df2[i,3]<-a2$table[1,2]/(a2$table[1,2]+a2$table[2,2])
df2[i,4]<-a2$table[2,1]/(a2$table[2,1]+a2$table[1,1])
df2[i,5]<-a2$table[1,2]
df2[i,6]<-a2$table[2,1]

### RF ###
fit.rf <- train(STATIC~., data=trainSplit, method="rf", metric=metric, trControl=control)
### Testing cart ####
rfPred<-predict(fit.rf,testSplit)
a3<-confusionMatrix(rfPred,testSplit$STATIC)
print(sprintf("Random Forest Results for Iteration: %d", i))
print(sprintf("Accuracy: %f", a3$overall[1]))
print(sprintf("False Positive Ratio: %f", a3$table[1,2]/(a3$table[1,2]+a3$table[2,2])))
print(sprintf("False Negative Ratio: %f", a3$table[2,1]/(a3$table[2,1]+a3$table[1,1])))
print(sprintf("Number of False Positive: %d", a3$table[1,2]))
print(sprintf("Number of False Negatives: %d", a3$table[2,1]))
df3[i,1]<-i
df3[i,2]<-a3$overall[1]
df3[i,3]<-a3$table[1,2]/(a3$table[1,2]+a3$table[2,2])
df3[i,4]<-a3$table[2,1]/(a3$table[2,1]+a3$table[1,1])
df3[i,5]<-a3$table[1,2]
df3[i,6]<-a3$table[2,1]


apply(HW_TESLA,2,var)
pca =prcomp(HW_TESLA[,2:133])
std_dev = pca$sdev
pr_var = std_dev^2
prop_varex <- pr_var/sum(pr_var)

# Simple plot b/w Principal Components and Variance.
#plot(cumsum(prop_varex), xlab = "Principal Component", 
#     ylab = "Cumulative Proportion of Variance Explained", type = "b")

# First 20 to 30 Principle Components are accountable for almost 100 % of the variance
# So we are considering only those 15 Principle Components For classification

### C5.0 for PCA based Results ###
fit1.c50PCA <- train(STATIC~., data=trainSplit[,1:25], method="C5.0", metric=metric, trControl=control)
### Testing C50 ####
c50Pred1PCA<-predict(fit1.c50PCA,testSplit[,1:25])
confusionMatrix(c50Pred1PCA,testSplit$STATIC)
a4<-confusionMatrix(c50Pred1PCA,testSplit$STATIC)
print(sprintf("C5.0 with PCA Results for Iteration: %d", i))
print(sprintf("Accuracy: %f", a4$overall[1]))
print(sprintf("False Positive Ratio: %f", a4$table[1,2]/(a4$table[1,2]+a4$table[2,2])))
print(sprintf("False Negative Ratio: %f", a4$table[2,1]/(a4$table[2,1]+a4$table[1,1])))
print(sprintf("Number of False Positive: %d", a4$table[1,2]))
print(sprintf("Number of False Negatives: %d", a4$table[2,1]))
df4[i,1]<-i
df4[i,2]<-a4$overall[1]
df4[i,3]<-a4$table[1,2]/(a4$table[1,2]+a4$table[2,2])
df4[i,4]<-a4$table[2,1]/(a4$table[2,1]+a4$table[1,1])
df4[i,5]<-a4$table[1,2]
df4[i,6]<-a4$table[2,1]
### Random Forest for PCA based Results ###
fit1.rfPCA <- train(STATIC~., data=trainSplit[,1:25], method="rf", metric=metric, trControl=control)
### Testing C50 ####
rfPred1PCA<-predict(fit1.rfPCA,testSplit[,1:25])
confusionMatrix(rfPred1PCA,testSplit$STATIC)
a5<-confusionMatrix(rfPred1PCA,testSplit$STATIC)
print(sprintf("Random Forest with PCA Results for Iteration: %d", i))
print(sprintf("Accuracy: %f", a5$overall[1]))
print(sprintf("False Positive Ratio: %f", a5$table[1,2]/(a5$table[1,2]+a5$table[2,2])))
print(sprintf("False Negative Ratio: %f", a5$table[2,1]/(a5$table[2,1]+a5$table[1,1])))
print(sprintf("Number of False Positive: %d", a5$table[1,2]))
print(sprintf("Number of False Negatives: %d", a5$table[2,1]))
df5[i,1]<-i
df5[i,2]<-a5$overall[1]
df5[i,3]<-a5$table[1,2]/(a5$table[1,2]+a5$table[2,2])
df5[i,4]<-a5$table[2,1]/(a5$table[2,1]+a5$table[1,1])
df5[i,5]<-a5$table[1,2]
df5[i,6]<-a5$table[2,1]

}

#################### PCA ######################3

apply(HW_TESLA,2,var)
pca =prcomp(HW_TESLA[,2:133]) 
std_dev = pca$sdev
pr_var = std_dev^2
prop_varex <- pr_var/sum(pr_var)

# Simple plot b/w Principal Components and Variance.
#plot(cumsum(prop_varex), xlab = "Principal Component", 
#     ylab = "Cumulative Proportion of Variance Explained", type = "b")

# First 20 to 30 Principle Components are accountable for almost 100 % of the variance
# So we are considering only those 15 Principle Components For classification

### C5.0 for PCA based Results ###
fit1.c50 <- train(STATIC~., data=trainSplit[,1:25], method="C5.0", metric=metric, trControl=control)
### Testing C50 ####
c50Pred1<-predict(fit1.c50,testSplit[,1:25])
confusionMatrix(c50Pred1,testSplit$STATIC)
print(sprintf("False Positive Ratio: %f", a$table[1,2]/(a$table[1,2]+a$table[2,2])))
print(sprintf("False Negative Ratio: %f", a$table[2,1]/(a$table[2,1]+a$table[1,1])))
