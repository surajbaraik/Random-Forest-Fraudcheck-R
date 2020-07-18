


install.packages("C50")
install.packages("tree")
install.packages("caret")
install.packages("gmodels")
install.packages("party")
install.packages("knitr")
install.packages("png")
library(C50)
library(tree)
library(caret)
library(gmodels)
library(party)
library(knitr)
library(png)
library(randomForest)

FraudCheck <- read.csv(file.choose())
View(FraudCheck)
hist(FraudCheck$Taxable.Income)

# Splitting data into training and testing.
# splitting the data based on Income
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)
View(FC)

FC_train <- FC[1:300,]

FC_test <- FC[301:600,]

# Building a random forest model on training data 
fit.forest <- randomForest(Risky_Good~.,data= FC_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(FC_train$Risky_Good==predict(fit.forest,FC_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,FC_train)
library(caret)


# Confusion Matrix
confusionMatrix(FC_train$Risky_Good, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=FC_test)
mean(pred_test==FC_test$Risky_Good) # Accuracy = 100 % 


# Confusion Matrix 

confusionMatrix(FC_test$Risky_Good, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:6,cex=0.8,fill=1:6)

