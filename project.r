#read in csv
getwd()
data <- read.csv('data.csv')

#no missing values
sum(is.na(data))

#make all category columns into factors
data$home_ownership_cat <- as.factor(data$home_ownership_cat)
data$income_cat <- as.factor(data$income_cat)
data$term_cat <- as.factor(data$term_cat)
data$application_type_cat <- as.factor(data$application_type_cat)
data$purpose_cat <- as.factor(data$purpose_cat)
data$interest_payment_cat <- as.factor(data$interest_payment_cat)
data$loan_condition_cat <- as.factor(data$loan_condition_cat)
data$grade_cat <- as.factor(data$grade_cat)

#look at summary of all variables
summary(data)

#home ownership 4, 5, and 6 should be combined to as one value of 4 that will represent "unknown"
data$home_ownership_cat
levels(data$home_ownership_cat) <- c("1", "2", "3", "4", "4", "4")
data$home_ownership_cat


#loan period should be discrete rather than continous values, round the numbers
data$loanPeriod <- round(data$loanPeriod, digits = 0)
data$loanPeriod
View(data)

install.packages('tree')
install.packages('ggplot2')
install.packages('dplyr')
#Added by Yeshu for decision tree on 03/24/2019
library(tree)
library(ggplot2)
library(dplyr)

#removed the first column.
data <- data[,-1]

#plotting the loan condition category to see the distribution 
options(scipen=5)
ggplot(data.frame(data), aes(x=loan_condition_cat)) +
  geom_bar(fill='pink')+theme_minimal()+geom_text(stat='count',aes(label=..count..))

#subsetting data as per the loan category into goodLoanData and badLoanData
goodLoanData <- subset(data, data$loan_condition_cat == 0)
badLoanData <- subset(data, data$loan_condition_cat == 1)

#to create train data set which contains 50% data of both good loan and bad loan I have sampled goodLoanData which has
#67429 observation which is count of bad loan obervations
sampleGoodLoanData <- sample_n(goodLoanData,67429)

#binding 50% of good loan and bad loan data to create trainData
trainData <- rbind(sampleGoodLoanData,badLoanData)
View(trainData)

# to create test data set I have selected 33% of the total train data to include 50% of good loan obervations and
# 50% of bad loan observations
sampleGoodLoanTestData <- sample_n(goodLoanData,22251)
sampleBadLoanTestData <- sample_n(badLoanData,22251)
testData <- rbind(sampleGoodLoanTestData,sampleBadLoanTestData)
View(testData)

# building decision tree
set.seed(106)
tree.loanData <- tree(loan_condition_cat~., data = trainData)
summary(tree.loanData)
plot(tree.loanData)
text(tree.loanData, splits = TRUE, all = FALSE ,pretty = 0)
tree.loanData

# predicting the decision tree
tree.loanDataPred <- predict(tree.loanData,testData,type = "class")
with(testData,table(tree.loanDataPred,loan_condition_cat))

# pruning the tree with the use of cross-validation
cv.loanData = cv.tree(tree.loanData, FUN = prune.misclass)
cv.loanData

plot(cv.loanData)
#Looking at the plot, you see a downward spiral part because of the misclassification error on cross-validated points.
#So let's pick a value in the downward steps (5). Then, let's prune the tree to a size of 12 to identify that tree. 
#Finally, let's plot and annotate that tree to see the outcome.

#pruning the tree to the size of 5
prune.loanData = prune.misclass(tree.loanData, best = 3)
plot(prune.loanData)
text(tree.loanData, splits = TRUE, all = FALSE ,pretty = 0)

#evaluating on test data set
tree.loanDataPred <- predict(prune.loanData,testData,type = "class")
with(testData,table(tree.loanDataPred,loan_condition_cat))

# Added by Yeaswi on 03/24/2019 for Naive Bayes
install.packages('RWeka')
library(RWeka)

NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") 
trainDataNB <- NN(data=trainData, control= Weka_control(R="1-3"), na.action = NULL)
testDataNB <- NN(data=testData, control= Weka_control(R="1,3"), na.action = NULL)

NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
WOW(NB)

nb.loanData=NB(loan_condition_cat~., data = trainData)
nb.loanData

nb.loanDataPred = predict (nb.loanData, newdata = testDataNB, type = c("class"))
nb.loanDataPred

#Added by Yesaswi on 03/24/2019 for random forests
install.packages('randomForest')
library(randomForest)

# random forests with default parameters
rf.loanData = randomForest(loan_condition_cat~., data = trainData, importance = TRUE)
rf.loanData

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:13) {
  model3 <- randomForest(loan_condition_cat~., data = trainData, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, testData, type = "class")
  a[i-2] = mean(predValid == testData$loan_condition_cat)
}

a

plot(3:13,a)

# accuracy was maximum at mtry = 10. So, we will use that model
#increased the mtry to 10 from 4
rf.loanDataTuned = randomForest(loan_condition_cat~., data = trainData, ntree = 500, mtry = 10, importance = TRUE)
rf.loanDataTuned

#predict on the train dataset first
rf.trainDataPred <- predict(rf.loanDataTuned, trainData, type = "class")
table(rf.trainDataPred, trainData$loan_condition_cat)

#prediction on test data
rf.testDataPred <- predict(rf.loanDataTuned, testData, type = "class", keep.forest=FALSE, importance = TRUE)

#checking the classification accuracy
mean(rf.testDataPred == testData$loan_condition_cat)
table(rf.testDataPred,testData$loan_condition_cat)

# checking important variables
importance(rf.loanDataTuned)
varImpPlot(rf.loanDataTuned)




############################ YESASWI NAIVE BAYES ###########################
install.packages('e1071')
install.packages('caret')
library(e1071)
library(caret)
str(trainData)
nbTrain <- naiveBayes(loan_condition_cat ~ ., data = trainData)
nbTrainPred <- predict(nbTrain, trainData, type = 'class')
confusionMatrix(nbTrainPred, as.factor(trainData$loan_condition_cat))

nbTrainPred <- predict(nbTrain, testData, type = 'class')
confusionMatrix(nbTrainPred, as.factor(testData$loan_condition_cat))
