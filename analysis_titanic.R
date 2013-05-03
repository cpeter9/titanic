# Analysis of the titanic dataset

library(caret)
# install.packages("gbm")
library(gbm)

train <- read.csv("C:/Users/Chris/Desktop/RCode/titanic/train.csv")
train$survived <- as.factor(train$survived)
train$pclass <- as.ordered(train$pclass)

inTrain <- createDataPartition(train$survived,
                    list = FALSE,
                    p = 0.7)

training <- train[inTrain, -c(3, 8)]
testing <- train[-inTrain, -c(3, 8)]

# gbm_1 <- train(survived ~ .,
#       data = training,
#       method = "gbm",
#       distribution = "bernoulli",
#       shrinkage = 0.001,
#       interaction.depth = 3,
#       cv.folds = 3,
#       verbose = TRUE)

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     verboseIter = T,
                     classProbs = T,
                     summaryFunction = twoClassSummary)

install.packages("pls")
library(pls)

pls_1 <- train(survived ~ .,
               data = training,
               method = "pls",
               tuneLength = 15,
               trControl = ctrl,
               metric = "ROC")

confusionMatrix(pls_1)

glm_1 <- train(survived ~ .,
               data = training[ , -c(8,9)],
               method = "glmnet",
               family = "binomial",
               tuneLength = 5,
               lambda = seq()
               trControl = ctrl,
               metric = "ROC")

confusionMatrix(glm_1)

# install.packages("mboost")
library(mboost)

glmboost_1 <- train(survived ~ .,
               data = training[ , -c(8,9)],
               method = "glmboost",
               tuneLength = 5,
               trControl = ctrl,
               metric = "ROC")

confusionMatrix(glmboost_1)
               