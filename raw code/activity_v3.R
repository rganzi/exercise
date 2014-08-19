## final code

fileurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
filename <- "pml-training.csv"

downloadData <- function() {
        if (!file.exists(filename)) {
                download.file(fileurl, dest=filename, method="curl")
        }
        data <- read.csv("pml-training.csv", header = TRUE)
        data
}

activity <- downloadData()

## remove user, dates/times
activity.m <- activity[, -c(1:7)]

## remove summary variables (avg & sd)
navalues <- is.na(activity.m[1, ])
activity.mi <- activity.m[, !navalues]


## change 2, 200-300 level factor variables to numeric
for(i in 1:dim(activity.mi)[2]-1) {
        if(is.factor(activity.mi[, i])==TRUE) {
                activity.mi[, i] <- as.numeric(activity.mi[, i])
        }
}

activity.min <- activity.mi

library(caret)

set.seed(3232)
inTrain <- createDataPartition(activity.min$classe, p=0.7, list=FALSE)

training <- activity.min[inTrain, ]
testing <- activity.min[-inTrain, ]

## near zero variance vars
nsv <- nearZeroVar(training, saveMetrics=TRUE)

nzvar <- nsv$nzv
training <- training[, !nzvar]

##
library(doMC)

y <- training$classe
x <- training[, -dim(training)[2]]

normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)

subsets <- c(5:10,15,20,25)

set.seed(21)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   number = 10,
                   repeats = 5,
                   verbose = FALSE)

registerDoMC(cores = 4)
set.seed(126)
rfProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)

best <- predictors(rfProfile)
rfProfile

## rfProfile plot
plot(rfProfile, type = c("g", "o"))

matches <- as.integer()

indices <- function(n) {
        ind <- best[1:n]
        
        for(i in 1:n) {
                mat <- match(ind[i], names(training))
                matches <- c(matches, mat)
        }
        matches
}

indexes <- indices(25)
training.min <- training[, c(indexes, dim(training)[2])]

## set up new predictors data.frame, x1, based on training.min
x1 <- training.min[, -dim(training.min)[2]]

normalization1 <- preProcess(x1)
x1 <- predict(normalization1, x1)
x1 <- as.data.frame(x1)

## from trainControl example
set.seed(1235)

seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)

## 10-fold cv, repeated 5 times
ctrl.rf <- trainControl(method = "repeatedcv", 
                        repeats = 5,
                        number = 10,
                        seeds = seeds)

registerDoMC(cores = 4)
set.seed(2)

## fit model "rf"
rf <- train(x1, y,
            method = "rf",
            ntree=500,
            trControl = ctrl.rf)

## prediction/classe table
pred <- predict(rf, testing)
table(pred, testing$classe)

## out-of-sample accuracy rate
predRight <- pred == testing$classe
sum(predRight)/nrow(testing)

library(randomForest)
rf.test <- randomForest(training$classe ~., data = training)

pred.rf.test <- predict(rf.test, testing)
table(pred.rf.test, testing$classe)

pred.rf.Right <- pred.rf.test == testing$classe
sum(pred.rf.Right)/nrow(testing) 

## Ada Boost

registerDoMC(cores = 4)

glmb <- train(training$classe ~ ., data = training, method = "glmboost")

pred.glmb <- predict(glmb, testing)
table(pred.glmb, testing$classe)

pred.glmb.Right <- pred.glmb == testing$classe
sum(pred.glmb.Right) / nrow(testing)