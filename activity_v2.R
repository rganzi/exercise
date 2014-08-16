## Practical Machine Learning, Course Project

library(lubridate)
library(caret)

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
activity.m <- activity[, -c(1:6)]
navalues <- is.na(activity.m[1, ])
sum(navalues)
# names(activity)
activity <- activity[, -c(1,14,17,26,89,92,101,127,130,139)]

activity.m <- activity[, -c(1:7,14,17,26,89,92,101,127,130,139)]

## summary vars (avg & sd)
navalues <- is.na(activity.m[1, ])
sum(navalues) #67

activity.mi <- activity.m[, !navalues]

## change 200-300 level factor vars to numeric
str(activity.mi)

for(i in 1:76) {
        if(is.factor(activity.mi[, i])==TRUE) {
                activity.mi[, i] <- as.numeric(activity.mi[, i])
        }
}

activity.m <- activity[, -c(1:7)]

## summary variables (avg & sd)
navalues <- is.na(activity.m[1, ])
sum(navalues) #67

activity.mi <- activity.m[, !navalues]

## change 2, 200-300 level factor variables to numeric
for(i in 1:85) {
        if(is.factor(activity.mi[, i])==TRUE) {
                activity.mi[, i] <- as.numeric(activity.mi[, i])
        }
}

activity.min <- activity.mi

str(activity.min)


## bad vars
## kurtosis_yaw_belt
## 16 skewness_yaw_belt
## 25 amplitude_yaw_belt
## kurtosis_yaw_dumbbell
## skewness_yaw_dumbbell
## amplitude_yaw_dumbbell
## kurtosis_yaw_forearm
## skewness_yaw_forearm
## amplitude_yaw_forearm

# factor vars (with 300-400 levels) for some reason
activity$kurtosis_roll_belt <- as.numeric(activity$kurtosis_roll_belt)
activity$kurtosis_picth_belt <- as.numeric(activity$kurtosis_picth_belt)
activity$skewness_roll_belt <- as.numeric(activity$skewness_roll_belt)
activity$skewness_roll_belt.1 <- as.numeric(activity$skewness_roll_belt.1)
activity$max_yaw_belt <- as.numeric(activity$max_yaw_belt)
activity$min_yaw_belt <- as.numeric(activity$min_yaw_belt)

activity$kurtosis_roll_arm <- as.numeric(activity$kurtosis_roll_arm)
activity$kurtosis_picth_arm <- as.numeric(activity$kurtosis_picth_arm)
activity$kurtosis_yaw_arm <- as.numeric(activity$kurtosis_picth_arm)
activity$skewness_roll_arm <- as.numeric(activity$skewness_roll_arm)
activity$skewness_pitch_arm <- as.numeric(activity$skewness_pitch_arm)
activity$skewness_yaw_arm <- as.numeric(activity$skewness_yaw_arm)

activity$kurtosis_roll_dumbbell <- as.numeric(activity$kurtosis_roll_dumbbell)
activity$kurtosis_picth_dumbbell <- as.numeric(activity$kurtosis_picth_dumbbell)
activity$skewness_roll_dumbbell <- as.numeric(activity$skewness_roll_dumbbell)
activity$skewness_pitch_dumbbell <- as.numeric(activity$skewness_pitch_dumbbell)
activity$max_yaw_dumbbell <- as.numeric(activity$max_yaw_dumbbell)
activity$min_yaw_dumbbell <- as.numeric(activity$min_yaw_dumbbell)

activity$kurtosis_roll_forearm <- as.numeric(activity$kurtosis_roll_forearm)
activity$kurtosis_picth_forearm <- as.numeric(activity$kurtosis_picth_forearm)
activity$skewness_roll_forearm <- as.numeric(activity$skewness_roll_forearm)
activity$skewness_pitch_forearm <- as.numeric(activity$skewness_pitch_forearm)
activity$max_yaw_forearm <- as.numeric(activity$max_yaw_forearm)
activity$min_yaw_forearm <- as.numeric(activity$min_yaw_forearm)

# cc <- complete.cases(activity)
# sum(cc)

# summary(activity$min_roll_forearm)
# summary(activity$var_yaw_forearm)

activity.min <- activity[, -c(15:16,18:19,21:32,46:55,71:79,87:88,90:91,93:94,96:105,122:123,125:126,128:129,131:140)]

# str(activity.min)
# cc.min <- complete.cases(activity.min)
# sum(cc.min)

activity.clean <- activity.min[, -c(1:6)]
str(activity.clean)

## split training and test sets
set.seed(32323)

inTrain <- createDataPartition(activity.clean$classe, p=0.7, list=FALSE)
training <- activity.clean[inTrain, ]; testing <- activity.clean[-inTrain, ]

## near zero variance vars (24)
nsv <- nearZeroVar(training, saveMetrics=TRUE)
sum(nsv$nzv)

nvars <- nsv$nzv
training <- training[, !nvars]

## boosting with trees, standardized
fit.gbm <- train(classe ~ ., 
                 data=training.min,
                 method="gbm",
                 preProcess = c("center", "scale"),
                 verbose=FALSE)

pred <- predict(fit.gbm, testing)
table(pred, testing$classe)

predRight.gbm <- pred == testing$classe
sum(predRight.gbm)/nrow(testing) #0.

## RRF
library(RRF)

fit.rrf <- RRF(classe ~ ., 
               data=training,
               ntree=100,
               keep.forest=FALSE,
               importance=TRUE)

importance(fit.rrf)

## this is probably over-fitting
training.min <- training[, c(1:3,7,10:11,15:16,19,23,25,27,32,35:42,46:48,50:53)]

fit.rrf.min <- RRF(classe ~ ., 
               data=training.min,
               ntree=100,
               keep.forest=TRUE,
               importance=TRUE)

importance(fit.rrf.min)

pred.rrf <- predict(fit.rrf.min, testing)
table(pred.rrf, testing$classe)

predRight.rrf <- pred.rrf == testing$classe
sum(predRight.rrf)/nrow(testing) #0.9908-0.9913

## random forest with caret package
rf <- train(classe ~ ., data=training.min, 
            method="rf", 
            prox=TRUE)

pred.rf <- predict(rf, testing)
table(pred.rf, testing$classe)

predRight.rf <- pred.rf == testing$classe
sum(predRight.rf)/nrow(testing) #0.

## rfFuncs
library(caret)
library(doMC)

y <- training$classe
x <- training[, -53]

test <- testing[, !nvars]
testX <- as.data.frame(test[, -53])
testY <- test$classe

normalization <- preProcess(x)
x <- predict(normalization, x)
x <- x[, -findCorrelation(cor(x), cutoff=.8, verbose=FALSE)]
x <- as.data.frame(x)

## full data set
activity1 <- activity.clean[, !nvars]

y1 <- activity1[, 53]
x1 <- activity1[, -53]
normalization1 <- preProcess(x1)
x1 <- predict(normalization1, x1)
x1 <- as.data.frame(x1)

subsets <- c(5,10,15,20,25)

set.seed(11)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   number = 5,
                   repeats = 5,
                   verbose = FALSE)

registerDoMC(cores = 4)
set.seed(126)
rfProfile <- rfe(x1, y1,
                 sizes = subsets,
                 rfeControl = ctrl)
rfProfile #0.9913 accuracy with to 10 vars

best <- predictors(rfProfile)

trellis.par.set(caretTheme())
plot(rfProfile, type = c("g", "o"))

postResample(predict(rfProfile, testX), testY)

## straight up rrf with 52 predictors
fit.rrf2 <- RRF(classe ~ ., 
               data=training,
               ntree=100,
               keep.forest=TRUE,
               importance=TRUE)

pred.rrf2 <- predict(fit.rrf2, testing)
table(pred.rrf2, testing$classe)

predRight.rrf2 <- pred.rrf2 == testing$classe
sum(predRight.rrf2)/nrow(testing) #0.9901

dim(activity)[2]

importance(fit.rrf2) #half of vars do nothing

fit.rrf3 <- RRF(classe ~ ., 
                data=training,
                ntree=500,
                keep.forest=TRUE,
                importance=TRUE)

pred.rrf3 <- predict(fit.rrf3, testing)
table(pred.rrf3, testing$classe)

predRight.rrf3 <- pred.rrf3 == testing$classe
sum(predRight.rrf3)/nrow(testing) #0.9910

## randomForest
library(randomForest)

fit.rf <- randomForest(classe ~ ., data=training, ntree=1000)
pred.rf <- predict(fit.rf, testing)
table(pred.rf, testing$classe)

pred.rf <- pred.rf == testing$classe
sum(pred.rf)/nrow(testing)

fit.rf5 <- randomForest(classe ~ ., data=training, ntree=500)
pred.rf5 <- predict(fit.rf5, testing)
table(pred.rf5, testing$classe)

pred.rf5 <- pred.rf5 == testing$classe
sum(pred.rf5)/nrow(testing)

## nearZeroVariation for entire activity.clean
nsv2 <- nearZeroVar(activity.clean[, -77], saveMetrics=TRUE)
sum(nsv2$nzv)

nvars2 <- nsv2$nzv
activity.nzv <- activity.clean[, !nvars2]

## k-folds cross-validation
library(caret)

set <- as.numeric()

for(i in 1:5) {
        
        set.seed(i + 123)
        folds <- createFolds(activity.nzv$classe, k=5)

        for(j in 1:5) {
        
                testing <- activity.nzv[folds[[j]], ]
                training <- activity.nzv[-folds[[j]], ]
        
                set.seed(j)
                mod <- randomForest(classe ~ ., data=training, ntree=500)
        
                pred <- predict(mod, testing)
                pred <- pred == testing$classe
                acc <- sum(pred)/nrow(testing)
        
                set <- c(set, acc)
        }
}

mean(set) #0.9957 @ ntree=100 (length: 25) 0.9960 @ ntree=500

## test data (20 cases)
test.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
test.file <- "pml-testing.csv"

download.file(test.url, dest=test.file, method="curl")
test.data <- read.csv(test.file)


## from trainControl exampl
set.seed(1235)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     seeds = seeds)

set.seed(2)
mod <- train(classe ~ ., data = activity.nzv, 
             method = "rf",
             ntree=500,
             trControl = ctrl)