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
# names(activity)
activity <- activity[, -c(1,14,17,26,89,92,101,127,130,139)]

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

# names(activity)

# summary(activity$min_roll_forearm)
# summary(activity$var_yaw_forearm)

activity.min <- activity[, -c(15:16,18:19,21:32,46:55,71:79,87:88,90:91,93:94,96:105,122:123,125:126,128:129,131:140)]

# str(activity.min)

d <- parse_date_time(activity.min$cvtd_timestamp, "%d%m%Y %H!%M")
da <- as.Date(d)
h <- hour(d)
e <- as.POSIXct(activity.min$cvtd_timestamp, format = "%d/%m/%Y %H:%M", tz = "UTC")

# cc.min <- complete.cases(activity.min)
# sum(cc.min)

activity.clean <- cbind(da, h, activity.min[, -c(1:6)])
na <- names(activity.clean)
names(activity.clean) <- c("date", "hour", na[-c(1:2)])
str(activity.clean)

## split training and test sets
set.seed(32323)

inTrain <- createDataPartition(activity.clean$classe, p=0.7, list=FALSE)
training <- activity.clean[inTrain, ]; testing <- activity.clean[-inTrain, ]

## near zero variance vars (24)
nsv <- nearZeroVar(training, saveMetrics=TRUE)
sum(nsv$nzv)
sum(nsv$zeroVar)


## boosting with trees
modFit <- train(classe ~ ., method="gbm", data=training, verbose=FALSE)

pred <- predict(modFit, testing)
table(pred, testing$classe)

predRight.gbm <- pred == testing$classe
sum(predRight.gbm)/nrow(testing) #0.9600

## same boosting but with center, scale
modFit2 <- train(classe ~ ., data=training,
                 method="gbm",
                 preProcess = c("center", "scale"),
                 verbose=FALSE)

pred2 <- predict(modFit2, testing)
table(pred2, testing$classe)

predRight.gbm2 <- pred2 == testing$classe
sum(predRight.gbm2)/nrow(testing) #0.9609

## same boosting but with BoxCox
modFit3 <- train(classe ~ ., data=training,
                 method="gbm",
                 preProcess="BoxCox",
                 verbose=FALSE)

pred3 <- predict(modFit3, testing)
table(pred3, testing$classe)

predRight.gbm3 <- pred3 == testing$classe
sum(predRight.gbm3)/nrow(testing) #0.9606

## boosting with trees with pca preprocessing
fit.gbm.pca <- train(classe ~ ., method="gbm", preProcess="pca", data=training, verbose=FALSE)

pred.gbm.pca <- predict(fit.gbm.pca, testing)
table(pred.gbm.pca, testing$classe)

predRight.gbm.pca <- pred.gbm.pca == testing$classe
sum(predRight.gbm.pca)/nrow(testing) #0.8279

## random forest (takes too long)
rf <- train(classe ~ ., data=training, 
            method="rf", 
            preProcess = c("center", "scale"), 
            prox=TRUE)

pred.rf <- predict(rf, testing)
table(pred.rf, testing$classe)

predRight.rf <- pred.rf == testing$classe
sum(predRight.rf)/nrow(testing) #0.

## rf with pca preprocessing
fit.rf.pca <- train(classe ~ ., method="rf", preProcess="pca", data=training, prox=TRUE)
