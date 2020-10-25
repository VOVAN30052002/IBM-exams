rm(list=ls())
ptm <- proc.time() # Timer

#install.packages("gbm")
#install.packages("dplyr")

library(gbm)
library(dplyr)

df <- read.csv("clean_train_v9.csv")

df <- subset(df, select = -c(click_bool, gross_bookings_usd, date_time, position ))


n <- nrow(df)
train <- df[1:40000000,]
validation <- df[4000000:n,]
df <-0 # Clearing memory


#Training model

gbm.ndcg <- gbm(train$booking_bool~.,train, distribution=list(name='pairwise',metric="ndcg",group='srch_id'),n.trees=1000,interaction.depth=10,cv.folds=5,n.cores=14)

summary(gbm.ndcg, main='pairwise (ndcg)') 

best.iter.ndcg <- gbm.perf(gbm.ndcg, method='cv')
train <- 0 # Clearing memory

pred1 = predict(gbm.ndcg, validation ,n.trees=best.iter.ndcg, type="response")

validate <- data.frame(cbind(validation$srch_id, validation$prop_id,validation$booking_bool, pred1))
validate <- arrange(validate, validate$V1, -validate$pred1)

runscore <- validate[,c(1,2,3)]
validation <- 0 # Clearing memory

# Predicting rank
load("myFunction.Rdata")

testdata <-  read.csv("clean_test_v9.csv")
pred1 = predict(gbm.ndcg, testdata ,n.trees=best.iter.ndcg, type="response")

test <- data.frame(cbind(testdata$srch_id, testdata$prop_id,pred1))
test <- arrange(test, test$V1, -test$pred1)

runscore <- test[,c(1,2)]

colnames(runscore) <- c("srch_id", "prop_id")
write.csv(runscore, file = "prediction 12.csv", row.names = FALSE)
proc.time() - ptm





