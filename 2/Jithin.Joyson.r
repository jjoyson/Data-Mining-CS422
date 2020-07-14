
library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)
library('dplyr')
library(arules)
library(arulesViz)
setwd(getwd())
set.seed(1122)

adult_test.df <- read.csv("adult-test.csv", sep = ",", header = T)
adult_train.df <- read.csv("adult-train.csv", sep = ",", header = T)
for (i in names(adult_test.df)){
    vec <- which(adult_test.df[i] == "?")
    vec2 <- which(adult_train.df[i] == "?")
    if(length(vec)){
        adult_test.df <- adult_test.df[-vec,]
        adult_train.df <- adult_train.df[-vec2,]
    }
}
paste("Test observations: ", nrow(adult_test.df))
paste("Train observations: ", nrow(adult_train.df))

model <- rpart(income ~ ., method="class", data = adult_train.df)

model.sum <- summary(model)

model.sum['variable.importance']

rpart.plot(model, extra=104, fallen.leaves = T, type=4, main="Rpart on Income data (Full Tree)")

pred <- predict(model, adult_test.df, type="class")
conf <- confusionMatrix(pred, as.factor(adult_test.df[, ncol(adult_test.df)]))
conf

str(conf)

balanced_accuracy <- round(mean(c(conf$byClass[['Sensitivity']],conf$byClass[['Specificity']])), 3)
paste("Balanced Accuracy: ",balanced_accuracy)

balanced_error_rate = 1 - balanced_accuracy
paste("Balanced Error Rate: ",balanced_error_rate)

paste("Sensitivity: ",conf$byClass[['Sensitivity']])
paste("Specificity: ",conf$byClass[['Specificity']])

pred.rocr <- predict(model, adult_test.df, type="prob")[, 2]
f.pred <- prediction(pred.rocr, adult_test.df$income)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
cat(paste("The area under curve (AUC) for this model is ", round(auc@y.values[[1]], 3)))

printcp(model)

train_less50k <- filter(adult_train.df, income == ">50K")
train_more50k <- filter(adult_train.df, income == "<=50K")

paste("There are ",nrow(train_more50k)," observations in the class <=50K")
paste("There are ",nrow(train_less50k)," observations in the class >50K")

indicies <- sample(1:nrow(train_more50k), nrow(train_less50k))
train_more50k <- train_more50k[indicies, ]
new_training_dataset <- rbind (train_more50k, train_less50k)

model = rpart(income ~ ., method="class", data = new_training_dataset)
pred <- predict(model, adult_test.df, type="class")
conf <- confusionMatrix(pred, as.factor(adult_test.df[, ncol(adult_test.df)]))

balanced_accuracy <- round(mean(c(conf$byClass[['Sensitivity']],conf$byClass[['Specificity']])), 3)
paste("Balanced Accuracy: ",balanced_accuracy)

balanced_error_rate = 1 - balanced_accuracy
paste("Balanced Error Rate: ",balanced_error_rate)

paste("Sensitivity: ",conf$byClass[['Sensitivity']])
paste("Specificity: ",conf$byClass[['Specificity']])

pred.rocr <- predict(model, adult_test.df, type="prob")[, 2]
f.pred <- prediction(pred.rocr, adult_test.df$income)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
cat(paste("The area under curve (AUC) for this model is ", round(auc@y.values[[1]], 3)))

conf

tr_1k = read.transactions("tr-1k-canonical.csv", sep = ",")
tr_5k = read.transactions("tr-5k-canonical.csv", sep = ",")
tr_20k = read.transactions("tr-20k-canonical.csv", sep = ",")
tr_75k = read.transactions("tr-75k-canonical.csv", sep = ",")

summary(tr_1k)

f_is <- apriori(tr_1k, parameter=list(support=.04, target="frequent itemsets"))
inspect(sort(f_is, decreasing = T, by="count"))
rm(f_is)

rules <- apriori(tr_1k, parameter = list(sup = 0.02, conf = 1, target = "rules"))
inspect(sort(rules, by="count", decreasing=T))

summary(tr_5k)

f_is <- apriori(tr_5k, parameter=list(support=.09, target="frequent itemsets"))
inspect(sort(f_is, decreasing = T, by="count"))
rm(f_is)

rules <- apriori(tr_5k, parameter = list(sup = 0.02
                                         , conf = 1, target = "rules"))
inspect(sort(rules, by="count"))
rm(rules)

f_is <- apriori(tr_20k, parameter=list(support=.09, target="frequent itemsets"))
inspect(sort(f_is, decreasing = T, by="count"))
rm(f_is)

rules <- apriori(tr_20k, parameter = list(sup = 0.02
                                         , conf = 1, target = "rules"))
inspect(sort(rules, by="count"))
rm(rules)

summary(tr_75k)

f_is <- apriori(tr_75k, parameter=list(support=.09, target="frequent itemsets"))
inspect(sort(f_is, decreasing = T, by="count"))
rm(f_is)

rules <- apriori(tr_75k, parameter = list(sup = 0.02
                                         , conf = 1, target = "rules"))
inspect(sort(rules, by="count"))
rm(rules)

f_is <- apriori(tr_75k, parameter=list(support=.02, target="frequent itemsets"))
inspect(sort(f_is, decreasing = T, by="count"))
rm(f_is)
