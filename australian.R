##import the dataset
australian = read.csv("http://www.yurulin.com/class/spring2016_datamining/data/australian.csv", header = TRUE, na.strings = c('?'))

##2.a
Rowname = c("A2", "A3", "A7", "A10", "A13", "A14")
Columnname = c("min", "1st quartile", "median", "mean", "3rd quartile", "max", "standard deviation")
A2 = c(as.vector(summary(australian$A2)), sd(australian$A2))
A3 = c(as.vector(summary(australian$A3)), sd(australian$A3))
A7 = c(as.vector(summary(australian$A7)), sd(australian$A7))
A10 = c(as.vector(summary(australian$A10)), sd(australian$A10))
A13 = c(as.vector(summary(australian$A13)), sd(australian$A13))
A14 = c(as.vector(summary(australian$A14)), sd(australian$A14))
summary = matrix(c(A2, A3, A7, A10, A13, A14), nrow = 6, ncol = 7, byrow = TRUE, dimnames = list(Rowname, Columnname))
View(summary)

##2.b
library(ggplot2)
ggplot(australian, aes(x = A2)) + geom_density()
ggplot(australian, aes(x = A3)) + geom_density()
ggplot(australian, aes(x = A7)) + geom_density()
ggplot(australian, aes(x = A10)) + geom_density()
ggplot(australian, aes(x = A13)) + geom_density()
ggplot(australian, aes(x = A14)) + geom_density()

##2.c
australian$A1 <- as.factor(australian$A1)
australian$A4 <- as.factor(australian$A4)
australian$A5 <- as.factor(australian$A5)
australian$A6 <- as.factor(australian$A6)
australian$A8 <- as.factor(australian$A8)
australian$A9 <- as.factor(australian$A9)
australian$A11 <- as.factor(australian$A11)
australian$A12 <- as.factor(australian$A12)
australian$A15 <- as.factor(australian$A15)
ggplot(data = australian, aes(x = A15)) + facet_grid(A1~., scales = "free_y") + geom_bar(alpha=0.5)
ggplot(data = australian, aes(x = A15)) + facet_grid(A4~., scales = "free_y") + geom_bar(alpha=0.5)
ggplot(data = australian, aes(x = A15)) + facet_grid(A5~., scales = "free_y") + geom_bar(alpha=0.5)
ggplot(data = australian, aes(x = A15)) + facet_grid(A6~., scales = "free_y") + geom_bar(alpha=0.5)
ggplot(data = australian, aes(x = A15)) + facet_grid(A8~., scales = "free_y") + geom_bar(alpha=0.5)
ggplot(data = australian, aes(x = A15)) + facet_grid(A9~., scales = "free_y") + geom_bar(alpha=0.5)
ggplot(data = australian, aes(x = A15)) + facet_grid(A11~., scales = "free_y") + geom_bar(alpha=0.5)
ggplot(data = australian, aes(x = A15)) + facet_grid(A12~., scales = "free_y") + geom_bar(alpha=0.5)

##3.a
library(cvTools)
library(lattice)
library(robustbase)
library(car)
##model 1
fold <- 10
set.seed(1)
cvgroup <- cvFolds(nrow(australian), K = fold)
errors = dim(fold)
precisions = dim(fold)
recalls = dim(fold)
fscores = dim(fold)
probs = NULL
actuals = NULL
for (i in (1:fold)){
  train <- which(cvgroup$which != i)
  test <- which(cvgroup$which == i)
  model1 <- glm(A15~A1+A4+A5+A6+A8+A9+A11+A12, family = binomial(link = "logit"), data = australian, subset = train)
  prob <- predict(model1, australian[test, ], type = c("response"))
  predicted <- recode(prob, "0.5:1 = 1; else = 0")
  actual = australian[test, ]$A15
  cm = table(actual, predicted)
  cm
  error = (cm[1, 2] + cm[2, 1]) / nrow(australian[test, ])
  errors[i] = error
  if ((cm[1, 1]) + (cm[2, 1]) == 0) {
    precision = 0
  } else {
    precision = (cm[1, 1]) / ((cm[1, 1]) + (cm[2, 1]))
  }
  precisions[i] = precision
  if ((cm[1, 1]) + (cm[1, 2]) == 0) {
    recall = 0
  } else {
    recall = (cm[1, 1]) / ((cm[1, 1])+(cm[1,2]))
  }
  recalls[i] = recall
  fscore = (2*precision*recall) / (precision + recall)
  fscores[i] = fscore
  probs = c(probs, prob)
  actuals = c(actuals, actual)
}
summary(model1)
avg.error1 = mean(errors)
avg.precision1 = mean(precisions)
avg.recall1 = mean(recalls)
avg.fscore1 = mean(fscores)
Rowname = c("model1")
Columnname = c("Error Rates", "Precisions", "Recall", "F1 Scores")
model1 = matrix(c(avg.error1, avg.precision1, avg.recall1, avg.fscore1), nrow = 1, ncol = 4, byrow = TRUE, dimnames = list(Rowname, Columnname))
model1
library(ROCR)
library(gplots)
result = data.frame(probs, actuals)
pred = prediction(result$probs, result$actuals)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "model1 roc")
df = cbind(probs, actuals)
rank.df = as.data.frame(df[order(probs, decreasing = TRUE), ])
colnames(rank.df) = c('predicted', 'actual')
baserate = mean(actuals)
n.total = length(australian$A15)
ax = dim(n.total)
ay.base = dim(n.total)
ay.pred = dim(n.total)
ax[1] = 1
ay.base[1] = baserate
ay.pred[1] = rank.df$actual[1]
for (i in 2:n.total) {
  ax[i] = i
  ay.base[i] = baserate*i
  ay.pred[i] = ay.pred[i - 1] +rank.df$actual[i]
}
plot(ax, ay.pred, xlab = "number of cases", ylab = "number of successes", main = "lift: model 1")
points(ax, ay.base, type = "l")
##model 2
fold <- 10
set.seed(1)
cvgroup <- cvFolds(nrow(australian), K = fold)
errors = dim(fold)
precisions = dim(fold)
recalls = dim(fold)
fscores = dim(fold)
probs = NULL
actuals = NULL
for (i in (1:fold)){
  train <- which(cvgroup$which != i)
  test <- which(cvgroup$which == i)
  model2 <- glm(A15~A1+A4+A5+A6+A8+A9+A12, family = binomial(link = "logit"), data = australian, subset = train)
  prob <- predict(model2, australian[test, ], type = c("response"))
  predicted <- recode(prob, "0.5:1 = 1; else = 0")
  actual = australian[test, ]$A15
  cm = table(actual, predicted)
  cm
  error = (cm[1, 2] + cm[2, 1]) / nrow(australian[test, ])
  errors[i] = error
  if ((cm[1, 1]) + (cm[2, 1]) == 0) {
    precision = 0
  } else {
    precision = (cm[1, 1]) / ((cm[1, 1]) + (cm[2, 1]))
  }
  precisions[i] = precision
  if ((cm[1, 1]) + (cm[1, 2]) == 0) {
    recall = 0
  } else {
    recall = (cm[1, 1]) / ((cm[1, 1])+(cm[1,2]))
  }
  recalls[i] = recall
  fscore = (2*precision*recall) / (precision + recall)
  fscores[i] = fscore
  probs = c(probs, prob)
  actuals = c(actuals, actual)
}
summary(model2)
avg.error = mean(errors)
avg.precision = mean(precisions)
avg.recall = mean(recalls)
avg.fscore = mean(fscores)
Rowname = c("model2")
Columnname = c("Error Rates", "Precisions", "Recall", "F1 Scores")
model2 = matrix(c(avg.error, avg.precision, avg.recall, avg.fscore), nrow = 1, ncol = 4, byrow = TRUE, dimnames = list(Rowname, Columnname))
model2
result = data.frame(probs, actuals)
pred = prediction(result$probs, result$actuals)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "model2 roc")
df = cbind(probs, actuals)
rank.df = as.data.frame(df[order(probs, decreasing = TRUE), ])
colnames(rank.df) = c('predicted', 'actual')
baserate = mean(actuals)
n.total = length(australian$A15)
ax = dim(n.total)
ay.base = dim(n.total)
ay.pred = dim(n.total)
ax[1] = 1
ay.base[1] = baserate
ay.pred[1] = rank.df$actual[1]
for (i in 2:n.total) {
  ax[i] = i
  ay.base[i] = baserate*i
  ay.pred[i] = ay.pred[i - 1] +rank.df$actual[i]
}
plot(ax, ay.pred, xlab = "number of cases", ylab = "number of successes", main = "lift: model 2")
points(ax, ay.base, type = "l")
##model 3
fold <- 10
set.seed(1)
cvgroup <- cvFolds(nrow(australian), K = fold)
errors = dim(fold)
precisions = dim(fold)
recalls = dim(fold)
fscores = dim(fold)
probs = NULL
actuals = NULL
for (i in (1:fold)){
  train <- which(cvgroup$which != i)
  test <- which(cvgroup$which == i)
  model3 <- glm(A15~A4+A5+A6+A8+A9+A12, family = binomial(link = "logit"), data = australian, subset = train)
  prob <- predict(model3, australian[test, ], type = c("response"))
  predicted <- recode(prob, "0.5:1 = 1; else = 0")
  actual = australian[test, ]$A15
  cm = table(actual, predicted)
  cm
  error = (cm[1, 2] + cm[2, 1]) / nrow(australian[test, ])
  errors[i] = error
  if ((cm[1, 1]) + (cm[2, 1]) == 0) {
    precision = 0
  } else {
    precision = (cm[1, 1]) / ((cm[1, 1]) + (cm[2, 1]))
  }
  precisions[i] = precision
  if ((cm[1, 1]) + (cm[1, 2]) == 0) {
    recall = 0
  } else {
    recall = (cm[1, 1]) / ((cm[1, 1])+(cm[1,2]))
  }
  recalls[i] = recall
  fscore = (2*precision*recall) / (precision + recall)
  fscores[i] = fscore
  probs = c(probs, prob)
  actuals = c(actuals, actual)
}
summary(model3)
avg.error = mean(errors)
avg.precision = mean(precisions)
avg.recall = mean(recalls)
avg.fscore = mean(fscores)
Rowname = c("model3")
Columnname = c("Error Rates", "Precisions", "Recall", "F1 Scores")
model3 = matrix(c(avg.error, avg.precision, avg.recall, avg.fscore), nrow = 1, ncol = 4, byrow = TRUE, dimnames = list(Rowname, Columnname))
model3
result = data.frame(probs, actuals)
pred = prediction(result$probs, result$actuals)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "model3 roc")
df = cbind(probs, actuals)
rank.df = as.data.frame(df[order(probs, decreasing = TRUE), ])
colnames(rank.df) = c('predicted', 'actual')
baserate = mean(actuals)
n.total = length(australian$A15)
ax = dim(n.total)
ay.base = dim(n.total)
ay.pred = dim(n.total)
ax[1] = 1
ay.base[1] = baserate
ay.pred[1] = rank.df$actual[1]
for (i in 2:n.total) {
  ax[i] = i
  ay.base[i] = baserate*i
  ay.pred[i] = ay.pred[i - 1] +rank.df$actual[i]
}
plot(ax, ay.pred, xlab = "number of cases", ylab = "number of successes", main = "lift: model 3")
points(ax, ay.base, type = "l")
##model 4
fold <- 10
set.seed(1)
cvgroup <- cvFolds(nrow(australian), K = fold)
errors = dim(fold)
precisions = dim(fold)
recalls = dim(fold)
fscores = dim(fold)
probs = NULL
actuals = NULL
for (i in (1:fold)){
  train <- which(cvgroup$which != i)
  test <- which(cvgroup$which == i)
  model4 <- glm(A15~A4+A5+A8+A9+A12, family = binomial(link = "logit"), data = australian, subset = train)
  prob <- predict(model4, australian[test, ], type = c("response"))
  predicted <- recode(prob, "0.5:1 = 1; else = 0")
  actual = australian[test, ]$A15
  cm = table(actual, predicted)
  cm
  error = (cm[1, 2] + cm[2, 1]) / nrow(australian[test, ])
  errors[i] = error
  if ((cm[1, 1]) + (cm[2, 1]) == 0) {
    precision = 0
  } else {
    precision = (cm[1, 1]) / ((cm[1, 1]) + (cm[2, 1]))
  }
  precisions[i] = precision
  if ((cm[1, 1]) + (cm[1, 2]) == 0) {
    recall = 0
  } else {
    recall = (cm[1, 1]) / ((cm[1, 1])+(cm[1,2]))
  }
  recalls[i] = recall
  fscore = (2*precision*recall) / (precision + recall)
  fscores[i] = fscore
  probs = c(probs, prob)
  actuals = c(actuals, actual)
}
summary(model4)
avg.error = mean(errors)
avg.precision = mean(precisions)
avg.recall = mean(recalls)
avg.fscore = mean(fscores)
Rowname = c("model4")
Columnname = c("Error Rates", "Precisions", "Recall", "F1 Scores")
model4 = matrix(c(avg.error, avg.precision, avg.recall, avg.fscore), nrow = 1, ncol = 4, byrow = TRUE, dimnames = list(Rowname, Columnname))
model4
result = data.frame(probs, actuals)
pred = prediction(result$probs, result$actuals)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "model4 roc")
df = cbind(probs, actuals)
rank.df = as.data.frame(df[order(probs, decreasing = TRUE), ])
colnames(rank.df) = c('predicted', 'actual')
baserate = mean(actuals)
n.total = length(australian$A15)
ax = dim(n.total)
ay.base = dim(n.total)
ay.pred = dim(n.total)
ax[1] = 1
ay.base[1] = baserate
ay.pred[1] = rank.df$actual[1]
for (i in 2:n.total) {
  ax[i] = i
  ay.base[i] = baserate*i
  ay.pred[i] = ay.pred[i - 1] +rank.df$actual[i]
}
plot(ax, ay.pred, xlab = "number of cases", ylab = "number of successes", main = "lift: model 4")
points(ax, ay.base, type = "l")

##3.b
model3 <- glm(A15~A4+A5+A6+A8+A9+A12, family = binomial(link = "logit"), data = australian, subset = train)
summary(model3)
exp(model3$coef)