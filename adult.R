#1
adult = read.csv("http://www.yurulin.com/class/spring2016_datamining/data/adult.csv", header = TRUE)
nrow(adult)
summary(adult)
any(is.na(adult))
adult <- adult[!(adult$work==" ?"), ]
nrow(adult)
summary(adult)
adult = na.omit(adult)
adult$salary <- as.character(adult$salary)
adult$salary[adult$salary == " <=50K"] <- 0
adult$salary[adult$salary == " >50K"] <- 1
adult$salary <- as.factor(adult$salary)
y = as.factor(adult$salary)
adult = cbind(y = y, adult)
adult = adult[ , -12]
adult_num = adult
adult[1:3, ]
adult$work = as.factor(adult$work)
adult$relation = as.factor(adult$relation)
adult$race = as.factor(adult$race)
adult$sex = as.factor(adult$sex)
summary(adult)

##For knn
adult_num$age = as.numeric(adult_num$age)
adult_num$work = as.numeric(adult_num$work)
adult_num$fnlwgt = as.numeric(adult_num$fnlwgt)
adult_num$edu_num = as.numeric(adult_num$edu_num)
adult_num$relation = as.numeric(adult_num$relation)
adult_num$race = as.numeric(adult_num$race)
adult_num$sex = as.numeric(adult_num$sex)
adult_num$cap_gain = as.numeric(adult_num$cap_gain)
adult_num$cap_loss = as.numeric(adult_num$cap_loss)
adult_num$hours = as.numeric(adult_num$hours)
for (i in 2:11){
  adult_num[,i] = adult_num[,i]*(10/mean(adult_num[,i]))
}
adult_num[1:3,]

library(MASS) # for the example dataset 
library(plyr) # for recoding data 
library(ROCR) # for plotting roc 
library(e1071) # for NB and SVM 
library(rpart) # for decision tree 
library(ada) # for adaboost
library(class)# for knn

set.seed(12345)
do.classification <- function(train.set, test.set, cl.name, verbose=F) {
  switch(cl.name,
         #test k = 1
         knn1 = {
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 1, prob = T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] #modified
           prob = attr(prob,"prob")
           #print(cbind(prob,as.character(test.set$y)))
           prob
         },
         #test k = 3
         knn3 = {
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 3, prob = T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] 
           prob = attr(prob,"prob")
           prob
         },
         #test k = 5
         knn5 = {
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 5, prob = T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] 
           prob = attr(prob,"prob")
           prob
         },
         #test k = 10
         knn10 = {
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 10, prob = T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] 
           prob = attr(prob,"prob")
           prob
         },
         #test k = 20
         knn20 = {
           prob = knn(train.set[,-1], test.set[,-1], cl=train.set[,1], k = 20, prob = T)
           attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] 
           prob = attr(prob,"prob")
           prob
         },
         #logistic regression
         lr = {
           model = glm(y~., family=binomial, data=train.set)
           if (verbose) {
             print(summary(model))
           }
           prob = predict(model, newdata=test.set, type="response")
           prob
         },
         #naivebayes
         nb = {
           model = naiveBayes(y~., data=train.set)
           prob = predict(model, newdata=test.set, type="raw")
           prob = prob[,2]/rowSums(prob) # renormalize the prob.
           prob
         },
         #decision tree
         dtree = {
           model = rpart(y~., data=train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             printcp(model) # print the cross-validation results
             plotcp(model) # visualize the cross-validation results
             ## plot the tree
             plot(model, uniform=TRUE, main="Classification Tree")
             text(model, use.n=TRUE, all=TRUE, cex=.8)
           }
           prob = predict(model, newdata=test.set)
           if (0) { #default tree,
             # prune the tree
             pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
             prob = predict(pfit, newdata=test.set)
             # plot the pruned tree
             plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
             text(pfit, use.n=TRUE, all=TRUE, cex=.8)
           }
           prob = prob[,2]/rowSums(prob)
           prob
         },
         #decision tree prune
         dtreeprune = {
           model = rpart(y~., data=train.set)
           if (verbose) {
             print(summary(model)) 
             printcp(model) 
             plotcp(model) 
             plot(model, uniform=TRUE, main="Classification Tree")
             text(model, use.n=TRUE, all=TRUE, cex=.8)
           }
           prob = predict(model, newdata=test.set)
           if (1) { #prune the tree
             pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
             prob = predict(pfit, newdata=test.set)
             plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
             text(pfit, use.n=TRUE, all=TRUE, cex=.8)
           }
           prob = prob[,2]/rowSums(prob) 
           prob
         },
         #support vector machine
         svm = {
           model = svm(y~., data=train.set, probability=T)
           if (0) { # fine-tune the model with different kernel and parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set,
                               kernel="radial",
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T,
                         kernel="radial", gamma=gamma, cost=cost)
           }
           prob = predict(model, newdata=test.set, probability=T)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svm1 = {
           model = svm(y~., data=train.set, probability=T)
           if (1) { # kernel = radial here we use parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set,
                               kernel="radial",
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T,
                         kernel="radial", gamma=gamma, cost=cost)
           }
           prob = predict(model, newdata=test.set, probability=T)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svm2 = {
           model = svm(y~., data=train.set, probability=T)
           if (1) { # kernel = sigmoid   here we use parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set,
                               kernel="sigmoid",
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T,
                         kernel="radial", gamma=gamma, cost=cost)
           }
           prob = predict(model, newdata=test.set, probability=T)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         svm3 = {
           model = svm(y~., data=train.set, probability=T)
           if (1) {    #kernel = polynomial   here we use parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(y~., data = train.set,
                               kernel="polynomial",
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(y~., data = train.set, probability=T,
                         kernel="radial", gamma=gamma, cost=cost)
           }
           prob = predict(model, newdata=test.set, probability=T)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         #ada
         ada = {
           model = ada(y~., data = train.set)
           prob = predict(model, newdata=test.set, type='probs')
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob)
           prob
         }
        )
}#classifications function end

pre.test <- function(dataset, cl.name, r=0.6, prob.cutoff=0.5) {
  ## Let's use 60% random sample as training and remaining as testing
  ## by default use 0.5 as cut-off
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.train = floor(n.obs*r)
  train.idx = sample(1:n.obs,n.train)
  train.idx
  train.set = dataset[train.idx,]
  test.set = dataset[-train.idx,]
  cat('pre-test',cl.name,':',
      '#training:',nrow(train.set),
      '#testing',nrow(test.set),'\n')
  prob = do.classification(train.set, test.set, cl.name)
  #prob is an array of probabilities for cases beingpositive
  ## get confusion matrix
  predicted = as.numeric(prob > prob.cutoff)
  actual = test.set$y
  confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
  error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)
  cat('error rate:',error,'\n')
  # you may compute other measures based on confusion.matrix
  # @see handout03 p.30-
  ## plot ROC
  result = data.frame(prob,actual)
  pred = prediction(result$prob,result$actual)
  perf = performance(pred, "tpr","fpr")
  plot(perf)
}

my.classifier <- function(dataset, cl.name, do.cv) {
  n.obs <- nrow(dataset) #number of observations in dataset
  n.cols <- ncol(dataset) #numberof predictors 
  cat('my dataset:',
      n.obs,'observations',
      n.cols-1,'predictors','\n')
  print(dataset[1:3,])
  cat('label (y) distribution:')
  print(table(dataset$y))
  pre.test(dataset, cl.name)
  if (do.cv) k.fold.cv(dataset, cl.name)
}

##define the k-fold
k.fold.cv <- function(dataset, cl.name, k.fold=10, prob.cutoff=0.5){
  ## default: 10-fold CV, cut-off 0.5
  n.obs <- nrow(dataset) # no. of observations 
  s = sample(n.obs)
  errors = dim(k.fold)
  probs = NULL
  actuals = NULL
  for (k in 1:k.fold) {
    test.idx = which(s %% k.fold == (k-1) ) # use modular operator
    train.set = dataset[-test.idx,]
    test.set = dataset[test.idx,]
    cat(k.fold,'-fold CV run',k,cl.name,':',
        '#training:',nrow(train.set),
        '#testing',nrow(test.set),'\n')
    prob = do.classification(train.set, test.set, cl.name)
    predicted = as.numeric(prob > prob.cutoff)
    actual = test.set$y
    confusion.matrix =
      table(actual,factor(predicted,levels=c(0,1)))
    confusion.matrix
    error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)
    errors[k] = error
    cat('\t\terror=',error,'\n')
    probs = c(probs,prob)
    actuals = c(actuals,actual)
  }
  avg.error = mean(errors)
  cat(k.fold,'-fold CV results:','avg error=',avg.error,'\n')
  ## plot ROC
  result = data.frame(probs,actuals)
  pred = prediction(result$probs,result$actuals)
  perf = performance(pred, "tpr","fpr")
  plot(perf)
  ## get other measures by using 'performance'
  get.measure <- function(pred, measure.name='auc') { 
    perf = performance(pred,measure.name)
    m <- unlist(slot(perf, "y.values"))
    m
  }
  err = mean(get.measure(pred, 'err'))
  precision = mean(get.measure(pred, 'prec'),na.rm=T)
  recall = mean(get.measure(pred, 'rec'),na.rm=T)
  fscore = mean(get.measure(pred, 'f'),na.rm=T)
  cat('error=',err,
      'precision=',precision,
      'recall=',recall,
      'score',fscore,'\n')
  auc = get.measure(pred, 'auc')
  cat('auc=',auc,'\n')
  return(perf)
}

##main part##
##Choose a best knn
knn1=my.classifier(adult_num, cl.name='knn1',do.cv=T)
##Logistic regression
lr = my.classifier(adult, cl.name='lr',do.cv=T)
#naive bayes
nb = my.classifier(adult, cl.name='nb',do.cv=T)
#decision tree
dtree = my.classifier(adult, cl.name='dtree',do.cv=T)
#support vector machine
svm = my.classifier(adult, cl.name='svm',do.cv=T)
#ada
ada = my.classifier(adult, cl.name='ada',do.cv=T)

##classification results
fscore <- matrix(c(0.4695279,0.522527,0.5152907,0.4704465,0.5223778,0.5470483), 
                  nrow=1, ncol=6, byrow=TRUE, 
                  dimnames=list(
                    c("f-score"), 
                    c("knn", "logistic", "nb", "dtree", "svm", "ada")))
fscore<- as.table(fscore)
auc <- matrix(c(0.5920398,0.6404105,0.624972,0.71043,0.6399743,0.7093254), 
                 nrow=1, ncol=6, byrow=TRUE, 
                 dimnames=list(
                   c("auc"), 
                   c("knn", "logistic", "nb", "dtree", "svm", "ada")))
auc<- as.table(auc)
##plot f-score and auc bars
barplot(fscore,main="f-score of all models",xlab="model type", ylab="f-score",width=10)
barplot(auc,main="auc of all models",xlab="model type", ylab="auc",width=10)

##2
##knn variants
knn3=my.classifier(adult_num, cl.name='knn3',do.cv=T)
knn5=my.classifier(adult_num, cl.name='knn5',do.cv=T)
knn10=my.classifier(adult_num, cl.name='knn10',do.cv=T)
knn20=my.classifier(adult_num, cl.name='knn20',do.cv=T)
##decision tree variants
dtreeprune = my.classifier(adult, cl.name='dtreeprune',do.cv=T)
##support vector machine variants
svm1 = my.classifier(adult, cl.name='svm1',do.cv=T)
svm2 = my.classifier(adult, cl.name='svm2',do.cv=T)
svm3 = my.classifier(adult, cl.name='svm3',do.cv=T)

##3
plot(lr, col="blue")
plot(knn10, add=TRUE, col="gray")
plot(nb, add=TRUE, col="orange")
plot(dtree, add=TRUE, col="green")
plot(svm2, add=TRUE, col="black")
plot(ada, add=TRUE, col="red")
legend("bottomright", title="Classification",
       c("lr","knn10","nb","dtree","svm2","ada"),
       fill=c("blue","gray","orange","green","black","red"), horiz=FALSE)