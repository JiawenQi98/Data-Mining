##1
bike = read.csv("http://www.yurulin.com/class/spring2016_datamining/data/bike.csv", header = TRUE, na.strings = c('?'))

##2.a
Rowname = c("temp","atemp","hum","windspeed","cnt")
Columnname =c("min","1st quartile","median","mean","3rd quartile","max","standar devitation")
temp = c(as.vector(summary(bike$temp)), sd(bike$temp))
atemp = c(as.vector(summary(bike$atemp)), sd(bike$atemp))
hum = c(as.vector(summary(bike$hum)), sd(bike$hum))
windspeed = c(as.vector(summary(bike$windspeed)), sd(bike$windspeed))
cnt = c(as.vector(summary(bike$cnt)), sd(bike$cnt))
summary = matrix(c(temp, atemp, hum, windspeed, cnt),nrow = 5, ncol = 7, byrow = TRUE, dimnames = list(Rowname, Columnname))
View(summary)

##2.b
##density distribution
library(ggplot2)
ggplot(bike, aes(x = temp)) + geom_density()
ggplot(bike, aes(x = atemp)) + geom_density()
ggplot(bike, aes(x = hum)) + geom_density()
ggplot(bike, aes(x = windspeed)) + geom_density()
ggplot(bike, aes(x = cnt)) + geom_density()

##2.c
##correlation and scatterplot
cor(bike$temp, bike$cnt)
ggplot(bike, aes(x=temp, y=cnt)) + geom_point(shape=1) +geom_smooth(method=lm, se = FALSE)
cor(bike$atemp, bike$cnt)
ggplot(bike, aes(x=atemp, y=cnt)) + geom_point(shape=1) +geom_smooth(method=lm, se = FALSE)
cor(bike$hum, bike$cnt)
ggplot(bike, aes(x=hum, y=cnt)) + geom_point(shape=1) +geom_smooth(method=lm, se = FALSE)
cor(bike$windspeed, bike$cnt)
ggplot(bike, aes(x=windspeed, y=cnt)) + geom_point(shape=1) +geom_smooth(method=lm, se = FALSE)

##2.d
##conditional density plot
bike$cnt = as.numeric(bike$cnt)
bike$season = as.factor(bike$season)
bike$yr = as.factor(bike$yr)
bike$mnth = as.factor(bike$mnth)
bike$holiday = as.factor(bike$holiday)
bike$weekday = as.factor(bike$weekday)
bike$weathersit = as.factor(bike$weathersit)
ggplot(data=bike, aes(x = cnt, fill = season)) + geom_density(alpha=0.5)
ggplot(data=bike, aes(x = cnt, fill = yr)) + geom_density(alpha=0.5)
ggplot(data=bike, aes(x = cnt, fill = mnth)) + geom_density(alpha=0.5)
ggplot(data=bike, aes(x = cnt, fill = holiday)) + geom_density(alpha=0.5)
ggplot(data=bike, aes(x = cnt, fill = weekday)) + geom_density(alpha=0.5)
ggplot(data=bike, aes(x = cnt, fill = weathersit)) + geom_density(alpha=0.5)

##2.e
library(doBy)
library(survival)
library(splines)
summaryBy(bike$cnt~bike$holiday, data=bike, FUN = c(sum, mean, sd, var, length))
holiday0 = subset(bike, holiday == "0", select = cnt)
holiday1 = subset(bike, holiday == "1", select = cnt)
t.test(holiday0, holiday1, paired = FALSE, var.equal = FALSE, conf.level =0.95)

##3.a
##standard linear regression 
##model 1: with all predictors
lm_cnt = lm(cnt ~ ., data = bike)
summary(lm_cnt)
rmse = sqrt(mean(residuals(lm_cnt)^2))
rmse
n = length(bike$cnt)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train = train1[train1 != k]
  m1 = lm(cnt ~ ., data = bike[train, ])
  pred = predict(m1, newdata = bike[-train, ])
  obs = bike$cnt[-train]
  error[k] = obs - pred
}
lm1_me = mean(error)
lm1_rmse = sqrt(mean(error^2))
lm1_me
lm1_rmse

##3.b
##optimize linear model and try non-linear model
##model 2: linear
library (MASS)
stepAIC(lm_cnt, direction = "backward")
n = length(bike$cnt)
error = dim(n)
for (k in 1:n) {train1 = c(1:n)
    train = train1[train1 != k]
    m2 = lm(cnt ~ weekday + holiday + hum + windspeed + mnth + season + temp + weathersit + yr, data = bike[train, ])
    pred = predict(m2, newdata = bike[-train, ])
    obs = bike$cnt[-train]
    error[k] = obs - pred
}
lm2_me = mean(error)
lm2_rmse = sqrt(mean(error^2))
lm2_me
lm2_rmse
##model 3:non-linear
poly1 = lm(cnt ~ weekday + holiday + poly(hum, degree = 2) + poly(windspeed, degree = 2) + mnth + season + poly(temp, degree = 2) + weathersit + yr, data = bike) 
n = length(bike$cnt)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train = train1[train1 != k]
  m3 = lm(cnt ~ weekday + holiday + poly(hum, degree = 2) + poly(windspeed, degree = 2) + mnth + season + poly(temp, degree = 2) + weathersit + yr, data = bike[train,])
  pred = predict(m3, newdat = bike[-train, ])
  obs = bike$cnt[-train]
  error[k] = obs - pred
}
poly1_me = mean(error)
poly1_rmse = sqrt(mean(error^2))
poly1_rmse
##model 4:non-linear
poly2 = lm(cnt ~ weekday + holiday + poly(hum, degree = 3) + poly(windspeed, degree = 3) + mnth + season + poly(temp, degree = 3) + weathersit + yr, data = bike) 
n = length(bike$cnt)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train = train1[train1 != k]
  m4 = lm(cnt ~ weekday + holiday + poly(hum, degree = 3) + poly(windspeed, degree = 3) + mnth + season + poly(temp, degree = 3) + weathersit + yr, data = bike[train,])
  pred = predict(m4, newdat = bike[-train, ])
  obs = bike$cnt[-train]
  error[k] = obs - pred
}
poly2_me = mean(error)
poly2_rmse = sqrt(mean(error^2))
poly2_rmse
##model 5:non-linear
poly3 = lm(cnt ~ weekday + holiday + poly(hum, degree = 4) + poly(windspeed, degree = 4) + mnth + season + poly(temp, degree = 4) + weathersit + yr, data = bike) 
n = length(bike$cnt)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train = train1[train1 != k]
  m5 = lm(cnt ~ weekday + holiday + poly(hum, degree = 4) + poly(windspeed, degree = 4) + mnth + season + poly(temp, degree = 4) + weathersit + yr, data = bike[train,])
  pred = predict(m5, newdat = bike[-train, ])
  obs = bike$cnt[-train]
  error[k] = obs - pred
}
poly3_me = mean(error)
poly3_rmse = sqrt(mean(error^2))
poly3_rmse
##model 6:non-linear
poly4 = lm(cnt ~ weekday + holiday + poly(hum, degree = 5) + poly(windspeed, degree = 5) + mnth + season + poly(temp, degree = 5) + weathersit + yr, data = bike) 
n = length(bike$cnt)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train = train1[train1 != k]
  m6 = lm(cnt ~ weekday + holiday + poly(hum, degree = 5) + poly(windspeed, degree = 5) + mnth + season + poly(temp, degree = 5) + weathersit + yr, data = bike[train,])
  pred = predict(m6, newdat = bike[-train, ])
  obs = bike$cnt[-train]
  error[k] = obs - pred
}
poly4_me = mean(error)
poly4_rmse = sqrt(mean(error^2))
poly4_rmse

##3.c
summary(poly3)