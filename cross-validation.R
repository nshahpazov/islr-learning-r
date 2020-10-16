install.packages("ISLR")
install.packages("TSA")
install.packages("sigmoid")

library(MASS)
library(ISLR)
library(TSA)
attach(Auto)
library(boot)

model4 = lm(formula = mpg ~ poly(horsepower, 2), data = Auto)
lines(sort(horsepower), fitted(model4)[order(horsepower)], col='purple', type='line', lwd=3)

# validation set approach
l = length(horsepower)
set.seed(1)
# we take a random sample of half of the values for training the model
train = sample(l, l / 2)

lm.fit1 = lm(mpg ~ horsepower, data=Auto, subset=train)

# calculate test error on what is not the train data
mte = mean((mpg-predict(lm.fit1, Auto))[-train]^2)

# second model
lm.fit2 = lm(mpg~poly(horsepower, 2),data = Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

# third model
lm.fit3 = lm(mpg~poly(horsepower, 3),data = Auto, subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2) #19.26

# little evidence of significance of cubic term
# also, there's no drastic improvement in the test error when adding a new term for the cubic component

# Leave One out validation

#First initialize the output vector as an empty object outside the loop.
fitted_value <- NULL
mses <- NULL
for(i in 1:l) {
  # we train on all except i
  training <- Auto[-i,]
  
  # we validate on the left out
  validation <- Auto[i,]
  model1 <- lm(mpg ~ poly(horsepower, 2), data = training)
  # we estimate the fitted value
  fitted_value[i] <- predict(model1, newdata = validation)
  mses[i] = (mpg[i] - fitted_value[i])^2
}

CV = mean(mses)

# alternative way to calculate the cross validation
loocv = function(fit) {
  # leverage statistics
  h = lm.influence(fit)$h
  # using the alternative formula
  mean((residuals(fit)/(1-h))^2)
}

simple.model = lm(mpg ~ poly(horsepower,2))
loocv(simple.model)
CV

# it can be also done with glm and cv.glm
glm.fit.loocv = glm(mpg ~ horsepower, data=Auto)
cv = cv.glm(Auto, glm.fit.loocv)$delta

# using loocv for different models
cv.error = rep(0, 5)
for (i in 1:5) {
  glm.fit = glm(mpg∼poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# k-fold
#Randomly shuffle the data
yourData <- Auto[sample(nrow(Auto)),]

# Create 10 equally size folds
folds <- cut(seq(1, nrow(yourData)),breaks=10,labels=FALSE)
cut(seq(1, nrow(Auto)),breaks=10, labels=FALSE)
mses <- NULL
fitted_value = NULL

# Perform 10-fold cross validation
for(i in 1:10) {
  # Segement your data by fold using the which() function 
  testIndexes <- which(folds == i, arr.ind=TRUE)
  validation <- Auto[testIndexes, ]
  training <- Auto[-testIndexes, ]
  # Use the test and train data partitions however you desire...
  
  model1 <- lm(mpg ~ poly(horsepower, 2), data = training)
  fitted_values <- predict(model1, newdata = validation)
  mses[i] = mean((mpg[testIndexes] - fitted_values)^2)
}

mean(mses)
# another way to do it is again using cv.glm
# it can be also done with glm and cv.glm
glm.fit = glm(mpg ~ poly(horsepower, 2), data = Auto)
cv = cv.glm(Auto, glm.fit, K = 10)$delta

# using loocv for different models
cv.errors = rep(0, 5)
for (i in 1:5) {
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.errors[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.errors
mean(mses)
set.seed(17)
# The Bootstrap method

alpha.fn = function(data, indices) {
  X=data$X[indices]
  Y=data$Y[indices]
  # using emperical var and emperical cov as estimates for the theoretical ones
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)));
}

set.seed(1)
alpha.fn(Portfolio, sample(100, 100,replace=T))

B = 1000 ## number of bootstraps

results = numeric(B) ## vector to hold results
n = dim(Portfolio)[1]
# генерираме B на брой оценки за alpha 
# manual bootstrap
for(b in 1:B) {
  # we take a sample of size n of the indices of the data
  i = sample(x = 1:n, size = n, replace = TRUE) ## sample indices
  est = alpha.fn(Portfolio, i)
  results[b] = est
}

mean(results)
sd(results)

set.seed(1)
results = numeric(B) ## vector to hold results
n = length(mpg)
# генерираме B на брой оценки за alpha 
# manual bootstrap
for(b in 1:B) {
  # we take a sample of size n of the indices of the data
  i = sample(x = 1:n, size = n, replace = TRUE) ## sample indices
  est = coef(lm(mpg~horsepower, data=Auto, subset=i))[1]
  results[b] = est
}

# or just using the black box magic
boot(Portfolio, alpha.fn, R=1000)

hist(x = results, probability = TRUE, 
     main = "Bootstrapped Samples of Mean_mpg",
     xlab = "theta estimates")

f = function (n) {
  return(1-((n-1)/n)^n)
}

store=rep(NA, 10000)
for(i in 1:10000){
  store[i]=sum(sample(1:100, rep=TRUE)==4)>0
}
mean(store)


