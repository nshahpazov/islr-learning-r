install.packages("ISLR")
install.packages("TSA")
install.packages("leaps")

library(MASS)
library(ISLR)
library(leaps)
library(TSA)
attach(Auto)
library(boot)

?Credit

dim(Credit)

# we remove the name from the data
data = Auto[,-9]

forward.stepwise = function (data, name) {
  fields = colnames(data)
  p = ncol(data) # number of predictors
  # iterate from 1 to the number of predictors
  best.test.error = 40
  
  for (k in (1:(p-1))) {
    formula = as.formula(paste("mpg~", paste(fields[k], collapse="+")))
  }
}


# best subset selection in a brute force manner
bss = function (data, predictor.name) {
  # estimate the null model
  fields = colnames(data)
  p = ncol(data) # number of predictors
  # iterate from 1 to the number of predictors
  best.test.error = 40
  for (k in (1:(p-1))) {
    # all combinations
    combinations = combn((1:p)[-which(fields == predictor.name)], k)
    # print(combinations)
    models = vector(mode="list", length=ncol(combinations))
    min.dev = 10000
    min.dev.index = 0
    for (i in 1:ncol(combinations)) {
      # c is an array of two elements - the combinations
      formula = as.formula(paste("mpg~", paste(fields[combinations[,i]], collapse="+")))
      print(formula)
      models[[i]] = glm(data=data, formula = formula)
      
      current.dev = deviance(models[[i]])
      if (current.dev <= min.dev) {
        min.dev = current.dev
        min.dev.index = i
      }
    }
    currently.best.model = models[[min.dev.index]]
    # calculate the test error using cross validation
    current.test.error = cv.glm(data, currently.best.model, K = 10)$delta
    if (current.test.error < best.test.error) {
      best.test.error = current.test.error
      best.model = currently.best.model
    }
  }
  # pick best of the bests using CV statistic
  return(best.model)
}

x = bss(data, "mpg")

# forward stepwise selection
fss = function (data, fields) {
  # init the null model
}
plot(x=Auto[2], y=Auto[1])
model12 = lm(mpg ~ poly(horsepower, 12), data=Auto)
lines(sort(horsepower), fitted(model12)[order(horsepower)], col='purple', type='line', lwd=3)

# Ridge regression
plot(y)
lines(sort(x), fitted(m)[order(x)], col='purple', type='line', lwd=1)

mpg = Auto[,1]
dm = data.matrix(Auto[,-c(1,8,9)])
lambda = 40
design = cbind(intercept=1, dm)
bhats = solve(t(X2) %*% X2 + lambda * diag(ncol(design))) %*% t(X2) %*% mpg
X2 %*% bhats
# we chose the lambda penalizer by cross validation again


# LASSO REGRESSION
mod1 = lm(mpg ~ ., data = Auto[, -c(1,8,9)])
gamma = 0.001
betas.ls = coefficients(mod1)

pos.parts = ifelse(abs(betas.ls)-gamma <= 0, 0, abs(betas.ls)-gamma)
betas.lasso = sign(betas.ls) * pos.parts
