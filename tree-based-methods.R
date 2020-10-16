library(MASS)
library(ISLR)
library(purrr)

colnames(Hitters)

# first example from ISLR
experienced.hitters = Hitters[which(Hitters[,"Years"] < 4.5),]
unexperienced.hitters = Hitters[which(Hitters[,"Years"] >= 4.5),]
Hitters[,"Years"]

mean(experienced.hitters[,"Hits"], na.rm = TRUE)
exp(mean(log(experienced.hitters[,"Salary"]), na.rm = TRUE))

h1 = unexperienced.hitters[which(unexperienced.hitters[,"Hits"] < 117.5),]
h2 = unexperienced.hitters[which(unexperienced.hitters[,"Hits"] >= 117.5),]
mean(log(h1[,"Salary"]), na.rm=TRUE)
mean(log(h2[,"Salary"]), na.rm=TRUE)

(sort(Hitters[,"Years"]))

y = Hitters[, "Salary"]
drops  = c("Salary", "League", "Division", "NewLeague")

split = function () {}



data = na.omit(Hitters[ , !(names(Hitters) %in% drops)])
mean(y[which(data[,"Years"] < mean(data[, "Years"]))], na.rm = TRUE)

Map({function (i) {
  return(c(mean(y[which(data[, i] <= mean(data[,i]))], na.rm=TRUE),
           mean(y[which(data[, i] > mean(data[,i]))], na.rm=TRUE)))
}}, 1:ncol(data))


regression.tree = function (y, data, pred, number.of.predictors, desired.number.of.predictors = 3) {
  # recursively split the dataset by mean of the feature
  # recursion bottоm
  if (number.of.predictors >= desired.number.of.predictors) {
    result = mean(y[which(pred)], na.rm=TRUE)
    return(result)
  }
  
  errors = Map({function (i) {
    less.than.mean = y[which(data[, i] <= mean(data[, i]))]
    greater.than.mean = y[which(data[, i] > mean(data[, i]))]
    mean1 = mean(less.than.mean, na.rm=TRUE)
    mean2 = mean(greater.than.mean, na.rm=TRUE)
    rss1 = sum((less.than.mean - mean1)^2, na.rm=TRUE)
    rss2 = sum((greater.than.mean - mean2)^2, na.rm=TRUE)
    return(rss1 + rss2)
  }}, 1:ncol(data))
  # select one with best error
  best.error.index = which.min(errors)
  pred.new = pred & data[ ,best.error.index] > mean(data[, best.error.index])
  n = number.of.predictors + 1
  return(regression.tree(y, data[,-best.error.index], pred.new, n, desired.number.of.predictors))
}

regression.tree(y,data,rep(TRUE, length(y)), 0)
