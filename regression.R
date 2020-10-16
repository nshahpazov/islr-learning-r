install.packages("ISLR")
install.packages("TSA")
install.packages("sigmoid")
install.packages("purrr")
install.packages("ggplot2")
install.packages("plotly")



library(plotly)
library(MASS)
library(purrr)
library(ISLR)
library(TSA)
library(ggplot2)

# names and colnames is the same for a dataframe
names(Boston)
colnames(Boston) == names(Boston)

model1 = lm(medv~lstat, data=Boston)
attach(Boston)
plot(x=lstat, y=medv)
abline(model1)
summary(model1)



# So boston dataframe rows are different areas
# medv is median value of owner-occupied homes in 1k

names(model1)

# confidence intervals (See Seber on how that's done)
confint(model1)

# conf and prediction intervals for unobserved value (see wikipedia and Seber to know more)
# learn what's the difference
predict(model1, data.frame(lstat=c(5,10,15)), interval ="confidence")
predict(model1, data.frame(lstat=c(5,10,15)), interval ="prediction")

plot(lstat ,medv, pch="+")
abline(model1)

model2 = lm(medv ~ log(lstat))
lines(sort(lstat), fitted(model2)[order(lstat)], col='purple', `typ`e='line', lwd=3)

model3 = lm(medv ~ poly(lstat, 10), data=Boston)
lines(sort(lstat), fitted(model3)[order(lstat)], col='purple', type='line', lwd=3) 

# split plot screen into 4 separate screens
par(mfrow=c(2,2))
plot(model2)

# there seems to be correlation between residuals
plot(residuals(model2))
acf(residuals(model2))
pacf(residuals(model2))

#Standardizing the residuals allows you to view them on the standard scale, 
# which can make outlier detection easier
# testing for homoscedasticity
plot(rstudent(model2))
plot(predict(model2), residuals(model2))

# levarage statistics
hatvalues(model1)
plot(hatvalues(model1))

which.max((hatvalues(model2)))
# how does this help us?

# Multiple Linear Regression
lm.fit = lm(medv~.,data=Boston)
plot(age, medv)
summary(lm.fit)

install.packages("car")
library(car)
vif(lm.fit)
lm.fit1=update(lm.fit, ~.-age)
lm.fit1

names(Carseats)
dat = read.csv("http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv", header = TRUE)
attach(dat)

model1 = lm(Sales ~ TV, data=dat)
attach(dat)
plot(x=TV, y=Sales)
min(TV)
abline(model1)

# step function regression
ranges = seq(from=0, to=297, by = 10)
Xs = my.array <- array(1:24, dim=c(length(ranges),length(TV)))
n = length(TV)
r = length(ranges)

# construct the new indicator predictors
for (i in 1:(r-1)) {
  for (j in 1:n) {
    Xs[i,j] = 0 + (ranges[i] <= TV[j] && TV[j] <= ranges[i + 1])
  }
}

xs.stringified = 1:r %>%
  map(function(x) paste("Xs[",x, ",]"))

formula = as.formula(paste("Sales~", paste(xs.stringified, collapse="+")))
mod1 = lm(formula)

plot(coef(mod1)[3], type="l")
lines(coef(mod1))
lines(5)
abline(a=coef(mod1)[1],b=0)
for(i in (1:length(coef(mod1)))){
  abline(a=coef(mod1)[i],b=0)
}
coef(mod1)
# manually calculating RSS, RSE, confidence intervals
rss = sum(residuals(model1) * residuals(model1))
rse = sqrt(rss/(length(Sales)-2))
coef1 = coefficients(model1)[1]
se.sq.b0 = rse^2*(1/length(TV) + mean(TV)^2/(var(TV)*length(Sales)))

c(coef1 - 1.96*sqrt(se.sq.b0), coef1 + 1.96*sqrt(se.sq.b0))
# We can conclude that 95% of the cases we'll have sales between 
# 6.13689 and 7.928289 thousands without any advertisement at all

coef2 = coefficients(model1)[2]
se.sq.b1 = rse^2/(var(TV)*length(Sales))
se.b1 = sqrt(se.sq.b1)
c(coef2 - 1.96*se.b1, coef2 + 1.96*se.b1)
# we are 95% sure that 
# we'll have about between 42 and 53 units of increase for every 1k spent in TV adds

# we can use std error for Hypotesis testing
# H0 := beta1 = 0
t = coef2 / se.b1
pv = 2 * pt(df = 198, q=t, lower.tail = FALSE)
pv < 0.0005 # we reject the null hypothesis


model3 = lm(Sales ~ ., data=dat)
summary(model3)

attach(Auto)


plot(x=Auto[,4], y=Auto[,1])

model3 = lm(formula = mpg~horsepower, data=Auto)
abline(model3)
model4 = lm(formula = mpg ~ poly(horsepower, 2), data = Auto)
lines(sort(horsepower), fitted(model4)[order(horsepower)], col='purple', type='line', lwd=3)

# with increasing the value of y do we see changing of the errors around that fit?
plot(y = residuals(model3), x = fitted(model3))
sa = zlag(residuals(model4))
sa[1] = 0
lm(residuals(model4) ~ sa)
plot(residuals(model4) - sa)
acf(residuals(model4) - sa)

# we see in the summary that there's enough evidence to reject the null hypothesis that
# there is no correlation between residuals at i and residuals at i - 1

plot(y = residuals(model4), x = fitted(model4))
# here we see variation around 0 but the variation seems to increase with bigger values
# which means we have heteroscedasticity

plot(residuals(model4), zlag(residuals(model4)))
residuals(model4)

model5 = lm(formula = mpg ~ log(horsepower), data = Auto)
summary(model5)
plot(y = residuals(model5), x = fitted(model5))
lines(sort(horsepower), fitted(model4)[order(horsepower)], col='purple', type='line', lwd=3)

plot(rstudent(model5))
indices = which(rstudent(model5) < 3)
filtered.y = mpg[indices]
filtered.x = horsepower[indices]
model6 = lm(filtered.y ~ log(filtered.x))
summary(model6)

pnorm(q=1, lower.tail=FALSE)
plot(x=balance[default == "Yes"], y=income[default == "Yes"], pch=3, col="red")

par(bg="gray")
# we turn the levels to different numeric values so that we use them as indices
plot(income ~ balance, pch=c(3, 1)[as.numeric(default)], col=c("blue", "red")[as.numeric(default)])

# factor variables example
x <- factor(c("single", "married", "married", "single"))
as.numeric(x)

pr = function (x, beta0, beta1) {
  res = exp(beta0 + beta1*x)/(1+exp(beta0 + beta1*x))
  return(res)
}

ll = function(beta0, beta1) {
  res = prod(pr(balance, beta0, beta1)^(default == "Yes")) * prod((1-pr(balance, beta0, beta1))^(default == "No"))
  return(res)
}

ll(-10.6513,0.0055)

xs <- seq(-100, 100, by=1)
ys <- seq(-100, 100, by=1)
x <- seq(-10, 10, length= 30)
y <- x
z <- outer(x, y, ll)
 z[is.na(z)] <- 1

require(lattice)
wireframe(z, drape=T, col.regions=rainbow(100))


likelihood = function(xs, beta0, beta1) {
  
}

l = -20:20

f = function (x, y) {
  return(sin(x)+cos(y))
}

call.func = function (l, f) {
  result = sapply(l, function(x) sapply(l, function(y) f(x,y)))
  return(result)
}

contour(l, l, call.func(l, f))

# heatmap
image(l, l, call.func(l, f))

# some perspective into it
persp(l, l, call.func(l, ll), theta=70, phi=180)

Auto[, -5]


x = runif(200, 0, 10)
e = rnorm(200, 0, 0.5^2)
y = cos(1.25 * (x + 1)) + x/5 + e
plot(x=x,y=y)

# 1. piecewise polynomials begin by diving the range of x into non-overlapping intervals at k ordered x-values
#       t_j (-\infty, t_1], (t_1, t_2], ..., (tk, infity)
# 2. then a degree p polynomial is fit by least square regression to the data in each interval

# the simplest case is
cond = x >= 6
m1 = lm(y[which(cond)] ~ x[cond])

with(x[which(x <= 3)], {abline(m1)})
abline(m1)

b <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)

plot(x=x,y=y)

lm(y~(x-4.1))
read.csv("./reynolds.csv")
e = rnorm(200, sd = 5)
x1 = runif(200, 0, 10)
y1 = 5 * x1 + 3 + e
plot(x1,y1)
x2 = runif(200, 10, 20)
y2 = -5 * x2 + 100 + e
plot(x2,y2)
x=c(x1,x2)
y=c(y1,y2)
plot(x=x,y=y)

xk =  0 + (x >= 10) 
# Y = b0 + b1*(x-x_knot)*xk
y = 33.242 + -2.441 * (x-90)*xk
x.new = x - 10
x.new2= x.new * xk
m1 = lm(y~x.new2)

# spline regression
Quantity <- c(25,39,45,57,70,85,89,100,110,124,137,150,177)
Sales <- c(1000,1250,2600,3000,3500,4500,5000,4700,4405,4000,3730,3400,3300)
data <- data.frame(Quantity,Sales)
data

plot_ly(data,x=~Quantity,
        y=~Sales,
        type="scatter"
)

knot = 89

data$Xbar <- ifelse(data$Quantity>knot, 1, 0)
data$diff <- data$Quantity - knot
data$X <- data$diff*data$Xbar

data
reg <- lm(Sales ~ Quantity + X, data = data)


carfit <- lm(dist~speed+I(speed^2),cars)

# another example
y=c(
  1.540185,
  4.051166,
 5.621000,
  7.752237,
 10.700486,
 10.103224,
 12.150661,
 10.982853,
 11.108116,
14.993672)
x=1:10
plot(x=x,y=y)
x1=1:5
y1=y[1:5]

x2=6:10
y2=y[x2]

abline(lm(y1~x1), col="red")
abline(lm(y2~x2), col="blue")
x3 = ifelse(x<=5, 0, x-5)

plot(x=x,y=y)
lines(x, predict(lm(y ~ ifelse(x<=5,(x-5), 0) + ifelse(x>5,(x-5)^4,0))), col="green")

sn = rexp(300, rate = 10)
sn[1] = 0
jn = cumsum(sn)
ys = 0:299
plot(x=jn, y=ys, type='p')

# generating a simple birth process
lambda = 1/2
states = 1:100
hts = 1:100
for (i in states) {
  hts[i] = rexp(1, lambda * i)
}
hts[1] = 0
jts = cumsum(hts)
