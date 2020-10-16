install.packages("ISLR")
install.packages("TSA")
install.packages("permutations")
library("MASS")
install.packages("sigmoid")

library(MASS)
library(ISLR)
library(permutations)
library(TSA)
attach(Smarket)

glm.fit = glm(Direction ~ Lag1+ Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial)
summary(glm.fit)

coef(glm.fit)

summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]

# what does contrasts do?
contrasts (Direction)
glm.pred = rep("Down", 1250)
glm.pred[glm.probs >.5]="Up"
x = table(glm.pred, Direction)

up.count = length(Direction[which(Direction == "Up")])
down.count = length(Direction) - up.count

correctly.down.predicted.count = x['Down', 'Down'] / down.count
correctly.up.predicted.count = x['Up', 'Up'] / up.count
pred.down.but.up = x["Down", "Up"]
pred.up.but.down = x["Up", "Down"]
pred.down.but.up / up.count # percentage of those which we predicted down but were up out of all up
pred.up.but.down / down.count # percentag of misclassified as up but were down out of all down

(507+145) / 1250 # total correct predictions divided by all i.e. percentage of correct predictions

# mean error (the logistic regression correctly predicted 52% of the time)
mean.error = mean(glm.pred==Direction)


100 - mean.error # training error rate

# but we don't really know the testing error rate
train = (Year < 2005)
Smarket.2005 = Smarket[!train,] # rest data which is not for train
Direction.2005 = Direction[!train]


glm.fit2 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket ,family = binomial, subset = train)
glm.probs2 = predict(glm.fit2, Smarket.2005, type="response")

glm.pred2 = rep("Down", 252)
glm.pred2[glm.probs2 >.5] = "Up"
table(glm.pred2, Direction.2005)

mean(glm.pred2 == Direction.2005)
mean(glm.pred2 != Direction.2005) # test error rate

# less predictors
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket ,family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs >.5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

# further predicting 
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),type="response")

# lda
lda.fit = lda(Direction ~ Lag1 + Lag2, subset=train, data=Smarket)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class ,Direction.2005)


# problem 6
x1 = 50
x2 = 3.5
b0 = -6
b1 = 0.05
b2 = 1

p = exp(b0 + b1*x1 + b2*x2)/(1 + exp(b0 + b1*x1 + b2*x2))
p

# problem 7
mean1 = 10
mean2 = 0
v = 36

pi1 = .8
pi2 = .2
d1 = dnorm(x = 4,mean = 10, sd = 6)
d2 = dnorm(x = 4, mean = 0, sd = 6)

(pi1*d1) / (pi1*d1 +pi2 * d2)

# problem 9
0.37/1.37
16/(100-16)

# inverse transform sampling
y = runif(1000)
x = log(1-y)/-0.5
plot(ecdf(x))
hist(x)

# looks like we generated it


rng = (1:10000)
y = (1+1/rng)^rng
rate = abs(y[2:10000]-exp(1))/abs(y[1:9999]-exp(1))
plot(rate)
plot(y)

a = rng/2^rng

plot(a[2:10000]/(a[1:9999]))

x = 1/(2^(2^rng))
s = x[2:10000]/(x[1:9999])
plot(y=s, x=1:9999)
plot((x[2:10000]/(x[1:9999]))

plot(x)

