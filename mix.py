# ********************************EXP 1
library('LearnBayes')
data(studentdata)
print(studentdata[1:10,])

table(studentdata$Drink)

table(studentdata$Height)

table(studentdata$Drink)

barplot(table(studentdata$Drink),xlab="Drink",ylab="Count")

hours.of.sleep = studentdata$WakeUp - studentdata$ToSleep
summary(hours.of.sleep)

hist(hours.of.sleep,main="")

boxplot(hours.of.sleep~studentdata$Gender,ylab="Hours of Sleep")

female.Haircut=studentdata$Haircut[studentdata$Gender=="female"]
summary(female.Haircut)

male.Haircut=studentdata$Haircut[studentdata$Gender=="male"]
summary(male.Haircut)

hist(studentdata$Dvds)
print(summary(studentdata$Dvds))

print(table(studentdata$Dvds))
barplot(table(studentdata$Dvds))

boxplot(studentdata$Height~studentdata$Gender)
# **********************************************************************************************************
# ************************************************************************************Using a Discrete Prior** EXP 2
library('LearnBayes')
p <- seq(0.05, 0.95, by = 0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior / sum(prior)
plot(p, prior, type = "h", ylab="Prior Probability")
# ********************************The posterior for p:
data <- c(11, 16)
post <- pdisc(p, prior, data)
round(cbind(p, prior, post),2)
 

library(lattice)
PRIOR <- data.frame("prior", p, prior)
POST <- data.frame("posterior", p, post)
names(PRIOR) <- c("Type", "P", "Probability")
names(POST) <- c("Type","P","Probability")
data <- rbind(PRIOR, POST)

xyplot(Probability ~ P | Type, data=data, 
       layout=c(1,2), type="h", lwd=3, col="black")
# ******************************Using a Beta Prior
quantile2 <- list(p=.9, x=.5)
quantile1 <- list(p=.5, x=.3)
(ab <- beta.select(quantile1,quantile2))
# ************************Bayesian triplot:
a <- ab[1]
b <- ab[2]
s <- 11
f <- 16
curve(dbeta(x, a + s, b + f), from=0, to=1, xlab="p", ylab="Density", lty=1, lwd=4)
curve(dbeta(x, s + 1, f + 1), add=TRUE, lty=2, lwd=4)
curve(dbeta(x, a, b), add=TRUE, lty=3, lwd=4)
legend(.7, 4, c("Prior", "Likelihood", "Posterior"), lty=c(3, 2, 1), lwd=c(3, 3, 3))
# **************************
1 - pbeta(0.5, a + s, b + f)
qbeta(c(0.05, 0.95), a + s, b + f)
ps <- rbeta(1000, a + s, b + f)
hist(ps, xlab="p")
# **********************
sum(ps >= 0.5) / 1000
quantile(ps, c(0.05, 0.95))
# *****************************************************HISTO PRIOR
midpt <- seq(0.05, 0.95, by = 0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 
           0.1, 0, 0)
prior <- prior / sum(prior)
curve(histprior(x, midpt, prior), from=0, to=1,  ylab="Prior density", ylim=c(0, .3))
# *********************
curve(histprior(x,midpt,prior) * dbeta(x, s + 1, f + 1), from=0, to=1, ylab="Posterior density")
p <- seq(0, 1, length=500)
post <- histprior(p, midpt, prior) * dbeta(p, s + 1, f + 1)
post <- post / sum(post)
ps <- sample(p, replace = TRUE, prob = post)

hist(ps, xlab="p", main="")
# **********************************************************PREDICTIOn------ DISCRETE PRIOR APPROACH
p <- seq(0.05, 0.95, by=.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior / sum(prior)
m <- 20
ys <- 0:20
pred <- pdiscp(p, prior, m, ys)
cbind(0:20, pred)
# ************************************************CONTINUOUS PRIOR
ab <- c(3.26, 7.19)
m <- 20
ys <- 0:20
pred <- pbetap(ab, m, ys)
# **********************************************************Simulating predictive distribution:
p <- rbeta(1000, 3.26, 7.19)
y <- rbinom(1000, 20, p)
table(y)
# ****************************
freq <- table(y)
ys <- as.integer(names(freq))
predprob <- freq / sum(freq)
plot(ys, predprob, type="h", xlab="y", ylab="Predictive Probability")
dist <- cbind(ys, predprob)
# ****************************************************Construction of a prediction interval:
covprob <- .9
discint(dist, covprob)
# *************************************************************************************************************
# ************************************************ EXP 3
import numpy as np
import matplotlib.pyplot as plt
radius = 1

N = 100000   
X = np.random.uniform(low=-radius, high=radius, size=N) 
Y = np.random.uniform(low=-radius, high=radius, size=N)   


R = np.sqrt(X**2+Y**2);  

box_area =(2.0*radius)**2      
is_point_inside = R<radius
N_inside=np.sum(is_point_inside)
circle_area = box_area*N_inside/N

plt.scatter(X,Y, c=is_point_inside, s=5.0, edgecolors='none', cmap=plt.cm.Paired)  
plt.axis('equal')


print("Area of the circle = ", circle_area)
print("pi = ", circle_area/radius**2)
plt.show()
# *************************************************************************************************
# Have normally distributed data where the data is observed in grouped form. Consider the posterior of (μ,logσ)***** EXP 8
d <- list(int.lo=c(-Inf, seq(66, 74, by=2)),
        int.hi=c(seq(66, 74, by=2), Inf),
        f=c(14, 30, 49, 70, 33, 15))
y <- c(rep(65,14), rep(67,30), rep(69,49),
       rep(71,70), rep(73,33), rep(75,15))

mean(y)
## [1] 70.16588
log(sd(y))
## [1] 0.9504117
# ****************************************First obtain normal approximation to posterior.
start <- c(70, 1)
fit <- laplace(groupeddatapost, start, d)
fit
## $mode
## [1] 70.169880  0.973644
## 
## $var
##              [,1]         [,2]
## [1,] 3.534713e-02 3.520776e-05
## [2,] 3.520776e-05 3.146470e-03
## 
## $int
## [1] -350.6305
## 
## $converge
## [1] TRUE

# ************************************Now use a Metropolis (random walk) MCMC algorithm.
modal.sds <- sqrt(diag(fit$var))
proposal <- list(var=fit$var, scale=2)
fit2 <- rwmetrop(groupeddatapost,
                 proposal,
                 start,
                 10000, d)
fit2$accept
## [1] 0.2908
post.means <- apply(fit2$par, 2, mean)
post.sds <- apply(fit2$par, 2, sd)
cbind(c(fit$mode), modal.sds)
##                 modal.sds
## [1,] 70.169880 0.18800834
## [2,]  0.973644 0.05609341
cbind(post.means, post.sds)
##      post.means   post.sds
## [1,] 70.1636783 0.18672292
## [2,]  0.9811132 0.05767941
mycontour(groupeddatapost,
          c(69, 71, .6, 1.3), d,
       xlab="mu",ylab="log sigma")
points(fit2$par[5001:10000, 1],
       fit2$par[5001:10000, 2])
# *********************************************************************************************
# ******************************************************Linear Regression
import matplotlib.pyplot as plt
from scipy import stats

x = [5,7,8,7,2,17,2,9,4,11,12,9,6]
y = [99,86,87,88,111,86,103,87,94,78,77,85,86]

slope, intercept, r, p, std_err = stats.linregress(x, y)

def myfunc(x):
  return slope * x + intercept

mymodel = list(map(myfunc, x))

plt.scatter(x, y)
plt.plot(x, mymodel)
plt.show()