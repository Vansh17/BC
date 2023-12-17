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
