

# variables: weir and aerial survey standard
# deviations, the correlation between the surveys
# and the number of samples
# 



sd_weir <- 10
sd_aerial <- 50
cor_wa <- 0.5
n <- 50



library(mvtnorm)

# get the covariance matrix
crmat <- matrix(c(1,cor_wa,cor_wa,1), nrow=2, ncol=2)
sds <- c(sd_weir, sd_aerial)
cvmat <- crmat
cvmat <- crmat * prod(sds)
diag(cvmat) <- sds^2

# the mean values
x0 <- matrix(rep(seq(200, 300, length.out=n),2), ncol=2)

xTEST <- 32

# here is a change
ytest <- 33.3

#another change
ztest =0

# get the deviations from the mean
x1 <- rmvnorm(n, mean=c(0, 0), sigma=cvmat)

# new trend for the indices
x2 <- x1 + x0

# graph paper setup
hl <- seq(100, 400, 10)
vl <- seq(-10, n+0.1*n, 1)

# plot the true mean and the deviations
# for both the surveys
plot(x0[,1], type='n', ylim=range(x0,x2),
     xlab='Year', ylab='Index value')
abline(h=hl, v=vl, lty=3, col='gray80')
lines(x0[,1], type='l', ylim=range(x0,x2),
      lwd=3, col='gray30')
matplot(x2, type='o', add=TRUE, pch=16,
        col=c('blue', 'red'))
box()

legend('topleft', pch=c(NA, 16, 16), lty=1,
       col=c('gray30', 'blue', 'red'),
       legend=c('true values', 'weir', 'aerial'),
       bty='n', cex=1.25, title=paste('Cor:', cor_wa))


# correlation of deviations
print(cor(x1))

# observed correlation with trend
print(cor(x2))






