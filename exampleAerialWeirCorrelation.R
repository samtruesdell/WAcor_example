

# variables: weir and aerial survey standard
# deviations, the correlation between the surveys
# and the number of samples

sd_weir <- 10
sd_aerial <- 50
cor_wa <- 0.5
n <- 50



crmat <- matrix(c(1,cor_wa,cor_wa,1), nrow=2, ncol=2)
sds <- c(sd_weir, sd_aerial)
cvmat <- crmat
cvmat <- crmat * prod(sds)
diag(cvmat) <- sds^2

x0 <- matrix(rep(seq(200, 300, length.out=n),2), ncol=2)

x1 <- t(sapply(1:n, function(x) 
               rmvnorm(1, mean=x0[x,], sigma=cvmat)))


hl <- seq(100, 400, 10)
vl <- seq(-10, n+0.1*n, 1)

plot(x0[,1], type='n', ylim=range(x0,x1))
abline(h=hl, v=vl, lty=3, col='gray80')
lines(x0[,1], type='l', ylim=range(x0,x1))
matplot(x1, type='o', add=TRUE, pch=16,
        col=c('blue', 'red'))
box()

legend('topleft', pch=c(NA, 16, 16), lty=1,
       col=c('black', 'blue', 'red'),
       legend=c('true values', 'weir', 'aerial'),
       bty='n', cex=1.5)


print(cor(x1-x0[,1]))
print(var(x1-x0[,1]))






