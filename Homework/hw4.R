## Part #1 deterministic equidistant design
## Generate n=101 equidistant points in [-2\pi, 2\pi]
m <- 1000
n <- 101
x <- 2*pi*seq(-1, 1, length=n)
## Initialize the matrix of fitted values for three methods
fvlp <- fvnw <- fvss <- matrix(0, nrow= n, ncol= m)
##Generate data, fit the data and store the fitted values
for (j in 1:m){
  ## simulate y-values
  ## Note that you need to replace $f(x)$ below by the mathematical definition in eq. (2)
  y <- (1-x^2)*exp(-0.5*x^2) + rnorm(length(x), sd=0.2);
  ## Get the estimates and store them
  fvlp[,j] <- predict(loess(y ~ x, span = 0.75), newdata = x);
  fvnw[,j] <- ksmooth(x, y, kernel="normal", bandwidth= 0.2, x.points=x)$y;
  fvss[,j] <- predict(smooth.spline(y ~ x), x=x)$y
}
## Below is the sample R code to plot the mean of three estimators in a single plot
meanlp = apply(fvlp,1,mean);
meannw = apply(fvnw,1,mean);
meanss = apply(fvss,1,mean);
dmin = min( meanlp, meannw, meanss);
dmax = max( meanlp, meannw, meanss);
matplot(x, meanlp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, meannw, col="red")
matlines(x, meanss, col="blue")
legend("topright", legend=c("Loess", "NW", "Spline"), col=c("black", "red", "blue"), lty=1)
## You might add the raw observations to compare with the fitted curves
points(x,y)
## Can you adapt the above codes to plot the empirical bias/variance/MSE?
mex_hat <- (1-x^2)*exp(-0.5*x^2)

# Bias plot
biaslp <- meanlp - mex_hat
biasnw <- meannw - mex_hat
biasss <- meanss - mex_hat
dmin_2 <- min(biaslp, biasnw, biasss)
dmax_2 <- max(biaslp, biasnw, biasss)
matplot(x, biaslp, "l", ylim=c(dmin_2, dmax_2), ylab="Bias")
matlines(x, biasnw, col="red")
matlines(x, biasss, col="blue")
legend("topright", legend=c("Loess", "NW", "Spline"), col=c("black", "red", "blue"), lty=1)

# Variance plot
varlp <- rowMeans((fvlp - meanlp)^2)
varnw <- rowMeans((fvnw - meannw)^2)
varss <- rowMeans((fvss - meanss)^2)
dmin_3 <- min(varlp, varnw, varss)
dmax_3 <- max(varlp, varnw, varss)
matplot(x, varlp, "l", ylim=c(dmin_3, dmax_3), ylab="Variance")
matlines(x, varnw, col="red")
matlines(x, varss, col="blue")
legend("center", legend=c("Loess", "NW", "Spline"), col=c("black", "red", "blue"), lty=1)

# MSE plot
mselp <- rowMeans((fvlp - mex_hat)^2)
msenw <- rowMeans((fvnw - mex_hat)^2)
msess <- rowMeans((fvss - mex_hat)^2)
dmin_4 <- min(mselp, msenw, msess)
dmax_4 <- max(mselp, msenw, msess)
matplot(x, mselp, "l", ylim=c(dmin_4, dmax_4), ylab="MSE")
matlines(x, msenw, col="red")
matlines(x, msess, col="blue")
legend("topright", legend=c("Loess", "NW", "Spline"), col=c("black", "red", "blue"), lty=1)

## Part #2 non-equidistant design
## assume you save the file "HW04part2-1.x.csv" in the local folder "C:/temp",
x2 <- read.table(file= "HW04part2-1.x.csv", header=TRUE);
x2_num <- as.numeric(x2[,1])
## within each loop, you can consider the three local smoothing methods:
## please remember that you need to first simulate Y values within each loop
fvlp <- fvnw <- fvss <- matrix(0, nrow= n, ncol= m)
for (j in 1:m){
  ## simulate y-values
  ## Note that you need to replace $f(x)$ below by the mathematical definition in eq. (2)
  y <- (1-x2_num^2)*exp(-0.5*x2_num^2) + rnorm(length(x2_num), sd=0.2);
  ## Get the estimates and store them
  fvlp[,j] <- predict(loess(y ~ x2_num, span = 0.75), newdata = x2_num);
  fvnw[,j] <- ksmooth(x2_num, y, kernel="normal", bandwidth= 0.2, x.points=x2_num)$y;
  fvss[,j] <- predict(smooth.spline(y ~ x2_num), x=x2_num)$y
}
predict(loess(y ~ x2, span = 0.3365), newdata = x2);
ksmooth(x2, y, kernel="normal", bandwidth= 0.2, x.points=x2)$y;
predict(smooth.spline(y ~ x2, spar= 0.7163), x=x2)$y

## Below is the sample R code to plot the mean of three estimators in a single plot
meanlp = apply(fvlp,1,mean);
meannw = apply(fvnw,1,mean);
meanss = apply(fvss,1,mean);
dmin = min( meanlp, meannw, meanss);
dmax = max( meanlp, meannw, meanss);
matplot(x, meanlp, "l", ylim=c(dmin, dmax), ylab="Response")
matlines(x, meannw, col="red")
matlines(x, meanss, col="blue")
legend("topright", legend=c("Loess", "NW", "Spline"), col=c("black", "red", "blue"), lty=1)
## You might add the raw observations to compare with the fitted curves
points(x,y)
## Can you adapt the above codes to plot the empirical bias/variance/MSE?
mex_hat <- (1-x^2)*exp(-0.5*x^2)

# Bias plot
biaslp <- meanlp - mex_hat
biasnw <- meannw - mex_hat
biasss <- meanss - mex_hat
dmin_2 <- min(biaslp, biasnw, biasss)
dmax_2 <- max(biaslp, biasnw, biasss)
matplot(x, biaslp, "l", ylim=c(dmin_2, dmax_2), ylab="Bias")
matlines(x, biasnw, col="red")
matlines(x, biasss, col="blue")
legend("topright", legend=c("Loess", "NW", "Spline"), col=c("black", "red", "blue"), lty=1)

# Variance plot
varlp <- rowMeans((fvlp - meanlp)^2)
varnw <- rowMeans((fvnw - meannw)^2)
varss <- rowMeans((fvss - meanss)^2)
dmin_3 <- min(varlp, varnw, varss)
dmax_3 <- max(varlp, varnw, varss)
matplot(x, varlp, "l", ylim=c(dmin_3, dmax_3), ylab="Variance")
matlines(x, varnw, col="red")
matlines(x, varss, col="blue")
legend("top", legend=c("Loess", "NW", "Spline"), col=c("black", "red", "blue"), lty=1)

# MSE plot
mselp <- rowMeans((fvlp - mex_hat)^2)
msenw <- rowMeans((fvnw - mex_hat)^2)
msess <- rowMeans((fvss - mex_hat)^2)
dmin_4 <- min(mselp, msenw, msess)
dmax_4 <- max(mselp, msenw, msess)
matplot(x, mselp, "l", ylim=c(dmin_4, dmax_4), ylab="MSE")
matlines(x, msenw, col="red")
matlines(x, msess, col="blue")
legend("topright", legend=c("Loess", "NW", "Spline"), col=c("black", "red", "blue"), lty=1)


# EDA
x2_num <- as.numeric(x2[[1]])
y_num <- as.numeric(y[[1]])

hist(x2_num, breaks=10, main="Histogram of Equidistant Dataset", xlab="x", col="lightblue", border="black")
combined_data <- data.frame(equidistant = x, non_equidistant = x2_num)
boxplot(combined_data, col="lightblue", ylab="x")
boxplot(y, main="Boxplot of Y values", col="lightblue", ylab="x")
hist(y_num, breaks=20, main="Histogram of Equidistant Dataset", xlab="x", col="lightblue", border="black")
