# METHOD 1 : CENTROID OF 3-NEAREST NEIGHBOURS (RESULTS)

setwd("C:/Users/da15564/Dropbox/Demetris_BRISTOL/Masking sensitive microdata in Scatterplots/k-NN/3-NN")

set.seed(050516)

# number of data points
n <- 100

x <- rnorm(n, 10, 1)
y <- x + rnorm(n, 0, 1)
data <- cbind(x,y)

# Load the RANN package to use the 'nn2' function that searches for the Nearest Neighbours  
library(RANN)

# calculate the coordinates of the centroids by averaging the x and y coordinates of nearest neighbours separately

neighbours <- 3

# Find the two nearest neighbours of each data point 
nearest <- nn2(data, k = neighbours)

# Calculate the centroid of each n nearest data points 
x.centroid <- matrix()
y.centroid <- matrix()
for (i in 1:n){
  x.centroid[i] <- mean(x[nearest$nn.idx[i,1:neighbours]])
  y.centroid[i] <- mean(y[nearest$nn.idx[i,1:neighbours]])
}

# Shift the new data points to the origin
x.shifted <- matrix()
y.shifted <- matrix()
for (i in 1:n){
  x.shifted[i] <- x.centroid[i] - mean(x.centroid)
  y.shifted[i] <- y.centroid[i] - mean(y.centroid)
}

# Calculate the scaling factor
x.scalingFactor <- sqrt(var(x))/sqrt(var(x.centroid))
y.scalingFactor <- sqrt(var(y))/sqrt(var(y.centroid))

# Apply the scaling factor to the shifted centroids
x.masked <- x.shifted * x.scalingFactor
y.masked <- y.shifted * y.scalingFactor

# Shift the scaled data back to their actual position
x.new <- x.masked + mean(x.centroid)
y.new <- y.masked + mean(y.centroid)

min <- min(min(x), min(y))
max <- max(max(x), max(y))

# Create and save the Scatterplot

tiff(filename = "ScatterPlot.tiff", width = 480, height = 480)

# set the margins around the plot window
par(mar=c(4.5, 4.5, 1.5, 1.5))

plot(x, y, xlim=c(min, max), ylim=c(min, max), pch=1)
abline(lm(y~x))
wider.x.interval <- seq(min-5, max+5, length.out=n)
confint <- predict(lm(y~x), newdata=data.frame(x=wider.x.interval), interval="confidence")
lines(wider.x.interval, confint[,2], lty=2)
lines(wider.x.interval, confint[,3], lty=2)

# points(x.centroid, y.centroid, col='green', pch=3)
points(x.new, y.new, col='red', pch=3)
abline(lm(y.new~x.new), col='red')
confint.masked <- predict(lm(y.new~x.new), newdata=data.frame(x.new=wider.x.interval), interval="confidence")
lines(wider.x.interval, confint.masked[,2], lty=2, col='red')
lines(wider.x.interval, confint.masked[,3], lty=2, col='red')
legend(6.8, 13, c("Original Data", "Masked Data"), pch=c(1,3), col=c("black","red")) 

dev.off()

# Create and save the Histograms

tiff(filename = "Histograms.tiff", width = 960, height = 480)

# set the margins around the plot window
par(mar=c(4.5, 4.5, 1.5, 1.5))

x.minmin <- min(min(x),min(x.new))
x.maxmax <- max(max(x), max(x.new))
x.range <- x.maxmax - x.minmin
y.minmin <- min(min(y),min(y.new))
y.maxmax <- max(max(y), max(y.new))
y.range <- y.maxmax - y.minmin

x.old <- x

par(mfrow = c(1, 2))
hist(x.new, col='red', density=20, angle=45, prob=TRUE, xlim=c((x.minmin-0.1*x.range),(x.maxmax+0.1*x.range)), xlab='x', main='')
curve(dnorm(x, mean=mean(x.new), sd=sd(x.new)), lty=1, add=TRUE, col='red')
hist(x.old, add=TRUE, prob=TRUE)
curve(dnorm(x, mean=mean(x.old), sd=sd(x.old)), lty=2, add=TRUE)

hist(y.new, col='red', density=20, angle=45, prob=TRUE, xlim=c((y.minmin-0.1*y.range),(y.maxmax+0.1*y.range)), xlab='y', main='')
curve(dnorm(x, mean=mean(y.new), sd=sd(y.new)), lty=1, add=TRUE, col='red')
hist(y, add=TRUE, prob=TRUE)
curve(dnorm(x, mean=mean(y), sd=sd(y)), lty=2, add=TRUE)

dev.off()

# Create and save QQ plots and other plot diagnostics

tiff(filename = "PlotDiagnostics.Original.tiff", width = 960, height = 1440)
par(mfrow = c(3, 2))
plot(lm(y~x), which=c(1:6), sub.caption='Original Data')
dev.off()

tiff(filename = "PlotDiagnostics.Masked.tiff", width = 960, height = 1440)
par(mfrow = c(3, 2))
plot(lm(y.new~x.new), main='Masked Data', which=c(1:6))
dev.off()

# other statistics

# mean and variance
matMean <- matrix(c(mean(x), mean(x.new), mean(y), mean(y.new)), ncol=2, nrow=2, dimnames=list(c("original data", "masked data"),c("mean(x)", "mean(y)")))
matVar <- matrix(c(var(x), var(x.new), var(y), var(y.new)), ncol=2, nrow=2, dimnames=list(c("original data", "masked data"),c("var(x)", "var(y)")))

# covariance and correlation
matCov <- matrix(c(cov(x,y), cov(x.new,y.new)), ncol=1, nrow=2, dimnames=list(c("original data", "masked data"),c("cov(x,y)")))
matCor <- matrix(c(cor(x,y), cor(x.new,y.new)), ncol=1, nrow=2, dimnames=list(c("original data", "masked data"),c("cor(x,y)")))

# Load the e1071 package to calculate the skewness and kurtosis
library(e1071)
# skewness and kurtosis
matSkew <- matrix(c(skewness(x), skewness(x.new), skewness(y), skewness(y.new)), ncol=2, nrow=2, dimnames=list(c("original data", "masked data"),c("skew(x)", "skew(y)")))
matKurt <- matrix(c(kurtosis(x), kurtosis(x.new), kurtosis(y), kurtosis(y.new)), ncol=2, nrow=2, dimnames=list(c("original data", "masked data"),c("kurt(x)", "kurt(y)")))

matStats <- cbind(matMean, matVar, matCov, matCor, matSkew, matKurt)
matStats


# Create and save Mahalanobis-Mahalanobis plots
mahalanobis.x <- matrix()
mahalanobis.y <- matrix()
mahalanobis.x.new <- matrix()
mahalanobis.y.new <- matrix()
for (i in 1:n){
  mahalanobis.x[i] <- mahalanobis(x[i], center=mean(x), cov=var(x))
  mahalanobis.x.new[i] <- mahalanobis(x.new[i], center=mean(x.new), cov=var(x.new))
  mahalanobis.y[i] <- mahalanobis(y[i], center=mean(y), cov=var(y))
  mahalanobis.y.new[i] <- mahalanobis(y.new[i], center=mean(y.new), cov=var(y.new))
}
tiff(filename = "MahalanobisMahalanobis.tiff", width = 960, height = 480)
par(mfrow = c(1, 2))
plot(mahalanobis.x, mahalanobis.x.new)
plot(mahalanobis.y, mahalanobis.y.new)
dev.off()
