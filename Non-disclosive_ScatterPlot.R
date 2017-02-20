# NON-DISCLOSIVE SCATTER PLOT USING THE CENTROIDS OF EACH 3-NEAREST NEIGHBOURS

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

# points(x.centroid, y.centroid, col='green', pch=3)
points(x.new, y.new, col='black', pch=3)
abline(lm(y.new~x.new), col='black', lty=2)

legend(6.8, 13, c("Original data", "Original data trendline", "Masked data", "Masked data trendline"), pch=c(1,NA,3,NA), lty=c(NA,1,NA,2)) 

dev.off()