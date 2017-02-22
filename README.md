# Prototyped DataSHIELD Scatter Plot Function
---------
The following functions are prototypes for a privacy preserving scatter plot function in [DataSHIELD](https://github.com/datashield) based on the k-Nearest Neighbours algorithm. 

`centroids2dDS.b.R`
This function is a server-side function that returns the centroids of each n nearest neighbours. The function searches for the nearest neighbours of each data point in a 2-dimensional space. Nearest neighbours are data points with minimum distance metrics (e.g. Euclidean distance) between them. Each data point and its n-1 nearest neighbours are then used for the calculation of the coordinates of the centroid of those n points. The centroid has x-coordinate the average value of the x-coordinates of the n nearest neighbours and y-coordinate the average of the y-coordinates of the n nearest neighbours. The coordinates of the centroids are then returned to the client-side.

`ds.scatterPlot.b.R`
This function is a client-side function that generates a 2-dimensional scatter plot of the centroids of each n nearest neighbours. The function calls the server-side function centroids2dDS that calculates and returns the coordinates of the centroids of each n nearest neighbours.

`QA.ds.ScatterPlot.R`
This QA script checks the DataSHIELD client-server pair of functions that create non-disclosive 2-dimensional scatter plots.

# Test R script
The script `Non-disclosive_ScatterPlot.R` can run outside of DataSHIELD in native R. It produces a simulated dataset and creates a scatter plot utilising the same approach as the pair of DataSHIELD client-server functions.  


