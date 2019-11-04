# teeth
Data Replication Project

hi!
this is tony editing your file

# What is PCA?
a type of linear transformation on a given data set that has values for a certain number of variables (coordinates) for a certain amount of spaces
This linear transformation fits this dataset to a new coordinate system in such a way that the most significant variance is found on the first coordinate, and each subsequent coordinate is orthogonal to the last and has a lesser variance
Where many variables correlate with one another, they will all contribute strongly to the same principal component
Each principal component sums up a certain percentage of the total variation in the dataset
As you add more principal components, you summarize more and more of the original dataset. Adding additional components makes your estimate of the total dataset more accurate, but also more unwieldy

The first principal component is constructed to account for as much of the spread in the data as possible. Each subsequent PC is calculated in the same manner but under a requirement that it is orthogonal (and therefore uncorrelated) to the previous PC, and should account for the maximum amount of the remaining variance in the data
Useful when you have a dataset that either has a large number of variables, and/or you have variables that have been measured in different scales


# Eigenvalues and Eigenvectors

Simply put, an eigenvector is a direction, such as "vertical" or "45 degrees,"
an eigenvalue is a number telling you how much variance there is in the data in that direction

THE EIGENVECTOR WITH THE HIGHEST EIGENVALUE IS, THEREFORE, THE FIRST PRINCIPLE COMPONENT
The number of eigenvalues and eigenvectors that exits is equal to the number of dimensions the data set has
EX: 2 VARIABLES = 2 DIMENSIONAL

**Eigenvectors**: the direction in which the particular PC is oriented

**Eigenvalues**: the corresponding amount of variance that is accounted for by a each PC.
PC scores: new variables calculated by multiplying the original variables measured for each datapoint by the eigenvectors, and then summing them together. PC scores are what can be plotted to visualize the data after reducing the dimensionality of it

library(ggplot2)

library(curl)

library(psych)

library(factoextra)


f <- curl("https://raw.githubusercontent.com/benrod86/PCA/master/Froggy_hop_hop.csv")

h <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = TRUE)

head(h)

Data = h[, 1:8]

head(Data)

CorDat = cor(Data)

CorDat

pairs(Data, panel = function(x, y) {points(x, y) abline(lm(y ~ x), col = "red", lwd = 2)})

Are the variables correlated? Do you think that there is redundancy in the dataset?
# Bartlett's Test of Sphericity
One basic test  is BARTLETT’S TEST OF SPHERICITY, in which the null hypothesis states that there is no correlation between variables

If the variables were all orthogonal (no relationship between the variables), there would be no way to combine the variables as components

N = dim(Data)[1]

cortest.bartlett(CorDat, n = N)

# calculate eigenvalues and eigenvectors
eigVal = eigen(CorDat)

vectData = eigVal$vectors

eigValv = eigVal$values

eigValv
# Deciding how many components to retain
Kaiser’s rule: suggests to retain components which eigenvalues > 1. Since our first eigenvalue (4.61) is > 1, we have support to retain the first component. The second eigenvalue (0.93) is < 1 so we may opt to drop the second component and just retain the first one

# plotting eigenvalues through a scree plot
scree plot: allow us to visualize how the variance represented by each PC drops off

plot(c(1:length(eigVal$values)), eigVal$values, type = "l", xlab = "Principal Components", ylab = "Eigenvalues")

points(c(1:length(eigVal$values)), eigVal$values, pch = 19)

# same scree plot but using ggplot2
library(factoextra)
prt = prcomp(Data, center = TRUE, scale = TRUE)
summary(prt)
fviz_eig(prt, xlab = "Principal Components")
