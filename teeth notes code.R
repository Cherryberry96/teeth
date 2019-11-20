f <- "~/Desktop/Data Replication Stuff/S4 Table restructured.xlsx"
library(readxl)
TD <- read_excel(f, sheet = 1, col_names = TRUE)
head(TD)


#TD1 <- data.frame(TD,stringsAsFactors = FALSE)
#TD1 <- as.numeric(TD)
#as.numeric(data.frame(TD, row.names = NULL, check.rows = FALSE,
 #                     check.names = TRUE, fix.empty.names = TRUE,
  #                    stringsAsFactors = default.stringsAsFactors()))
#class(TD1)
#TD1$...9
#TD1[6, 9]
#class(TD1[6, 9])


class(TD$MAXI1MD)
class(TD$MAXI1LL)
class(TD$MAX12MD)
class(TD$MAXI2LL)
class(TD$MAXCMD)
class(TD$MAXCLL)
class(TD$MAXP1MD)
class(TD$MAXP1BL)
class(TD$MAXP2MD)
class(TD$MAXP2BL)
class(TD$MAXM1MD)


class(TD$MAXM1BL)


class(TD$MAXM2MD)
class(TD$MAXM2BL)
class(TD$MAXM3MD)
class(TD$MAXM3BL)
class(TD$MANDLI1MD)
class(TD$MANDLI1LL)
class(TD$MANDLI2MD)
class(TD$MANDLI2LL)
class(TD$MANDLCMD)
class(TD$MANDLCLL)
class(TD$MANDLP1MD)
class(TD$MANDLP1BL)
class(TD$MANDLP2MD)
class(TD$MANDLP2BL)
class(TD$MANDLM1MD)
class(TD$MANDLM1BL)


class(TD$MANDLM2MD)
class(TD$MANDLM2BL)


class(TD$MANDLM3MD)
class(TD$MANDLM3BL)




Data = TD[, 5:36]
head(Data)


CorDat = cor(Data)
CorDat

CorDat = cor(dat)
CorDat

library(ggplot2)
library(psych)
library(factoextra)


# CORRELATION MATRIX
pairs(Data, panel = function(x, y) {
  points(x, y)
  abline(lm(y ~ x), col = "red", lwd = .000000000001)
})


pairs(dat, panel = function(x, y) {
  points(x, y)
  abline(lm(y ~ x), col = "red", lwd = 1)
})

# BARTLETT'S TEST OF SPHERICITY
N = dim(Data)[1]
cortest.bartlett(CorDat, n = N)

N = dim(Data)
cortest.bartlett(CorDat, n = N)



N = dim(dat)[1]
cortest.bartlett(CorDat, n = N)


is.na(Data)
dat <- na.omit(Data)
dat
is.na(dat)



eigVal = eigen(CorDat)
vectData = eigVal$vectors
eigValv = eigVal$values
eigValv

plot(c(1:length(eigVal$values)), eigVal$values, type = "l", xlab = "Principal Components", 
     ylab = "Eigenvalues")
points(c(1:length(eigVal$values)), eigVal$values, pch = 19)

library(factoextra)
prt = prcomp(dat, center = TRUE, scale = TRUE)
summary(prt)



fviz_eig(prt, xlab = "Principal Components")



scoreData <- prt$x
LoadsData <- prt$rotation
LoadsData

barplot(LoadsData[, 1])
barplot(LoadsData[, 2])



biplot(scoreData[, c(1, 2)], LoadsData[, c(1, 2)], xlabs = rep("*", 980), col = c("orange", 
                                                                                  "black"))




z <- read_excel(f, sheet = 1, col_names = TRUE)
names(z)
z<-mutate(z,MAXI1AREA=MAXI1MD*MAXI1LL)


z<-mutate(z,MAXP1AREA=MAXP1MD*MAXP1BL)
z<-mutate(z,MAXP2AREA=MAXP2MD*MAXP2BL)
z<-mutate(z,MAXM1AREA=MAXM1MD*MAXM1BL)
z<-mutate(z,MAXM2AREA=MAXM2MD*MAXM2BL)

z<-mutate(z,MAXAVG=(MAXP1AREA+MAXP2AREA+MAXM1AREA+MAXM2AREA)/4)
z<-mutate(z,MAXCSF=sqrt(MAXAVG))

z<-mutate(z,SAMAXP1MD=MAXP1MD/MAXCSF)
z<-mutate(z,SAMAXP2MD=MAXP2MD/MAXCSF)
z<-mutate(z,SAMAXM1MD=MAXM1MD/MAXCSF)
z<-mutate(z,SAMAXM2MD=MAXM2MD/MAXCSF)


z<-mutate(z,SAMAXP1BL=MAXP1BL/MAXCSF)
z<-mutate(z,SAMAXP2BL=MAXP2BL/MAXCSF)
z<-mutate(z,SAMAXM1BL=MAXM1BL/MAXCSF)
z<-mutate(z,SAMAXM2BL=MAXM2BL/MAXCSF)




z<-mutate(z,MANDP1AREA=MANDLP1MD*MANDLP1BL)
z<-mutate(z,MANDM1AREA=MANDLM1MD*MANDLM1BL)
z<-mutate(z,MANDM2AREA=MANDLM2MD*MANDLM2BL)
z<-mutate(z,MANDM3AREA=MANDLM3MD*MANDLM3BL)

z<-mutate(z,MANDAVG=(MANDP1AREA+MANDM1AREA+MANDM2AREA+MANDM3AREA)/4)
z<-mutate(z,MANDCSF=sqrt(MANDAVG))

z<-mutate(z,SAMANDP1MD=MANDLP1MD/MANDCSF)
z<-mutate(z,SAMANDM1MD=MANDLM1MD/MANDCSF)
z<-mutate(z,SAMANDM2MD=MANDLM2MD/MANDCSF)
z<-mutate(z,SAMANDM3MD=MANDLM3MD/MANDCSF)

z<-mutate(z,SAMANDP1BL=MANDLP1BL/MANDCSF)
z<-mutate(z,SAMANDM1BL=MANDLM1BL/MANDCSF)
z<-mutate(z,SAMANDM2BL=MANDLM2BL/MANDCSF)
z<-mutate(z,SAMANDM3BL=MANDLM3BL/MANDCSF)

#SA MAX Diameters
z[, 50:57]
max <- select(z,SAMAXP1MD,SAMAXP2MD,SAMAXM1MD,SAMAXM2MD,SAMAXP1BL,SAMAXP2BL,SAMAXM1BL,SAMAXM2BL)
new_max <- na.omit(max)
max.pca <- prcomp(new_max, center = TRUE, scale. = TRUE)
summary(max.pca)
biplot(max.pca$x[, c(1, 2)], max.pca$rotation[, c(1, 2)])

#SA MAND Diameters
z[, 58:65]
mand <- select(z,SAMANDP1MD,SAMANDM1MD,SAMANDM2MD,SAMANDM3MD,SAMANDP1BL,SAMANDM1BL,SAMANDM2BL,SAMANDM3BL)
new_mand <- na.omit(mand)
mand.pca <- prcomp(new_mand, center = TRUE, scale. = TRUE)
summary(mand.pca)
biplot(mand.pca$x[, c(1, 2)], mand.pca$rotation[, c(1, 2)])


t <- select(z,Class,SAMAXP1MD,SAMAXP2MD,SAMAXM1MD,SAMAXM2MD,SAMAXP1BL,SAMAXP2BL,SAMAXM1BL,SAMAXM2BL)
z1 <- na.omit(t)
z2 <- as.data.frame(max.pca$x)  # Extract the PCA scores for each sample into a new dataframe
z2$Class <- z1$Class  # reattach the genus names of each datapoint
head(z2)  # Make sure it looks right

pcaplot <- ggplot(data = z2, mapping = aes(x = PC1, y = PC2, shape = Class, 
                                           col = Class, label = Class)) + geom_point()
pcaplot


t2 <- select(z,Class, SAMANDP1MD,SAMANDM1MD,SAMANDM2MD,SAMANDM3MD,SAMANDP1BL,SAMANDM1BL,SAMANDM2BL,SAMANDM3BL)
z3 <- na.omit(t2)
z4 <-  as.data.frame(mand.pca$x)
z4$Class <- z3$Class
head(z4)

pcaplot <- ggplot(data = z4, mapping = aes(x = PC1, y = PC2, shape = Class, 
                                           col = Class, label = Class)) + geom_point()
pcaplot











z[, 48]
t <- select(z,MAXCSF)
f <- na.omit(t)
f.pca <- prcomp(f, center = TRUE, scale. = TRUE)
f.pca

t <- select(z,Specimen,Class,Species,Source,MAXCSF)

t.pca <- prcomp(t[, 5:36], center = TRUE, scale. = TRUE)
head(t)
nrow(t)

z

t.pca
summary(t.pca)
biplot(t.pca$x[, c(1, 2)], t.pca$rotation[, c(1, 2)])


library(dplyr)













