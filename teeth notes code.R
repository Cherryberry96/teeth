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
Data = TD[, 5:36]
head(Data)


CorDat = cor(Data)
CorDat

CorDat = cor(dat)
CorDat
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


# ------------------------------------------------------------------------------------------------------


f <- "~/Desktop/Data Replication Stuff/S4 Table restructured.xlsx"
library(readxl)
z <- read_excel(f, sheet = 1, col_names = TRUE)
head(z)
names(z)

library(ggplot2)
library(psych)
library(factoextra)
library(dplyr)



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


t <- select(z,Species,Class,Source,Specimen, SAMAXP1MD,SAMAXP2MD,SAMAXM1MD,SAMAXM2MD,SAMAXP1BL,SAMAXP2BL,SAMAXM1BL,SAMAXM2BL)
z1 <- na.omit(t)
z2 <- as.data.frame(max.pca$x)
z2$Species <- z1$Species
z2$Class <-z1$Class
z2$Source <-z1$Source
z2$Specimen <-z1$Specimen
head(z2)


Class <- z2$Class

g <- filter(z2, Class=="Homo sapiens")


pcaplot <- ggplot(data = g, mapping = aes(x = PC1, y = PC2, shape = Class, 
                                           col = Class, label = Class)) + geom_point(shape=4, color="grey")
pcaplot


L <- filter(z2, Specimen=="LB1R")

pcaplot <- ggplot(data = g, mapping = aes(x = PC1, y = PC2, shape = Class, 
                                          col = Class, label = Class)) + geom_point(shape=4, color="grey")


LPlot <- ggplot(data = L, mapping = aes(x = PC1, y = PC2, shape = Specimen, 
                                          col = Specimen, label = Specimen)) + geom_point(shape=76, color="red", size =3)
LPlot



l <- filter(z2, Specimen=="LB1L")
lplot <- ggplot(data = l, mapping = aes(x = PC1, y = PC2, shape = Specimen, 
                                        col = Specimen, label = Specimen)) + geom_point(data = l, shape=108, color="red", size =3)

lplot

S <- filter(z2, Specimen=="S4R+L")
Splot <- ggplot(data = S, mapping = aes(x = PC1, y = PC2, shape = Specimen, 
                                        col = Specimen, label = Source)) + geom_point(data = S, shape=83, color="green", size =3)

Splot

D <- filter(z2, Specimen=="D2700/2735R+L")
Dplot <- ggplot(data = D, mapping = aes(x = PC1, y = PC2, shape = Specimen, 
                                        col = Specimen, label = Specimen)) + geom_point(data = D, shape=68, color="orange", size =3)

Dplot

h <- filter(z2, Species=="H. habilis sensu lato")
hplot <- ggplot(data = h, mapping = aes(x = PC1, y = PC2, shape = Species, 
                                        col = Species, label = Species)) + geom_point(data = h, shape=104, color="blue", size =3)

hplot










g <- filter(z2, Class=="Homo sapiens")
L <- filter(z2, Specimen=="LB1R")
l <- filter(z2, Specimen=="LB1L")
S <- filter(z2, Specimen=="S4R+L")
D <- filter(z2, Specimen=="D2700/2735R+L")
h <- filter(z2, Species=="H. habilis sensu lato")

pcaplot <- ggplot(data = g, mapping = aes(x = PC1, y = PC2, shape = Class, 
                                          col = Class, label = Class)) + geom_point(shape=4, color="grey") + geom_point(data=L, shape=76, color="red", size =3) + geom_point(data = l, shape=108, color="red", size =3) + geom_point(data = S, shape=83, color="green", size =3) + geom_point(data = D, shape=68, color="orange", size =3) + geom_point(data = h, shape=104, color="blue", size =3)
pcaplot














#Homo sapiens subsample
P <- g[1:28,]
P




hs <- aggregate(data = g, PC1 ~ Species, FUN = "mean", na.rm = TRUE)
View(hs)

hs2 <- aggregate(data = g, PC2 ~ Species, FUN = "mean", na.rm = TRUE)


hs2$PC2 <- hs$PC1
hs2


means

hsplot <- ggplot(data = hs, mapping = aes(x = PC1, y = PC2, shape = Species, 
                                        col = Species, label = Species)) + geom_point(shape=104, color="blue", size =3)







t2 <- select(z,Species,Class,Source,SAMANDP1MD,SAMANDM1MD,SAMANDM2MD,SAMANDM3MD,SAMANDP1BL,SAMANDM1BL,SAMANDM2BL,SAMANDM3BL)
z3 <- na.omit(t2)
z4 <-  as.data.frame(mand.pca$x)
z4$Species <- z3$Species
z4$Class <- z3$Class
z4$Source <- z3$Source
head(z4)

pcaplot <- ggplot(data = z4, mapping = aes(x = PC1, y = PC2, shape = Species, 
                                           col = Species, label = Species)) + geom_point()
pcaplot




#MANN WHITNEY U TEST
#PC Score for early javanese H. erectus and H. habilis
w <- z2[3:12,]

wilcox.test(PC1 ~ Species, data=w)
wilcox.test(PC2 ~ Species, data=w)
wilcox.test(PC3 ~ Species, data=w)
pvalues <- c(0.1778, 0.04444, 0.8889)

p.adjust(pvalues, method = "bonferroni")



