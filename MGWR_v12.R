library(rgdal)
library(raster)
library(tidyverse)
library(broom)
library(rgeos)
library(GGally)
library(reshape2)

# Rural Urban Classification
setwd("/Users/Yi-Min/Rsession/ScenicOrNot/predictor variables/Rural Urban Classification")
ru <- readOGR("RUC_LSOA.shp")
load('/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness/MGWR_v6.RData')
proj4string(Abs)
proj4string(Nat)
proj4string(Rem)
proj4string(Rug)
proj4string(LCM)
proj4string(sc)

setwd("/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness")
  sc.shp <- readOGR(dsn = "/Users/Yi-Min/Python/MGWR/sc_mgwr_20190811.shp", stringsAsFactors = FALSE)
 mgwr_tv <- read.csv("/Users/Yi-Min/Python/MGWR/mgwr_tvals_20190811.csv", )
  gwr_tv <- read.csv("/Users/Yi-Min/Python/MGWR/gwr_tvals_20190811.csv", )
 wal.shp <- readOGR(dsn = "./Wales boundary/wales_ol_2001.shp")
  np.shp <- readOGR(dsn = "./NationalParks/NRW_NATIONAL_PARKPolygon.shp")
aonb.shp <- readOGR(dsn = "./AreaofOutstandingNaturalBeauty/NRW_AONBPolygon.shp")
nlca.shp <- readOGR(dsn = "./NationalLandscapeCharacterAreas/NRW_LANDSCAPE_CHARACTER_AREASPolygon.shp")
setwd("")

OSGB.grd <- readOGR("/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness/OSGB_Grids-master/Shapefile/OSGB_Grid_5km.shp")
asp.area <- readOGR(dsn = "./LandmapVisualSensory/NRW_LandMap_Visual_SensoryPolygon.shp")
	 asp <- asp.area[,c(6,63,8:11,21:23)]
	 
asp@data[which(asp$VS_46 == "Outstanding"),"SQ"] <- 5
       asp@data[which(asp$VS_46 == "High"),"SQ"] <- 4
   asp@data[which(asp$VS_46 == "Moderate"),"SQ"] <- 3
        asp@data[which(asp$VS_46 == "Low"),"SQ"] <- 2
 asp@data[which(asp$VS_46 == "Unassessed"),"SQ"] <- 1
 
require(tmap)
require(tmaptools)
png(filename = "SQ.png", w = 5*2, h = 5*2, units = "in", res = 300)
tm_shape(asp) + tm_fill(col = "SQ", title = "Secnic Quality") +
  tm_layout(legend.bg.color = "white")
dev.off()


names(t_value) <- c("Int", "Abs", "Nat", "Rem", "Rug", "LCM1", "LCM2", "LCM3", 
					"LCM4", "LCM5", "LCM6", "LCM7", "LCM8", "LCM9", "LCM10")

proj4string(sc.shp)
proj4string(wal.shp)
proj4string(np.shp)
proj4string(aonb.shp)

bng   <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000
	      +ellps=airy +datum=OSGB36 +units=m +no_defs"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"	   
  
     sc.shp <- spTransform(sc.shp, bng)
    wal.shp <- spTransform(wal.shp,bng)
     np.shp <- spTransform(np.shp, bng)
   aonb.shp <- spTransform(aonb.shp,bng)
   nlca.shp <- spTransform(nlca.shp, bng)
   aonb.shp <- aonb.shp[-3, ]
neither.shp <- gDifference(wal.shp, union(aonb.shp, np.shp))
 

#wal.shp <- spTransform(wal.shp, bng)
wal.shp <- rgeos::gSimplify(wal.shp, 5)

   sc.aonb <- sc.shp[aonb.shp, ]
     sc.np <- sc.shp[np.shp, ]

# add "Designation" column to sc.shp
 sc.shp$DSG <- "NEITHER"
sc.aonb$DSG <- "AONB"
  sc.np$DSG <- "NP"

sc.shp[sc.aonb$OBJECTID, "DSG"] <- "AONB"
  sc.shp[sc.np$OBJECTID, "DSG"] <- "NP"

# add "National Landscape Character Area (NLCA)" coloumn to sc.shp
sc.shp$NLCA <- as.character(over(sc.shp, nlca.shp[,"ENAME"])$"ENAME")

  sc.df <- as.data.frame(sc, region = "OBJECTID")
 wal.df <- tidy(wal.shp)
  np.df <- tidy(np.shp)
aonb.df <- tidy(aonb.shp)
nlca.df <- tidy(nlca.shp)


### SPATIAL AUTOCORRELATION OF SCENIC RATINGS
library(spdep)

sc.nb <- knn2nb(knearneigh(coordinates(sc.shp), k = 8))
sc.lw <- nb2listw(sc.nb)
# sc.listw <- nb2listw(sc.nb, style = "W", zero.policy = TRUE)
sc.lw
sc.shp$Sce.lagged.means <- lag.listw(sc.lw, sc.shp$Sce_mean)
tm_shape(sc.shp) + tm_dots(col = 'Sce.lagged.means') +
  tm_layout(legend.bg.color = 'white')
  
## Moran scatterplot
with(data.frame(sc.shp), {
	plot(Sce_mean, Sce.lagged.means, cex = 0.2, asp = 1, xlim = range(Sce_mean), ylim = range(Sce_mean))
	abline(a = 0, b = 1)
	abline(v = mean(Sce_mean), lty = 2)
	abline(h = mean(Sce.lagged.means), lty = 2)
})
moran.plot(sc.shp$Sce_mean, sc.lw, cex = 0.2) 
# Moran's I test
moran.test(sc.shp$Sce_mean, sc.lw, randomisation = TRUE)  # Moran's I test under randomisation
moran.test(sc.shp$Sce_mean, sc.lw, randomisation = FALSE) # Moran's I test under normality
###

### Figure XXX Kendall Correlations

corr.m <- cor(sc.df[,c(4,20:23,10:19)])
corr.k <- cor(sc.df[,c(4,20:23,10:19)], method = "kendall", use = "pairwise")

# original panel plot
# ggpairs(wf, aes(alpha = 0.4),
#          upper = list(continuous = wrap('cor', size = 6, colour = "black")),
#          lower = list(continuous = wrap('smooth',alpha = 0.3, cex=0.2   ))) +
#          theme(axis.line=element_blank(),
#          	axis.text=element_blank(),
#          	axis.ticks=element_blank())

# functions change the layout modified from https://github.com/ggobi/ggally/issues/139
# for lower panel of plots
my_custom_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = I("blue"), alpha = 0.3, cex = 0.1) + 
    geom_smooth(method = "lm", lwd = 0.5, color = I("red3"), ...)
}

#my_custom_smooth(iris, aes(Sepal.Length, Sepal.Width))

# for upper panel plot
my_custom_cor <- function(data, mapping, color = I("black"), sizeRange = c(1.5, 3), ...) {
  # get the x and y data to use the other code
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  ct <- cor.test(x,y, method = "spearman")
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " "))
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  # since we can't print it to get the strsize, just use the max size range
  cex <- max(sizeRange)
  # helper function to calculate a useable size
  percent_of_range <- function(percent, range) {
    percent * diff(range) + min(range, na.rm = TRUE)}

  # plot the cor value
  ggally_text(
    label = as.character(rt), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = I(percent_of_range(cex * abs(r), sizeRange)),
    color = color,
    ...) + 
    # add the sig stars
    geom_text(
      aes_string(
        x = 0.8,
        y = 0.8),
      label = sig, 
      size = I(cex),
      color = color,
      ...) + 
    # remove all the background stuff and wrap it with a dashed line
    theme_classic() + 
    theme(
      panel.background = element_rect(
        color = "grey50", 
        linetype = "longdash"), 
      axis.line = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_blank())
}

# my_custom_cor(iris, aes(Sepal.Length, Sepal.Width))

names(sc)
setwd("/Users/Yi-Min/Python/MGWR/")
png(filename = "SpearmanCorrelation_sqrt.png", w = 24/3, h = 24/3, units = "in", res = 300)
ggpairs(sc.sqrt@data[,c(4,20:23,10:19)], aes(alpha = 0.4),
		  upper = list(continuous = my_custom_smooth),
		  lower = list(continuous = my_custom_cor)) +
		  theme(axis.line = element_blank(),
		  		axis.text = element_blank(),
		  		axis.ticks = element_blank())
dev.off()

### OLS regression
reg.mod <- as.formula(Sce ~ Abs + Nat + Rug + Rem +
							LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + 
						    LCM6 + LCM7 + LCM8 + LCM9 + LCM10)
ols.m <- lm(reg.mod, data = sc@data)
summary(ols.m)

## Box-cox transformation
ols <- lm(Sce ~ LCM1, data = sc@data)
summary(ols)

# Abs linear test
par(mfrow = c(1,2))
plot(Sce ~ Abs, data = sc@data, col = "dodgerblue", pch = 20, cex = 0.5)
Sce_Abs = lm(Sce ~ Abs, data = sc@data)
abline(Sce_Abs, col = "darkorange", lwd = 2)
plot(fitted(Sce_Abs), resid(Sce_Abs), col = "dodgerblue",
	 pch = 20, cex = 0.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

# Nat linear test
par(mfrow = c(1,2))
plot(Sce ~ Nat, data = sc@data, col = "dodgerblue", pch = 20, cex = 0.5)
Sce_Nat = lm(Sce ~ Nat, data = sc@data)
abline(Sce_Nat, col = "darkorange", lwd = 2)
plot(fitted(Sce_Nat), resid(Sce_Nat), col = "dodgerblue",
	 pch = 20, cex = 0.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

# Rem linear test
par(mfrow = c(1,2))
plot(Sce ~ Rem, data = sc@data, col = "dodgerblue", pch = 20, cex = 0.5)
Sce_Rem = lm(Sce ~ Rem, data = sc@data)
abline(Sce_Rem, col = "darkorange", lwd = 2)
plot(fitted(Sce_Rem), resid(Sce_Rem), col = "dodgerblue",
	 pch = 20, cex = 0.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

# Rug linear test
par(mfrow = c(1,2))
plot(Sce ~ Rug, data = sc@data, col = "dodgerblue", pch = 20, cex = 0.5)
Sce_Rug = lm(Sce ~ Rug, data = sc@data)
abline(Sce_Rug, col = "darkorange", lwd = 2)
plot(fitted(Sce_Rug), resid(Sce_Rug), col = "dodgerblue",
	 pch = 20, cex = 0.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

# LCM linear test
par(mfrow = c(1,2))
plot(Sce ~ LCM1, data = sc@data, col = "dodgerblue", pch = 20, cex = 0.5)
Sce_LCM1 = lm(Sce ~ LCM1, data = sc@data)
abline(Sce_LCM1, col = "darkorange", lwd = 2)
plot(fitted(Sce_LCM1), resid(Sce_LCM1), col = "dodgerblue",
	 pch = 20, cex = 0.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
MASS::boxcox(Sce_LCM1, plotit = TRUE)
MASS::boxcox(Sce_LCM1, plotit = TRUE, lambda = seq(0.84, 0.94, by = 0.05))



plot(fitted(ols.m), resid(ols.m), col = "dodgerblue",
	 pch = 20, cex = 0.2, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

summary(ols.sqrt)
plot(ols.sqrt)
plot(fitted(ols.sqrt), resid(ols.sqrt), col = "dodgerblue",
	 pch = 20, cex = 0.2, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
MASS::boxcox(ols.sqrt, plotit = TRUE)
MASS::boxcox(ols.m, plotit = TRUE, lambda = seq(0.86, 0.92, by = 0.01))

lmtest::bptest(ols.sqrt)           # Breusch-Pagan normaility test
lmtest::bptest(ols.col)
lmtest::bptest(ols.mcol)
lmtest::bptest(ols.m)
lmtest::bptest(ols.log)
lmtest::shapiro.test(ols.sqrt$residuals) # Shapiro-Wilk normality test
qqnorm(ols.sqrt$resid)
qqline(ols.sqrt$resid)
hist(ols.sqrt$resid, freq = F)

> amod = lm(Murder~Population+Illiteracy+Life.Exp+Frost+Area, data=states)
> shapiro.test(amod$resid)
Shapiro-Wilk normality test
data: amod$resid
W = 0.986, p-value = 0.8116
> par(mfrow=c(1,2))
> qqnorm(amod$resid)
> qqline(amod$resid)
> hist(amod$resid,freq=F)
> xseq=seq(-5,5,length=200)
> lines(xseq,dnorm(xseq,sd=summary(amod)$sigma))



plot(ols.log)
plot(fitted(ols.log), resid(ols.log), col = "dodgerblue",
	 pch = 20, cex = 0.2, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)



tab1a <- round(coef(summary(ols.m)), 3)
tab1b <- round(summary(sc.car)$Coef, 3)
index <- match(rownames(tab1b), rownames(tab1a))
tab1 <- matrix(NA, nrow = nrow(tab1a), ncol = ncol(tab1a)*2)
tab1[, 1:4] <- tab1a
tab1[index, 5:8] <- tab1b
tab1 <- data.frame(tab1)
rownames(tab1) <- rownames(tab1a)
colnames(tab1) <- append(colnames(tab1a), colnames(tab1b))
require(knitr)
kable(tab1)
setwd("/Users/Yi-Min/Python/MGWR")
write.csv(tab1, "Tab1-1.csv")

## Variance Infaltion factor
car::vif(ols.sqrt)

reg.mod <- as.formula(Sce ~ Abs + Nat + Rug + Rem +
							LCM1 + LCM2 + LCM3 + LCM5 + 
						    LCM6 + LCM7 + LCM8 + LCM9 + LCM10)
ols.col <- lm(reg.mod, data = sc@data)
car::vif(ols.col)
summary(ols.col)
summary(ols.m)
summary(ols.sqrt)

ols.coll.diag <- olsrr::ols_coll_diag(ols.sqrt)

## Condition Number
X <- as.matrix(cbind(1, sc.sqrt@data[, c(20:23,10:12,14:19)]))
BKWcn <- function(X) {
	p <- dim(X)[2]
	Xscale <- sweep(X, 2, sqrt(colSums(X^2)), "/")
	Xsvd <- svd(Xscale)$d
	Xsvd[1] / Xsvd[p]
	} 
BKWcn(X)

require(GWmodel)
test.CN <- function(model, data) {
	dMat <- gw.dist(dp.locat=coordinates(data), focus=0, p=2, theta=0)
	lcrmx.bw <- bw.gwr.lcr(model, data = data, kernel = "gaussian", adaptive = FALSE, dMat = dMat)
	print(model)
	print(lcrmx.bw)
	lcrmx <- gwr.lcr(model, data = data, bw = lcrmx.bw, kernel = "gaussian", adaptive = FALSE, dMat = dMat)
	print(summary(lcrmx$SDF$Local_CN))
	lcrmx$SDF$Local_CN
	}
data <- sc.sqrt
model <- as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10)
AllD <- test.CN(model, data)
lcrmx.bw <- bw.gwr.lcr(model, data = data, kernel = "gaussian", adaptive = FALSE)
lcrmx <- gwr.lcr(model, data = data, bw = lcrmx.bw, kernel = "gaussian", adaptive = FALSE)



## Moran's I test for Residuals Spatial Autocorrelation
sc.moran <- lm.morantest(ols.m, sc.lw)
print(sc.moran)

### RESIDUAL PLOT
require(GISTools)
sc.resid = rstandard(ols.m)
resid.shades = shading(c(-2,2),c("red","grey","blue"))
cols = resid.shades$cols[1 + findInterval(sc.resid, resid.shades$breaks)]
quartz()
plot(ols.m, col = cols)
abline(lm(reg.mod), col='red', lwd = 2, lty = 2)

## residual map
png(filename = "residual.png", w = 5, h = 5, units = "in", res = 300)
par(mar=c(0,0,0,0)) 
choropleth(sc, sc.resid, resid.shades, pch = 20, cex = 0.2, )
plot(Wales, lwd = 0.5, color = 'lightgrey', add = T)
dev.off()

# Global Moran's I for OLS regression residuals
require(spdep) 
sc.nb <- knn2nb(knearneigh(coordinates(sc), k = 8))
sc.lw <- nb2listw(sc.nb)
sc.moran <- lm.morantest(ols.sqrt, sc.lw)
print(sc.moran)

# CAR model
sc.car <- spautolm(reg.mod, data = sc, listw = sc.lw, family = "CAR")
summary(sc.car, Nagelkerke = T)

##
library(GWmodel)
dMat <- gw.dist(dp.locat=coordinates(sc), focus=0, p=2, theta=0)

## Bandwidth selection
bw <- bw.gwr(reg.mod, data = sc, 
  kernel = "bisquare", adaptive = F, approach = "CV", dMat = dMat) 

bw2 <- bw.gwr(reg.mod, data = sc, 
  kernel = "gaussian", adaptive = F, approach = "CV", dMat = dMat) 


## Fitting a standard GWR
gwr.m.bi <- gwr.basic(reg.mod, data = sc, bw = bw,
  kernel = "bisquare", adaptive = F, dMat = dMat)
  
gwr.m.ga <- gwr.basic(reg.mod, data = sc, bw = bw2,
  kernel = "gaussian", adaptive = F, dMat = dMat)

# coefficient table
tab2a <- round(t(apply(gwr.m.bi$SDF@data[,1:15], 2, function(x) summary(x)[c(2,3,5)])), 3)
tab2b <- round(t(apply(gwr.m.ga$SDF@data[,1:15], 2, function(x) summary(x)[c(2,3,5)])), 3)
tab2 <- data.frame(tab2a, round(tab2a[,3]-tab2a[,1], 3), tab2b, round(tab2b[,3]-tab2b[,1], 3))
colnames(tab2) <- c("Bisquare 1st Qu.", "Bisquare Median", "Bisquare 3rd Qu.", "Bisquare IQR", "Gaussian 1st Qu.", "Gaussian Median", "Gaussian 3rd Qu.", "Guassian IQR" )
kable(tab2)
setwd("/Users/Yi-Min/Python/MGWR")
write.csv(tab2b, "Tab2.csv")
  
gwr.m.df <- as.data.frame(gwr.m.bi$SDF, region = "OBJECTID")
gwr.m.df <- as.data.frame(gwr.m.ga$SDF, region = "OBJECTID")

## GWR using Bisquare kernel
summary(gwr.m.df[, 2:5])
p1 <- ggplot.func(data.i = gwr.m.df, i = 2, type = "seq", tab = "GWR") 
p2 <- ggplot.func(data.i = gwr.m.df, i = 3, type = "seq", tab = "GWR") 
p3 <- ggplot.func(data.i = gwr.m.df, i = 4, type = "div", tab = "GWR") 
p4 <- ggplot.func(data.i = gwr.m.df, i = 5, type = "seq", tab = "GWR")
summary(gwr.m.df[, 6:15])
p5 <- ggplot.func(data.i = gwr.m.df, i = 6, type = "div", tab = "GWR") 
p6 <- ggplot.func(data.i = gwr.m.df, i = 7, type = "div", tab = "GWR") 
p7 <- ggplot.func(data.i = gwr.m.df, i = 8, type = "div", tab = "GWR") 
p8 <- ggplot.func(data.i = gwr.m.df, i = 9, type = "div", tab = "GWR") 
p9 <- ggplot.func(data.i = gwr.m.df, i = 10, type = "div", tab = "GWR") 
p10 <- ggplot.func(data.i = gwr.m.df, i = 11, type = "div", tab = "GWR") 
p11 <- ggplot.func(data.i = gwr.m.df, i = 12, type = "div", tab = "GWR") 
p12 <- ggplot.func(data.i = gwr.m.df, i = 13, type = "div", tab = "GWR")
p13 <- ggplot.func(data.i = gwr.m.df, i = 14, type = "div", tab = "GWR")
p14 <- ggplot.func(data.i = gwr.m.df, i = 15, type = "div", tab = "GWR")

## GWR using Gaussian kernel
summary(gwr.m.df[, 12:15])
g1 <- ggplot.func(data.i = gwr.m.df, i = 12, type = "seq", tab = "GWR") 
g2 <- ggplot.func(data.i = gwr.m.df, i = 13, type = "div", tab = "GWR") 
g3 <- ggplot.func(data.i = gwr.m.df, i = 14, type = "div", tab = "GWR") 
g4 <- ggplot.func(data.i = gwr.m.df, i = 15, type = "div", tab = "GWR")
summary(gwr.m.df[, 2:11])
g5 <- ggplot.func(data.i = gwr.m.df, i = 2, type = "div", tab = "GWR") 
g6 <- ggplot.func(data.i = gwr.m.df, i = 3, type = "div", tab = "GWR") 
g7 <- ggplot.func(data.i = gwr.m.df, i = 4, type = "div", tab = "GWR") 
g8 <- ggplot.func(data.i = gwr.m.df, i = 5, type = "div", tab = "GWR") 
g9 <- ggplot.func(data.i = gwr.m.df, i = 6, type = "div", tab = "GWR") 
g10 <- ggplot.func(data.i = gwr.m.df, i = 7, type = "div", tab = "GWR") 
g11 <- ggplot.func(data.i = gwr.m.df, i = 8, type = "div", tab = "GWR") 
g12 <- ggplot.func(data.i = gwr.m.df, i = 9, type = "div", tab = "GWR")
g13 <- ggplot.func(data.i = gwr.m.df, i = 10, type = "div", tab = "GWR")
g14 <- ggplot.func(data.i = gwr.m.df, i = 11, type = "seq", tab = "GWR")

png(filename = ".png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(p1, p5, p9, p13, p2, p6, p10, p14, p3, p7, p11, p4, p8, p12), cols = 4)
dev.off()

png(filename = "GWR_1.png", w = 10*2, h = 6, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(g1, g2, g3, g4), cols = 4)
dev.off()

png(filename = "GWR_2.png", w = 10*2, h = 6, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(g5, g6, g7, g8, g9), cols = 5)
dev.off()

png(filename = "GWR_3.png", w = 10*2, h = 6, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(g10, g11, g12, g13, g14), cols = 5)
dev.off()

### Variable selection
test.CN <- function(model, data, dMat) {
	lcrmx.bw <- bw.gwr.lcr(model, data = data, kernel = "gaussian", adaptive = FALSE, dMat = dMat)
	print(model)
	print(lcrmx.bw)
	lcrmx <- gwr.lcr(model, data = data, bw = lcrmx.bw, kernel = "gaussian", adaptive = FALSE, dMat = dMat)
	print(summary(lcrmx$SDF$Local_CN))
	lcrmx$SDF$Local_CN
	}
m1.CN <- test.CN(model = as.formula(Sce ~ Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, dMat = dMat)

m2.CN <- test.CN(model = as.formula(Sce ~ Abs + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, dMat = dMat)

m3.CN <- test.CN(model = as.formula(Sce ~ Abs + Nat + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, dMat = dMat)

m4.CN <- test.CN(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, dMat = dMat)

m5.CN <- test.CN(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, dMat = dMat)

m6.CN <- test.CN(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, dMat = dMat)

m7.CN <- test.CN(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM8 + LCM9 + LCM10), data = sc, dMat = dMat)

m8.CN <- test.CN(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM10), data = sc, dMat = dMat)

model <- as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM9 + LCM10)

bw1 <- bw.gwr.lcr(as.formula(Sce ~ Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, kernel = "gaussian", adaptive = FALSE, dMat = dMat)

m2 <- bw.gwr.lcr(model = as.formula(Sce ~ Abs + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, kernel = "gaussian", adaptive = FALSE, dMat = dMat)

m3 <- bw.gwr.lcr(model = as.formula(Sce ~ Abs + Nat + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, kernel = "gaussian", adaptive = FALSE, dMat = dMat)

m4 <- bw.gwr.lcr(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, kernel = "gaussian", adaptive = FALSE, dMat = dMat)

m5 <- bw.gwr.lcr(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM5 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, kernel = "gaussian", adaptive = FALSE, dMat = dMat)

m6 <- bw.gwr.lcr(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM6 + LCM7 + LCM8 + LCM9 + LCM10), data = sc, kernel = "gaussian", adaptive = FALSE, dMat = dMat)

m7 <- bw.gwr.lcr(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM8 + LCM9 + LCM10), data = sc, kernel = "gaussian", adaptive = FALSE, dMat = dMat)

m8 <- bw.gwr.lcr(model = as.formula(Sce ~ Abs + Nat + Rem + Rug + LCM1 + LCM2 + LCM3 + LCM4 + LCM5 + LCM6 + LCM7 + LCM8 + LCM10), data = sc, kernel = "gaussian", adaptive = FALSE, dMat = dMat)




## Local collinearity dagnostics for basic GW
diagno.ga <- gwr.collin.diagno(reg.mod, data = sc, bw = bw2,
  							kernel = "gaussian", adaptive = F, p = 2, theta = 0, longlat = F, dMat = dMat)
diagno.bi <-   							
# plot the local condition numbers
sc.df$local_CN <- diagno$SDF$local_CN

png(filename = "F0b.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
ggplot(wal.df) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
	     	   colour="grey", fill="grey") +
  coord_equal() + 
  geom_point(data = sc.df, aes(x = coords.x1, y = coords.x2, 
	    		 colour = local_CN), size = 0.5) +
		 	 labs(x = NULL, y = NULL, title = 'Local condition numbers from basic GW regression') +
  theme(axis.line = element_blank(),
		axis.text = element_blank(),
	 	axis.ticks = element_blank(),
	 	legend.position = "bottom",
	 	legend.direction = "horizontal",
	 	panel.grid.major = element_blank(),
	 	panel.grid.minor = element_blank(),
	 	panel.border = element_blank(),
	    panel.background = element_blank()) +
  scale_colour_gradient2(low = ("#2171B5"), mid = "yellow", name = NULL,
				   		 high = ("#CB181D"), midpoint = 30, space = "Lab",
						 na.value = "white",
						 guide = guide_colourbar(title.position = "top",
							 					 title.hjust = 0.5,
							 					 title.vjust = 0.8,
							 					 barwidth = 10))
dev.off()
### FIGURE 1
# Exploratory Data Anaylsis 
# AONBs
  
sc.input <- sc.df[,c(4,20:23,10:19,56,57)]
# ggpairs
ggpairs(sc.input, aes(alpha = 0.4),
		 upper = list(continuous = wrap('cor', size = 6, colour = "black")),
		 lower = list(continuous = wrap('smooth', alpha = 0.3, cex = 0.2))) + 
		 theme(axis.line = element_blank(),
		 	   axis.text = element_blank(),
		 	   axis.ticks = element_blank())
		 	   
## FIGURE 1a: boxplot for the differetne designations
sc.m <- melt(sc.input[,-17], id.var = "DSG")
png(filename = "F1a.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
p <- ggplot(data = sc.m, mapping = aes(x = variable, y = value)) + 
     geom_boxplot(aes(fill = DSG))
p + facet_wrap( ~ variable, scales = "free", ncol = 5)
dev.off()   

## FIGURE 1b: boxplot of the scenic rating mean for each NLCA
sc.m <- melt(sc.input[,c(1,17)], id.var = "NLCA")
png(filename = "F1b.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
p <- ggplot(data = sc.m, mapping = aes(x = reorder(NLCA, value, FUN = median), y = value)) +
	 geom_boxplot(varwidth = TRUE, na.rm = TRUE) +
	 theme(
	 axis.ticks.length = unit(-0.05, "in"),
	 axis.text.x = element_text(colour = 'grey20', size = 10, angle = 90,
	 						    hjust = 0.5, vjust = 0.5, margin = margin(t = 0.3, unit = "cm")),
	 axis.ticks.x = element_blank(),
	 legend.position = "bottom")
p + facet_wrap( ~ variable, scales = "free", ncol = 1)	 
dev.off()

### FIGURE 2
## ggpair:



### FIGURE 3 and 4
## map wilderness components
	rug <- ggplot(wal.df) + 
		 	 geom_polygon(aes(x = long, y = lat, group = group), 
		 	 				  colour="grey", fill="grey") +
		 	 coord_equal() + 
		 	 geom_point(data = sc.df, aes(x = coords.x1, y = coords.x2, 
		 	 			colour = Rug), size = 0.5) +
		 	 		 	 labs(x = NULL, y = NULL, title = 'Rug') +
		 	 theme(axis.line = element_blank(),
		 	 	   axis.text = element_blank(),
		 	 	   axis.ticks = element_blank(),
		 	 	   legend.position = "bottom",
		 	 	   legend.direction = "horizontal",
		 	 	   panel.grid.major = element_blank(),
		 	 	   panel.grid.minor = element_blank(),
		 	 	   panel.border = element_blank(),
		 	 	   panel.background = element_blank()) +
		 	 scale_colour_distiller(type = "seq", palette = "GnBu", name = NULL, direction = 1,
								   guide = guide_colourbar(title.position = "top",
								   						   title.hjust = 0.5,
								   						   title.vjust = 0.8,
								   						   barwidth = 10))



## map the significant locations
mgwr_tv <- read.csv('mgwr_tvals_20190811.csv')
mgwr.df <- as.data.frame(sc.mgwr[26:40], region = "OBJECTID")
 gwr.df <- as.data.frame(sc.shp[41:55], region = "OBJECTID")
 
names(mgwr.df)[1:15] <- c("Int", "Abs", "Nat", "Rem", "Rug", 
					      "LCM1", "LCM2", "LCM3", "LCM4", "LCM5",
				          "LCM6", "LCM7", "LCM8", "LCM9", "LCM10")
 names(gwr.df)[1:15] <- c("Int", "Abs", "Nat", "Rem", "Rug", 
					      "LCM1", "LCM2", "LCM3", "LCM4", "LCM5",
				          "LCM6", "LCM7", "LCM8", "LCM9", "LCM10")

## plot function
ggplot.func <- function(data.i = mgwr.df, TV = t_value, i = 2, type = "seq", tab = "GWR") {
	tit = names(data.i)[i]
	val = data.i[,i]
	index <- TV[,i] > 1.96 |
			 TV[,i] < -1.96
	tit = paste0(tab, ": ", tit)
	p <- ggplot(wal.df) + 
		 	 geom_polygon(aes(x = long, y = lat, group = group), 
		 	 				  colour="grey", fill="grey") +
		 	 coord_equal() + 
		 	 geom_point(data = data.i, aes(x = coords.x1, y = coords.x2, 
		 	 			colour = val), size = 0.8) +
		 	 geom_point(data = data.i[index, ], aes(x = coords.x1, y = coords.x2), size = 0.08, alpha = 0.2) +
		 	 labs(x = NULL, y = NULL, title = tit) +
		 	 theme(axis.line = element_blank(),
		 	 	   axis.text = element_blank(),
		 	 	   axis.ticks = element_blank(),
		 	 	   legend.position = "bottom",
		 	 	   legend.direction = "horizontal",
		 	 	   panel.grid.major = element_blank(),
		 	 	   panel.grid.minor = element_blank(),
		 	 	   panel.border = element_blank(),
		 	 	   panel.background = element_blank())
		 	 	   
	if (type == "div") {
		p <- p + scale_colour_gradient2(low = ("#CB181D"), mid = "white", name = NULL,
								 high = ("#2171B5"), midpoint = 0, space = "Lab",
								 na.value = "white",
								 guide = guide_colourbar(title.position = "top",
								 							title.hjust = 0.5,
								 							title.vjust = 0.8,
								 							barwidth = 10))
	}
	if (type == "seq") {
		p <- p + scale_colour_distiller(type = "seq", palette = "GnBu", name = NULL, direction = 1,
								   guide = guide_colourbar(title.position = "top",
								   						   title.hjust = 0.5,
								   						   title.vjust = 0.8,
								   						   barwidth = 10))
	}
	p
}

summary(gwr.df[, 2:5])
p1 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 2, type = "seq", tab = "GWR")
p2 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 3, type = "div", tab = "GWR")
p3 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 4, type = "div", tab = "GWR")
p4 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 5, type = "div", tab = "GWR")
summary(mgwr.df[, 2:5])
p5 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 2, type = "seq", tab = "MGWR")
p6 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 3, type = "seq", tab = "MGWR")
p7 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 4, type = "div", tab = "MGWR")
p8 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 5, type = "seq", tab = "MGWR")

summary(gwr.df[, 6:15])
p9  <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 6, type = "div", tab = "GWR")
p10 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 7, type = "div", tab = "GWR")
p11 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 8, type = "div", tab = "GWR")
p12 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 9, type = "div", tab = "GWR")
p13 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 10, type = "div", tab = "GWR")
p14 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 11, type = "div", tab = "GWR")
p15 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 12, type = "div", tab = "GWR")
p16 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 13, type = "div", tab = "GWR")
p17 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 14, type = "div", tab = "GWR")
p18 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 15, type = "seq", tab = "GWR")
summary(mgwr.df[, 6:15])
p19 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 6, type = "seq", tab = "MGWR")
p20 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 7, type = "seq", tab = "MGWR")
p21 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 8, type = "seq", tab = "MGWR")
p22 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 9, type = "seq", tab = "MGWR")
p23 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 10, type = "seq", tab = "MGWR")
p24 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 11, type = "div", tab = "MGWR")
p25 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 12, type = "seq", tab = "MGWR")
p26 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 13, type = "seq", tab = "MGWR")
p27 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 14, type = "div", tab = "MGWR")
p28 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 15, type = "seq", tab = "MGWR")

## multiplot function
multiplot2 <- function(plot.list, file, cols=3, layout=NULL) {
	library(grid)
	numPlots = length(plot.list)
	if (is.null(layout)) {
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
						ncol = cols, nrow = ceiling(numPlots/cols))
	}
	if (numPlots==1) {
		print(plots[[1]])
	} else {
	  grid.newpage()
	  pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
	  for (i in 1:numPlots) {
	  	matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
	  	print(plot.list[[i]], vp = viewport(layout.pos.row = matchidx$row,
	  									layout.pos.col = matchidx$col))
	  }
	}
}

png(filename = "F3.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(p1, p5, p2, p6, p3, p7, p4, p8), cols = 4)
dev.off()

png(filename = "F4a.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(p9, p19, p10, p20, p11, p21, p12, p22, p13, p23), cols = 5)
dev.off()

png(filename = "F4b.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(p14, p24, p15, p25, p16, p26, p17, p27, p18, p28), cols = 5)
dev.off()

png(filename = "MGWR_1.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(p5, p6, p7, p8), cols = 4)
dev.off()

png(filename = "GWR_1.png", w = 10*2, h = 6, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(g1, g2, g3, g4), cols = 4)
dev.off()
