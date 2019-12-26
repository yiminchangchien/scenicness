### Lex Comber July 2018
## for Yi-Min
## Aim for paper on FB gwr and scenicness
## RQ: how do different factors relate to scenicness and at what scales do they operate?

### Load data and libraries
### set up data.sp etc
library(raster)
library(rgdal)
library(rgeos)
library(tidyr)
library(spatstat)
library(maptools)
library(GWmodel)
library(MASS)
library(sp)
library(sf)
library(ggplot2)
library(GGally)
library(landscapemetrics)
library(reshape2)
library(raster)

##### 1
##### 1.1. load in Scenic-Or-Not dataset #####
bng <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
        +ellps=airy +datum=OSGB36 +units=m +no_defs"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

sc <- readr::read_tsv("http://scenicornot.datasciencelab.co.uk/votes.tsv") %>%
  as.data.frame %>%
  st_as_sf(coords=c("Lon","Lat"), crs=4326) %>%
  st_transform(crs=27700) %>%
  as("Spatial")


for (i in 1:(dim(sc)[1])) {
  sc$Median[[i]] <- median(as.numeric(strsplit(as.character(sc$Votes[[i]]), ";")[[1]]))
  sc$Mean[[i]] <- mean(as.numeric(strsplit(as.character(sc$Votes[[i]]), ";")[[1]]))
  sc$IQR[[i]] <- IQR(as.numeric(strsplit(as.character(sc$Votes[[i]]), ";")[[1]]))
  sc$Variance[[i]] <- var(as.numeric(strsplit(as.character(sc$Votes[[i]]), ";")[[1]]))
}


GB <- getData("GADM", country = "United Kingdom", level = 0) %>% 
  disaggregate %>%
  geometry

# 2. urban rural
#setwd("/Users/Yi-Min/R session/ScenicOrNot/predictor variables/Rural Urban Classification")
#ru <- readOGR("RUC_LSOA.shp")
#ru <- spTransform(ru, proj)
#ol <- over(sc, ru)
#table(ol$RUC11, ol$RUC11CD)
#u <- (ol$RUC11CD == "A1" | ol$RUC11CD == "B1" | ol$RUC11CD == "C1" | ol$RUC11CD == "C2") + 0
#summary(u)
#sc <- SpatialPointsDataFrame(sc, data = data.frame(sc, u), proj4string = proj)
#names(sc@data)
#sc <- sc[, -c(19:21)]
#names(sc)[19] <- "Urban"
### we are leaving this out
## there are problems with the GWR because in many places it all rural
## this messes with the local regression

# 3. wildness attributes
setwd("/Users/Yi-Min/R session/ScenicOrNot/predictor variables/Wilderness Dimensions")
ac <- readGDAL("access")
na <- readGDAL("naturalness")
re <- readGDAL("remoteness")
rg <- readGDAL("ruggedness")
save(list = c("ac", "na", "re", "rg"), file = "wilderness.RData")

# transform sc to wild data projection
setwd("/Users/Yi-Min/R session/ScenicOrNot/predictor variables/Wilderness Dimensions")
load("wilderness.RData")
proj2 <- CRS(proj4string(rg))
sc <- spTransform(sc, proj2)
# do overlays
ol1 <- over(sc, ac)
ol2 <- over(sc, na)
ol3 <- over(sc, re)
ol4 <- over(sc, rg)

# combine to df
df <- data.frame(ol1, ol2, ol3, ol4)
head(df)
names(df) <- c("access", "naturalness", "remoteness", "ruggedness")
# create spdf
sc <- SpatialPointsDataFrame(sc, data = data.frame(sc, df), proj4string = proj2)
sc <- spTransform(sc, proj)
names(sc)
sc <- sc[, -c(19:21)]

# get rid of NAs
index <- apply(sc@data[, 10:22], 1, function(x) any(is.na(x)))
sc <- sc[!index,]
dim(sc)
summary(sc)
names(sc)[19:22] <- c("Access", "Naturalness", "Remoteness", "Ruggedness")
names(sc)[8] <- "Scenicness"

rm(list = c("ac", "na", "re", "rg"))

setwd("/Users/Yi-Min/R session/ScenicOrNot")
save.image("gwr3_sc_clip.RData")

####### END PART 1: load and peprpare data 


####### PART 2: initial analysis

setwd("/Users/Yi-Min/R session/ScenicOrNot")
load("gwr3_sc_clip.RData")

# correlations and covariance of variables
names(sc)
setwd("/Users/Yi-Min/R session/ScenicOrNot/Figs/clip")
# with terrain variables
png(filename = 'cormat1.png', w = 5, h = 5, units = 'in', res = 300)
plot(sc@data[,c(8,19:22)], cex = 0.5, col = grey(0.145,alpha=0.5), upper.panel=panel.smooth)
dev.off()

# with lcm 
png(filename = 'cormat2.png', w = 10, h = 10, units = 'in', res = 300)
plot(sc@data[,c(8,10:18)], cex = 0.5, col = grey(0.145,alpha=0.5), upper.panel=panel.smooth)
dev.off()

# with gclus
# Catherine Hurley’s gclus package. This is similar to the data frame plotting, but provides tools to reorder the variables so that the most strongly correlated pairs are closer to the diagonal in the plots. It can also colour-code the plots to show whether the correlations are positive (red) negative (blue) or near to zero (yellow). For this plot we focus on the predictors again. This is shown below

# covariance
cov.mat1 <- cov(sc@data[,c(8,19:22)])
kable(cov.mat1, digit = 3, booktabs = T, format = 'markdown')
round(cov.mat1, 3)[1, -1]
#write.csv(round(cov.mat1, 3), 'cov.mat1.csv') 
#png(filename = 'covmat1_gclus.png', w = 5, h = 5, units = 'in', res = 300)
cpairs(sc@data[,c(8,19:22)],order.single(cov.mat1),dmat.color(cov.mat1))
#dev.off()

cov.mat2 <- cov(sc@data[,c(8,10:18)])
kable(cov.mat2, digit = 3, booktabs = T, format = 'markdown')
round(cov.mat2, 3)[1, -1]
#write.csv(round(cov.mat2, 3), 'cov.mat2.csv')
#png(filename = 'covmat2_glcus.png', w = 10, h = 10, units = 'in', res = 300)
cpairs(sc@data[,c(8,10:18)],order.single(cov.mat2),dmat.color(cov.mat2))
#dev.off()

# or corelation (better)
cor.mat1 <- cor(sc@data[,c(8,19:22)]) 
kable(cor.mat1, digit = 3, booktabs = T, format = 'markdown')
round(cor.mat, 3)[1,-1]
#write.csv(round(cor.mat1, 3), 'cor.mat1.csv')
#png(filename = 'cormat1_glus.png', w = 5, h = 5, units = 'in', res = 300)
cpairs(sc@data[,c(8,19:22)],order.single(cor.mat1),dmat.color(cor.mat1))
#dev.off()
# all postively correlated with SC as expected 

cor.mat2 <- cor(sc@data[,c(8,10:18)]) 
kable(round(cor.mat2, 3), booktabs = T, format = 'markdown')
round(cor.mat2, 3)[1,-1]
#write.csv(round(cor.mat2, 3), 'cor.mat2.csv')
#png(filename = 'cormat2_glus.png', w = 10, h = 10, units = 'in', res = 300)
cpairs(sc@data[,c(8,10:18)],order.single(cor.mat2),dmat.color(cor.mat2))
#dev.off()
# suggestion moderate negative correlations with SC to lcm 3, 4 and positive with 5, 6 as expected

## get some plots out, gets some table etc

####### END PART 2: initial analysis


####### PART 3: Regression - OLS and GWR
### Remember the aim of the GWR analysis:
# is to better understand relationship heterogentity
# between scenicness and other variables
# That is how these vary spatially / locally

setwd("/Users/Yi-Min/R session/ScenicOrNot")
load("sc_clip.RData")
load("sc.RData")

#sc$lc <- as.factor(apply(sc@data[,10:19],1, which.max))
#reg.mod <- as.formula(Scenicness ~ Ruggedness+Naturalness+Access+Remoteness+LCM1+LCM2+LCM3+LCM4+LCM5+LCM6+LCM7+LCM8+LCM9+Urban)
#reg.mod <- as.formula(Scenicness ~ Ruggedness+Naturalness+Access+Remoteness + lc)
#reg.mod <- as.formula(Scenicness ~ Ruggedness+Naturalness+Access+Remoteness + relevel(lc, ref = "10"))
reg.mod <- as.formula(Scenicness ~ Ruggedness+Naturalness+Access+Remoteness+LCM1+LCM2+LCM3+LCM4+LCM5+LCM6+LCM7+LCM8+LCM9)

# 1. OLS
ols.m <- lm(reg.mod, data = sc@data)
summary(ols.m)

round(coef(summary(ols.m)), 3)
# what this says is for example that each increase in LCM5 of 10% is associated with an increase of 13% in scenicness, each increase of 10% of LCM8 is associated with a 18.6% increase in scenicness, each increase in 10% of Urban is associated with a 3.7% decrease in scenicness ### should be replaced by the following interpretation

# what this says is for example that each increase in LCM5 of 10% is associated with an increase of 14% in scenicness, each increase of 10% of LCM8 is associated with a 20.0% increase in scenicness, each increase in 10% of LCM1 is associated with a 8.6% increase in scenicness (only data within the bound of the national park)

# examine some model selection
# this selects the most parsimonious model
# ie. the one with the least variables but the greatest explanatory power
#summary(stepAIC(ols.m, trace = 1))
summary(stepAIC(ols.m, trace = 0))
round(coef(summary(stepAIC(ols.m, trace = 0))), 3)
reg.mod2 <- as.formula(summary(stepAIC(ols.m, trace = 0))[1])
# we may come back to this if the GWR cannot resolve locally

# 2. Collinearity 


# 2. Standard GWR

# load distance matrix
# I have re-done the distance matrix 
dMat <- gw.dist(coordinates(sc), coordinates(sc)) # does not take too long! 
#setwd("/Users/Yi-Min/R session/ScenicOrNot")
#save(dMat, file = "dMat.RData")
dim(dMat)
dim(sc)

# 2.1 GWR
#bw <- bw.gwr(reg.mod, data = sc, 
#  kernel = "bisquare", adaptive = T, approach = "CV", dMat = dMat) 
#gwr.m <- gwr.basic(reg.mod, data = sc, bw = bw, 
#  kernel = "bisquare", adaptive = T, dMat = dMat)
#summary(gw.m$SDF)

#bw <- bw.gwr(reg.mod, data = sc,
#  kernel = "gaussian", adaptive = T, approach = "CV", dMat = dMat) # Question: why the gaussian kernel was chosen here?
#bw/nrow(sc)  # Question: how come to divide bandwith with the number of data row?

## notice that the bandwidth selection had some problems resolving:
# "waring: solve(): system seems singular; attempting approx solution"
# it may be appropriate to use a mixed model GWR (see section 3)

#gwr.m <- gwr.basic(reg.mod, data = sc, bw = bw,
#  kernel = "bisquare", adaptive = T, dMat = dMat)
# indeed this fails to resolve and model fails to fit

# 2.2 Parsimonious GWR model   
# So... first try some model selection to get rid of spurous variables

#summary(stepAIC(ols.m, trace = 1)) # Yi-Min: check the process of eliminating the spurous variables
#summary(stepAIC(ols.m, trace = 0))[1]
#reg.mod2 <- as.formula(summary(stepAIC(ols.m, trace = 0))[1])
#bw2 <- bw.gwr(reg.mod2, data = sc,
#  kernel = "bisquare", adaptive = T, approach = "CV", dMat = dMat) # Question: why the kernel here was "bisquare"?
#bw2/nrow(sc)
#gwr.m <- gwr.basic(reg.mod2, data = sc, bw = bw2, 
#  kernel = "bisquare", adaptive = T, dMat = dMat)
#again this fails to resolve and model fails to fit

# 2.3 Parsimonious GWR model plus small error term
# so lets return to a parimonious model and try to fit that in a GWR
# and get rid of the zeros with a small random error term
scj <- sc
round(head(scj@data[,10:18]),6)
scj@data[, 10:18] <- apply(scj@data[,10:18], 2, function(x) jitter(x)) # Question: what's the purpose of jitter?
bw2 <- bw.gwr(reg.mod2, data = scj,
   kernel = "bisquare", adaptive = F, approach = "CV", dMat = dMat) # Question: why the bisquare kernel and fixed bandwidth were chosen at this stage? 
bw2/max(dMat) # Question: I have no idea about the purpose of this division
gwr.m <- gwr.basic(reg.mod2, data = scj, bw = bw2,
   kernel = "bisquare", adaptive = F, dMat = dMat)
summary(gwr.m$SDF@data)

#reg.mod4 <- as.formula(summary(stepAIC(ols.m, trace = 0))[1]) 
# but this only gets rid of LCM7

# Next it might be useful to split the analyses
# we can argue that the wilness data are built on LCM data 
# meaning the variables are not independent
# this may be why a GWR fails to fit
#reg.mod2 <- as.formula(Scenicness ~ Ruggedness+Naturalness+Access+Remoteness)
#reg.mod3 <- as.formula(Scenicness ~ LCM1+LCM2+LCM3+LCM4+LCM5+LCM6+LCM7+LCM8+LCM9)

# 2.4 Flexible BW GWR
# so at this stage we have 2 options
# 1) change kernel to gaussian - this essentailly weights and includes all data
# 1) drop some variables
# 2) use a mixed GWR with the gwr.psdm function.
# This allows a low bw for some variables in order to allow them vary locally
# a high bw for other variables, essentially fixing them globally
# But it takes some time to run

# In this way, this supports a 'mixed' GWR approach
# because it fits a bandwidth to each variable it takes ages to run
# this took 10 hours on my computer for the case study area
gwr.fb <- gwr.psdm(reg.mod2, data = scj, kernel = "bisquare",
    adaptive = F, criterion = "dCVR", max.iterations = 50)
setwd("/Users/Yi-Min/R session/ScenicOrNot")
save.image("GWR_FB_20180820.RData")

# 2.5 Summary - story so far
# 1) examined correlations between scenicness and other variables (some strong +ve ones with terrain variables, moderate positive and 
     negative ones with LCM classes)
# 2) OLS regression indicated global trend
# 3) the most parsimonious model was identified using stepwise model selection. This suggested dropping LCM3 and LCM7 (LCM10 already 
     excluded). Parsimonious models seek to determine the model with the fewest variables but the greatest explanatory power.
# 4) there were some problems fitting a local model. This was due to the problems caused by local data subsets all having the same    
     value locally (0) in GWR 
#    - To overcome this a small error term was added to the LCM data
#      then a GWR model was fitted. This indicated a bandwidth of 26.424km.
#      However, interested here in the scales over which different factors are associated with sccenicness: some may tend towards the
#      global, others the local, perhaps suggesting the need for a mixed GWR.

####### END PART 3: Regression - OLS and GWR


####### PART 4: Results - OLS and GWR

setwd("/Users/Yi-Min/R session/ScenicOrNot")
load("GWR_FB.RData")
#load("gwr3_sc_clip.RData")
load("sc.RData")


# 0. choropleth map
library(broom)
setwd("/Users/Yi-Min/R session/ScenicOrNot/")
  sc.shp <- readOGR(dsn = "./ScenicOrNot dataset/ScenicOrNot_Wales.shp", stringsAsFactors = FALSE)  
 wal.shp <- readOGR(dsn = "./Wales boundary/wales_ol_2001.shp")
  np.shp <- readOGR(dsn = "./NationalParks/NRW_NATIONAL_PARKPolygon.shp")
aonb.shp <- readOGR(dsn = "./AreaOfOutstandingNaturalBeauty/NRW_AONBPolygon.shp")

bng   <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
          +ellps=airy +datum=OSGB36 +units=m +no_defs"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
          
  sc.shp <- spTransform(sc.shp, wgs84)
 wal.shp <- spTransform(wal.shp,wgs84)
  np.shp <- spTransform(np.shp, wgs84)
aonb.shp <- spTransform(aonb.shp, wgs84)
aonb.shp <- aonb.shp[wal.shp, ]
aonb.shp <- aonb.shp[-3, ]

  sc.df <- as.data.frame(sc.shp, region = "OBJECTID")
 wal.df <- tidy(wal.shp)
  np.df <- tidy(np.shp)
aonb.df <- tidy(aonb.shp)

# choropleth with tmap
gg <- ggplot() +
		 geom_polygon(data = wal.df, aes(x = long, y = lat, group = group),
		 			  fill = "gray92") +
 		 geom_point(data = sc.df, aes(x = Lon, y = Lat, colour = sc.df$median), size = 0.3) + 
 		 scale_colour_continuous(name = "Median of Scenic Rating", low = "Yellow", high = "Brown",
 		 					   space = "Lab", na.value = "grey50", breaks = c(1, 5.5, 10), 
 		 					   labels = c(1, 5.5, 10), limits = c(1, 10), 
 		 					   guide = guide_colourbar(title = "Median of Scenic Rating", 
 		 					                           title.position = "top",
 		 					                           title.hjust = 0.5,
 		 					                           title.vjust = 0.8,
 		 					                           barwidth = 10)) +
 		 geom_polygon(data = np.df, aes(x = long, y = lat, group = group),
   		 			  fill = "transparent", color = "green3") +
   		 geom_polygon(data = aonb.df, aes(x = long, y = lat, group = group),
   		 			  fill = "transparent", color = "dodgerblue3") +
 		 coord_map() +		  		 
 		 labs(x = NULL, y = NULL, caption = "Source: http://scenicornot.datasciencelab.co.uk/") +
 		 theme(plot.caption = element_text(face = "bold", family = "Arial", size = 10, 
 		                                   color = "gray", margin = margin(t = 10, r = 80))) +
         theme(axis.line = element_blank(),
  		 	   axis.text = element_blank(),
  		 	   axis.ticks = element_blank(),
  		 	   legend.position = "bottom",
  		 	   legend.direction = "horizontal",
  		 	   legend.title = element_text(face = "bold", family = "Helvetica", size = 15),
  		 	   legend.text = element_text(face = "bold", family = "Arial", size = 10),
  		       panel.grid.major = element_blank(),
  		 	   panel.grid.minor = element_blank(),
  		 	   panel.border = element_blank(),
  			   panel.background = element_blank()) 
  		 #labs(colour = "Median of Scenic Rating", position = "top") 
  		 #guides(colour = guide_legend(title.position = "top"), guide = "colourbar") 
plot(gg)
ggsave(file = "./Figs/Scenicness.png", plot = gg, width = 10, height = 8)
 
 
# 1. Initial results: Correlations
head(sc@data)
corr.m <- cor(sc@data[,c(8,20:23)])
rownames(corr.m) <- c("Sce", "Acc", "Nat", "Rem", "Rug")
colnames(corr.m) <- c("Sce", "Acc", "Nat", "Rem", "Rug")
co = melt(corr.m)
setwd("/Users/Yi-Min/R session/ScenicOrNot/Figs")
png(filename = "f1.png", w = 5, h = 5, units = "in", res = 300)
ggplot(co, aes(Var1, Var2)) +  # x and y axes => Var1 and Var2
    geom_tile(aes(fill = value)) +  # background colours are mapped according to the value column
    geom_text(aes(fill = co$value, label = round(co$value, 2))) +  # write the values
    coord_fixed() +
    xlab("") +
    ylab("") +
    labs(fill = "Corr. Coef.") +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_fill_gradientn(colours = brewer.pal(9, 'Blues'))
dev.off()

corr.m <- cor(sc@data[,c(20:23,8,10:18)])
head(corr.m)
rownames(corr.m) <- c("Sce", "Acc", "Nat", "Rem", "Rug", "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", "LCM6", "LCM7", "LCM8", "LCM9")
colnames(corr.m) <- c("Sce", "Acc", "Nat", "Rem", "Rug", "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", "LCM6", "LCM7", "LCM8", "LCM9")
co = melt(corr.m)
setwd("/Users/Yi-Min/R session/ScenicOrNot/Figs")
png(filename = "f0.png", w = 12, h = 12, units = "in", res = 300)
ggplot(co, aes(Var1, Var2, fill = value)) +  # x and y axes => Var1 and Var2
    geom_tile() +  # background colours are mapped according to the value colum
    geom_text(aes(fill = co$value, label = round(co$value, 2)), size = 3) +  # write the values
    coord_fixed() +
    xlab("") +
    ylab("") +
    labs(fill = "Corr. Coef.") +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 60, hjust =1)) + 
    #scale_fill_gradientn(colours = brewer.pal(9,'RdBu)) +
    #scale_fill_brewer(palette = "RdBu") +
    scale_fill_gradient2(low = "#B2182B",
                         mid = "white",
                         high = "#2166AC",
                         midpoint = 0)  # determine the colour
dev.off()

# 2. Regression results OLS - original and selected
tab1a <- round(coef(summary(ols.m)), 3)
tab1b <- round(coef(summary(stepAIC(ols.m, trace = 0))), 3)
index <- match(rownames(tab1b), rownames(tab1a))
tab1 <- matrix(NA, nrow = nrow(tab1a), ncol = ncol(tab1a)*2)
tab1[, 1:4] <- tab1a
tab1[index, 5:8] <- tab1b
tab1 <- data.frame(tab1)
rownames(tab1) <- rownames(tab1a)
colnames(tab1) <- append(colnames(tab1a), colnames(tab1a))
kable(tab1)
setwd("/Users/Yi-Min/R session/ScenicOrNot/Figs")
write.csv(tab1, "Tab1.csv")

# 3. GWR - Standard and Flexible bandwidth      
#tab2a <- round(t(apply(gwr.m$SDF@data[,1:13], 2, function(x) summary(x)[c(2,3,5)])), 3) # I have adjusted the number of rounding.
#bw.vec <- round(gwr.fb[[5]][nrow(gwr.fb[[5]]),]/1000, 1)
#bw.perc <-round(gwr.fb[[5]][nrow(gwr.fb[[5]]),]/ max(dMat)*100, 1)
#tab2b <- round(t(apply(gwr.fb$SDF@data[,1:13], 2, function(x) summary(x)[c(2,3,5)])), 3)
#tab2 <- data.frame(tab2a, bandwidths = bw.vec, percentage = bw.perc, tab2b, round(tab2a[,3]-tab2a[,1], 3), round(tab2b[,3]-tab2b[,1], 3))
#colnames(tab2) <- c("Standard 1st Qu.", "Standard Median", "Standard 3rd Qu.", "bandwidths", "percentage", "FB 1st Qu.", "FB Median", "FB 3rd Qu.", "Standard IQR", "FB IQR" )

tab2a <- round(t(apply(gwr.m$SDF@data[,1:13], 2, function(x) summary(x)[c(2,3,5)])), 3) # I have adjusted the number of rounding.
bw.vec <- round(gwr.fb[[5]][nrow(gwr.fb[[5]]),]/1000, 1)
bw.perc <-round(gwr.fb[[5]][nrow(gwr.fb[[5]]),]/ max(dMat)*100, 1)
tab2b <- round(t(apply(gwr.fb$SDF@data[,1:13], 2, function(x) summary(x)[c(2,3,5)])), 3)
tab2 <- data.frame(tab2a, round(tab2a[,3]-tab2a[,1], 3), bandwidths = bw.vec, percentage = bw.perc, tab2b, round(tab2b[,3]-tab2b[,1], 3))
colnames(tab2) <- c("Standard 1st Qu.", "Standard Median", "Standard 3rd Qu.", "Standard IQR", "bandwidths", "percentage",
                    "FB 1st Qu.", "FB Median", "FB 3rd Qu.", "FB IQR" )

kable(tab2)
setwd("/Users/Yi-Min/R session/ScenicOrNot/Figs")
write.csv(tab2, "Tab2.csv")
# here we can see that the optimal bandwidths for different factors vary from the very local (LCM1, LCM4, LCM5, LCM6, LCM8 Ruggedness), to the intermediate(LCM9) and the global (Naturalness, Remoteness, Access, LCM2)

# 4. Coefficent maps and significant locations (t values)
# load wales outline
setwd("/Users/Yi-Min/R session/ScenicOrNot/Wales boundary")
wal <- readOGR("wales_ol_2001.shp")
wal <- spTransform(wal, proj2)

#plot(clip)
plot(scj)
add.alpha(brewer.pal(5, "Greys"), 0.5)
#plot(sc, add = T, pch = 1, cex = 0.2, col = '#25252580')
#plot(scj, add = T, pch = 1, cex = 0.2, col = '#25252580')
plot(wal, add = T, border = 'red')

## Map all together
setwd("/Users/Yi-Min/R session/ScenicOrNot/Figs")
png(filename = "f3.png", w = 12, h = 12, units = "in", res = 300)
tm_shape(gwr.m$SDF) +
    #tm_dots(col = c("LCM1", "LCM2", "LCM4", "LCM5", "LCM6", "LCM8", "LCM9"),
    tm_dots(col = c("Ruggedness", "Naturalness", "Access", "Remoteness", 
                    "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", "LCM6", "LCM8", "LCM9"),
        size = 0.3, style = "kmeans", palette = "RdBu") +
    tm_format_NLD_wide() +
    tm_shape(wal) +
    tm_borders()
dev.off()

png(filename = "f4.png", w = 12, h = 12, units = "in", res = 300)
tm_shape(gwr.fb$SDF) +
    #tm_dots(col = c("LCM1", "LCM2", "LCM4", "LCM5", "LCM6", "LCM8", "LCM9"),
    tm_dots(col = c("Ruggedness", "Naturalness", "Access", "Remoteness", 
                    "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", "LCM6", "LCM8", "LCM9"),
        size = 0.3, style = "kmeans", palette = "RdBu") +
    tm_format_NLD_wide() +
    tm_shape(wal) +
    tm_borders()
dev.off()

png(filename = "f5.png", w = 12, h = 12, units = "in", res = 300)
tm_shape(scj@data) +
    #tm_dots(col = c("LCM1", "LCM2", "LCM4", "LCM5", "LCM6", "LCM8", "LCM9"),
    tm_dots(col = c("Ruggedness", "Naturalness", "Access", "Remoteness", 
                    "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", "LCM6", "LCM8", "LCM9"),
        size = 0.3, style = "kmeans", palette = "RdBu") +
    tm_format_NLD_wide() +
    tm_shape(wal) +
    tm_borders()
dev.off()

## now map the significant locations
setwd("/Users/Yi-Min/R session/ScenicOrNot/Figs/0122")
names(gwr.m$SDF)[c(2:5, 20:23, 33:36)] <- c("Rug", "Nat", "Acc", "Rem", "Rug_SE", "Nat_SE", "Acc_SE", "Rem_SE", "Rug_TV", "Nat_TV", "Acc_TV", "Rem_TV")
names(gwr.fb$SDF)[c(2:5, 17:20, 30:33)] <- c("Rug", "Nat", "Acc", "Rem", "Rug_SE", "Nat_SE", "Acc_SE", "Rem_SE", "Rug_TV", "Nat_TV", "Acc_TV", "Rem_TV")

vars <- names(scj)
vars <- vars[c(23, 21, 22, 20, 10:15, 17, 18)]
vars[1:4] <- c("Rug", "Nat", "Rem", "Acc")
i <- 3
var <- vars[i]

breaks.list <- list()
breaks.list[[1]] <- c(  -0.08,    0.46,    1.01,    1.55,   2.10,   2.64) # Ruggedness
breaks.list[[2]] <- c(  -0.18,    0.04,    0.26,    0.47,   0.69,   0.91) # Naturalness
breaks.list[[3]] <- c(-0.0014, -0.0010, -0.0006, -0.0001, 0.0003, 0.0007) # Remoteness
breaks.list[[4]] <- c(   0.02,    0.05,    0.09,    0.12,   0.16,   0.19) # Access
breaks.list[[5]] <- c(  -4.72,   -1.90,    0.92,    3.75,   6.57,   9.39) # LCM1
breaks.list[[6]] <- c(  -4.91,   -2.59,   -0.27,    2.06,   4.38,   6.70) # LCM2
breaks.list[[7]] <- c(  -2.17,   -0.17,    1.83,    3.84,   5.84,   7.84) # LCM3
breaks.list[[8]] <- c(  -0.48,    0.61,    1.71,    2.80,   3.90,   4.99) # LCM4
breaks.list[[9]] <- c( -21.57,   -5.66,   10.26,   26.17,  42.09,  58.00) # LCM5
breaks.list[[10]]<- c(  -3.22,   -1.46,    0.30,    2.07,   3.83,   5.59) # LCM6
breaks.list[[11]]<- c( -38.00,  -27.24,  -16.49,   -5.73,   5.02,  15.78) # LCM8
breaks.list[[12]]<- c( -54.61,  -30.18,   -5.75,   18.69,  43.12,  67.55) # LCM9


index <- gwr.m$SDF@data[, paste0(var, "_TV")] > 1.96 | gwr.m$SDF@data[, paste0(var,"_TV")] < -1.96
gwr.index <- gwr.m$SDF[index,]
p1 <- tm_shape(gwr.m$SDF) +
    	tm_dots(col = c(var),
       	size = 0.05, breaks = breaks.list[[i]], palette = "RdBu",
    	  legend.hist = F) +
    	tm_format_NLD_wide() +
    	tm_layout(frame = F, #title = "GWR",
   	    legend.hist.size = 0.5,
       	legend.outside = F) +
   		tm_shape(gwr.index) +
    	tm_dots(col = "#25252580", size = 0.01, shape = 1, alpha = 0.08) + # I adjust the size parameter
    	tm_shape(wal) +
    	tm_borders()
    	
## and compare with FB GWR
index <- gwr.fb$SDF@data[, paste0(var, "_TV")] > 1.96 | gwr.fb$SDF@data[, paste0(var, "_TV")] < -1.96
gwr.fb.index <- gwr.fb$SDF[index,]
p2 <- tm_shape(gwr.fb$SDF) +
   		tm_dots(col = c(var),
       		size = 0.05, breaks = breaks.list[[i]],
       		palette = "RdBu", legend.hist = F) +
    	tm_format_NLD_wide() +
    	tm_layout(frame = F, #title = "FBGWR",
        	    legend.hist.size = 0.5,
            	legend.outside = F) +
   		tm_shape(gwr.fb.index) +
    	tm_dots(col = "#25252580", size = 0.01, shape = 1, alpha = 0.08) +
    	tm_shape(wal) +
    	tm_borders()

	
## multiple plotting!
library(grid)
png(filename = paste0(var,".png"), w = 2.5, h = 5, units = "in", res = 300)
#grid.newpage()
#par(mar = c(1,1,1,1))
# set up the layout
pushViewport(viewport(layout = grid.layout(2,1)))
# plot using the print command
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1, height = 5))
print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1, height = 5))
dev.off()

setwd("/Users/Yi-Min/R session/ScenicOrNot")
save.image("GWR_FB.RData")
####### End PART 4: Results - OLS and GWR #######   

library(grid)
grid.newpage()
vp <- viewport(x = 0.5, y = 0.5, width = 0.5, height = 0.25, angle = 45)
pushViewport(vp)
grid.rect()
grid.show.viewport(viewport(x = 0.6, y = 0.6, w = unit(1, "inches"), h = unit(1, "inches")))

grid.newpage()
colvec <- c('red', 'green', 'blue')
xvec <- c(0.3, 0.5, 0.7)
for (i in 1 : 3) {
	vp <- viewport(x = xvec[i], y = 0.5, width = 0.4, height = 0.8, gp = gpar(col = colvec[i]))
	pushViewport(vp)
	grid.rect()
	upViewport()
} 








######################### discard the code below ########################

sc$lc <- which.max(sc@data[, 10:18]) # Question: why do this again and neglect the last

# One way to overcome this is
# ?gwr.lcr

# Function to find the global condition number (CN)
BKW_cn <- function (X) {
	p <- dim(X)[2]
	Xscale <- sweep(X, 2, sqrt(colSums(X^2)), "/")
	Xsvd <- svd(Xscale)$d
	cn <- Xsvd[1] / Xsvd[p]
	cn
}

X <- cbind(1, sc@data[,c(8, 10:22)])
head(X)
CN.global <- BKW_cn(X)
CN.global
## Question: suppose these snipets were copied from other applications
## Not run: 
# gwr.lcr function with a global bandwidth to check that the global CN is found
#gwr.lcr1 <- gwr.lcr(GenE12004~DiffAdd+LARent+SC1+Unempl+LowEduc+Age18_24
#+Age25_44+Age45_64, data=Dub.voter, bw=10000000000)
#summary(gwr.lcr1$SDF$Local_CN)

bw.lcr <- bw.gwr.lcr(reg.mod2, data = sc, kernel = "bisquare",
          lambda = 0, lambda.adjust = FALSE, cn.thresh = 30,
          adaptive = FALSE, p = 2, theta = 0, longlat = F, dMat = dMat)
# Question: how do I know the parameter settings (i.e. lambda, lambda.adjust, cn.thresh, p, theta)?

# 3. Flexible bandwidth GWR
setwd("/Users/Yi-Min/R session/ScenicOrNot")
load("gwr3_sc.RData")

reg.mod <- as.formula(Scenicness ~ Ruggedness+Naturalness+Access+Remoteness+LCM1+LCM2+LCM3+LCM4+LCM5+LCM6+LCM7+LCM8+LCM9)
reg.mod2 <- as.formula(summary(stepAIC(ols.m, trace = 0))[1])

#gwr.psdm
# did not resolve with reg.mod
#fbgwr.g <- gwr.psdm(reg.mod, data = sc, kernel = "guassian", adaptive = F, criterion = "dCVR") #Question: how gwr.psdm works?
#fbgwr.b <- gwr.psdm(reg.mod, data = sc, kernel = "bisquare", adaptive = F, criterion = "dCVR") #Question: what's the difference between "gaussian" and "bisquare" kernel

fbgwr.g <- gwr.psdm(reg.mod2, data = sc, kernel = "gaussian", adaptive = F, criterion = "dCVR") #Question: why the adaptive width is not adapted?
fbgwr.b <- gwr.psdm(reg.mod2, data = sc, kernel = "bisquare", adaptive = F, criterion = "dCVR")

summary(sc)
summary(gw.m$SDF)

### transform the data
par(mfrow = c(3,3))
par(mar = c(2,2,2,2))
for (i in 10:18) {
	hist(sc@data[,i], main = "", xlab = "")
	title(names(sc)[i])
}

par(mfrow = c(3,3))
par(mar = c(2,2,2,2))
for (i in 10:18) {
	hist(log((sc@data[,i]+0.001)*100), main = "", xlab = "")
	title(names(sc)[i])
}

scr <- sc
scr@data[,10:18] <- apply(scr@data[, 10:18], 1, function(x) log((sc@data[,i]+0.001))) # Question: What is the function for



# 1. OLS
ols.l <- lm(reg.mod, data = sc@data)
summary(ols.l)

hist(log((sc@data[,i]+0.001)*100), main = "", xlab = "")

#scr@data[,10:18] <- apply(scr@data[, 10:18], 1, function(x) rescale(as.vector(x)))
scr@data[,10:18] <- apply(scr@data[, 10:18], 1, function(x) log(100*as.vector(x)))

# There are some problems. The bandwidth was fitted but
# I think the collinearity in the data stopped the gwr working
# this is to do with the dimensionality of the data
       


ols.m2 <- lm(reg.mod2, data = sc@data)
summary(ols.m2)
ols.m3 <- lm(reg.mod3, data = sc@data)
summary(ols.m3)
  
# Now lets try and fit 2 GWR models 

bw2 <- bw.gwr(reg.mod2, data = sc, 
  kernel = "bisquare", adaptive = T, approach = "AIC", dMat = dMat) 
save(bw2, file = "bw2.Rdata")  
   
bw3 <- bw.gwr(reg.mod3, data = sc, 
  kernel = "bisquare", adaptive = T, approach = "AIC", dMat = dMat)  # not working out
save(list = c("bw2", "bw3"), file = "bw2_3.Rdata")
load("bw2_3.Rdata")

gwr.2 <- gwr.basic(reg.mod2, data = sc, bw = bw2, 
  kernel = "bisquare", adaptive = T, dMat = dMat)
gwr.3 <- gwr.basic(reg.mod3, data = sc, bw = bw3, 
  kernel = "bisquare", adaptive = T, dMat = dMat)

summary(gw.m$SDF)

# nlme (nonlinear mixed-effects models)

# * drop some variables 

# psdm (parameter-specific distance metrics)
# * mixed GWR psdm
# - low bw for let them vary
# - high bw fix them 




# Next we subset the data to examine local patches to see if we can get a handle on the likely bw

index <- sample(1:nrow(sc), 1)
# get nearest 1000 data points
ord <- order(dMat[index,])
index2 <- which(ord <= 1000)

bw <- bw.gwr(reg.mod, data = sc[index2,], kernel = "bisquare", 
    	adaptive = T, approach = "AIC", dMat = dMat[index2, index2])

index.matrix <- matrix(ncol = 100, nrow = 700)
bw.vec <- vector()
for (i in 1:100) {
	index.i <- sample(1:nrow(sc), 700)
	index.matrix[,i] <- index.i
	bw <- bw.gwr(reg.mod, data = sc[index.i,], kernel = "bisquare", 
    	adaptive = T, approach = "AIC", dMat = dMat[index.i, index.i])
    #rm(.Random.seed)
    bw.vec <- append(bw.vec, bw)
    cat(i, "\t")
}


# Next we use sample() to reduce the dimensionality of the data 
n = 800
index <- sample(1:nrow(sc), n)
bw <- bw.gwr(reg.mod, data = sc[index,], kernel = "bisquare", 
    	adaptive = T, approach = "AIC", dMat = dMat[index, index])
bw    	
# regardless of the value of n (I tried between 500 and 2000)


# run this for 100 iterations and we should be able to determine what proportion 
# of the data points should be included in a flexible bandwidth


index.matrix <- matrix(ncol = 100, nrow = 700)
bw.vec <- vector()
for (i in 1:100) {
	index.i <- sample(1:nrow(sc), 700)
	index.matrix[,i] <- index.i
	bw <- bw.gwr(reg.mod, data = sc[index.i,], kernel = "bisquare", 
    	adaptive = T, approach = "AIC", dMat = dMat[index.i, index.i])
    #rm(.Random.seed)
    bw.vec <- append(bw.vec, bw)
    cat(i, "\t")
}


set.seed(1)
index <- sample(1:nrow(sc), 2000)
bw <- bw.gwr(reg.mod3, data = sc[index,], kernel = "bisquare", 
    adaptive = T, approach = "AIC", dMat = dMat[index, index])

set.seed(1)
index <- sample(1:nrow(sc), 2000)
bw <- bw.gwr(reg.mod4, data = sc[index,], kernel = "bisquare", 
    adaptive = T, approach = "AIC", dMat = dMat[index, index])



  gw.col.i <- gwr.collin.diagno(reg.mod, data = sc, bw = bw, 
    kernel = "bisquare", adaptive = TRUE, dMat = dMat)

  
