### Yi-Min Chang Chien December 2019
## Aim for paper on scenicness with MGWR
## RQ: how do different factors relate to scenicness and at what scales do they operate?

### Load data and libraries
### set up data.sp etc
library(raster)
library(rgdal)
library(rgeos)
library(tidyr)
library(spatstat)
library(maptools)
library(GISTools)
library(GWmodel)
library(MASS)
library(sp)
library(sf)
library(ggplot2)
library(GGally)
library(landscapemetrics)
library(reshape2)
library(raster)
library(readr)
library(spatialEco)

##### 1
##### 1.1. load in Scenic-Or-Not dataset #####
bng <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
        +ellps=airy +datum=OSGB36 +units=m +no_defs"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

sc <- read_tsv("http://scenicornot.datasciencelab.co.uk/votes.tsv", 
               col_types = cols("ID" = col_number(),
                                "Lat" = col_double(),
                                "Lon" = col_double(),
                                "Average" = col_double(),
                                "Variance" = col_double(),
                                "Votes" = col_character(),
                                "Geograph URI" = col_character())) %>%
        as.data.frame %>%
        st_as_sf(coords=c("Lon","Lat"), crs=4326) %>%
        st_transform(crs=27700) %>%
        as("Spatial")

# for (i in 1:(dim(sc)[1])) {
#   sc$Median[[i]] <- median(as.numeric(strsplit(as.character(sc$Votes[[i]]), ",")[[1]]))
#   sc$Mean[[i]] <- mean(as.numeric(strsplit(as.character(sc$Votes[[i]]), ",")[[1]]))
#   sc$IQR[[i]] <- IQR(as.numeric(strsplit(as.character(sc$Votes[[i]]), ",")[[1]]))
#   sc$Variance[[i]] <- var(as.numeric(strsplit(as.character(sc$Votes[[i]]), ",")[[1]]))
# }

grid <- raster::getData("GADM", country = "United Kingdom", level = 1) %>% 
          subset(NAME_1 %in% c('England', 'Scotland','Wales')) %>%
          disaggregate %>%
          st_as_sf %>%
          st_transform(crs = 27700) %>%
          st_geometry %>%
          st_make_grid(cellsize = c(10000,10000), crs = 27700, what = "polygons", square = FALSE)
grid.sp <- as(grid, "Spatial")

setwd("/Users/Yi-Min/Rsession/ScenicOrNot/predictor variables/Wilderness Dimensions")
wildness <- list("access","naturalness","remoteness","ruggedness") %>% lapply(raster)

test <- lapply(list, raster)
     "access" %>% raster -> Abs
"naturalness" %>% raster -> Nat
 "remoteness" %>% raster -> Rem
 "ruggedness" %>% raster -> Rug
 
GB.sp <- as(GB, "Spatial") %>% spTransform(crs(Abs))


 Abs[mask(is.na(Abs), gBuffer(GB.sp, byid=T, width=-100))] <- 19.125

require(dplyr)
grid.sp <- as(grid.sp, "SpatialPolygonsDataFrame")
grid.sp$Sce <- over(grid.sp, sc[,'Average'], fn = median) %>% as.vector
grid.sp$Abs <- raster::extract(Abs, grid.sp, fun = median, na.rm = TRUE) %>% as.vector
grid.sp$Nat <- raster::extract(Nat, grid.sp, fun = median, na.rm = TRUE) %>% as.vector
grid.sp$Rem <- raster::extract(Rem, grid.sp, fun = median, na.rm = TRUE) %>% as.vector
grid.sp$Rug <- raster::extract(Rug, grid.sp, fun = median, na.rm = TRUE) %>% as.vector

test <- grid.sp@data %>% 
          as_tibble %>% 
          mutate(Sce = over(grid.sp, sc[,"Average"], fn = median)) %>% 
          mutate(
            'Abs' = raster::extract(Abs, grid.sp, fun = median, na.rm = TRUE),
            'Nat' = raster::extract(Nat, grid.sp, fun = median, na.rm = TRUE),
            'Rem' = raster::extract(Rem, grid.sp, fun = median, na.rm = TRUE),
            'Rug' = raster::extract(Rug, grid.sp, fun = median, na.rm = TRUE)
          )

####### END PART 1: load and peprpare data 

####### PART 2: initial analysis
# correlations and covariance of variables
grid.sp@data[,c(1,2:5)] %>% mutate_all(scale) %>% 
                            plot(cex = 0.5, col = grey(0.145,alpha=0.5), upper.panel=panel.smooth)
# covariance
## get some plots out, gets some table etc

####### END PART 2: initial analysis


####### PART 3: Regression
### Remember the aim of the GWR analysis
# is to better understand relationship heterogentity
# between scenicness and other variables
# That is how these vary spatially / locally

reg.mod <- as.formula(Sce ~ scale(Abs) + scale(Nat) + scale(Rem) + scale(Rug))
reg.mod <- as.formula(Sce ~ Abs + Nat + Rem + Rug)

# 3.1. Linear Regression

ols.m <- grid.sp %>% sp.na.omit(margin = 1) %>%
                     lm(reg.mod, data = .)
summary(ols.m)
round(coef(summary(ols.m)), 3)

# 3.1.1. examine some model selection
summary(stepAIC(ols.m, trace = 0))
round(coef(summary(stepAIC(ols.m, trace = 0))), 3)
reg.mod2 <- as.formula(summary(stepAIC(ols.m, trace = 0))[1])

# 3.1.2. examine the residual of the global model
s.resids = rstandard(ols.m) # to compute some of the regression (leave-one-out deletion) diagnostics for linear and generalized linear models discussed in Belsley, Kuh and Welsch (1980), Cook and Weisberg (1982)
resid.shades = shading(c(-2,2),c("red","grey","blue")) # red: overestimation; blue: underestimation
cols = resid.shades$cols[1 + findInterval(s.resids, resid.shades$breaks)]
plot(ols.m, col = cols)
abline(lm(reg.mod), col='red', lwd = 2, lty = 2)

#png(filename = "f2.png", w = 5, h = 5, units = "in", res = 300)
#par(mar=c(0,0,0,0)) 
grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  choropleth(s.resids, shading = resid.shades)
choro.legend(400000, 300000, resid.shades, fmt="%4.1g", cex = 0.5, title = 'Residuals Map')

# reset the plot margins
#par(mar=c(5,4,4,2))
#dev.off()
           
outlier_under <- which(rstandard(ols.m) > 2) 
 outlier_over <- which(rstandard(ols.m) < -2) 

urls <- grid.sp[row.names(grid.sp) %in% names(outlier_over), ] %>%
          over(sc, .) %>% 
          select("Geograph.URI")
setwd("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/overestimate")
download.img <- function(url) {
  require(jpeg)
  require(rvest)
  require(dplyr)
  id <- gsub("http://www.geograph.org.uk/photo/", "", url)
  url %>% 
    html_session %>%
    html_nodes("img") %>%
    .[1] %>%
    html_attr("src") %>%
    download.file(paste0(id,".jpg"), mode = "wb")
}

for (url in urls[,1]) {
  tryCatch(download.img(url),
           error = function(e) print(paste(url, 'did not work out')))    
}

#apply(urls, 1, download.img)

underestimate <- grid.sp[row.names(grid.sp) %in% names(outlier_under), ]
underestimate$fitted.value <- ols.m$fitted.values[which(names(ols.m$fitted.values) %in% row.names(underestimate))]

spplot(sc[underestimate,'Average'], pch=19, cex=0.1,
       sp.layout = list(underestimate[, 'Sce'], zcol=c("Sce"), first = FALSE)) 

require(dplyr)
underestimate@data <- tibble::rownames_to_column(underestimate@data, "HexID")

underestimate %>%
  st_as_sf() %>%
  st_intersection(st_as_sf(sc[,c("Average","Votes","Geograph.URI")])) %>%
  mutate(gridimage_id = as.integer(gsub("http://www.geograph.org.uk/photo/", "", Geograph.URI))) ->
  underestimation

sc <- read_tsv("http://scenicornot.datasciencelab.co.uk/votes.tsv", 
               col_types = cols("ID" = col_number(),
                                "Lat" = col_double(),
                                "Lon" = col_double(),
                                "Average" = col_double(),
                                "Variance" = col_double(),
                                "Votes" = col_character(),
                                "Geograph URI" = col_character())) %>%
  as.data.frame %>%
  st_as_sf(coords=c("Lon","Lat"), crs=4326) %>%
  st_transform(crs=27700) %>%
  as("Spatial")

Geograph <- function(url, pts) {
  url %>% url() %>%
    gzcon() %>%
    readLines() %>%
    textConnection() %>%
    read.csv(sep = "\t") %>%
    as.data.frame() %>%
    left_join(pts, ., by = "gridimage_id") ->
    pts
  return(pts)
}
underestimation %>%
  Geograph("http://data.geograph.org.uk/dumps/gridimage_base.tsv.gz", .) %>%
  Geograph("http://data.geograph.org.uk/dumps/gridimage_text.tsv.gz", .) %>%
  Geograph("http://data.geograph.org.uk/dumps/gridimage_geo.tsv.gz", .) ->
  underestimation


overestimate <- grid.sp[row.names(grid.sp) %in% names(outlier_over), ]
overestimate$fitted.value <- ols.m$fitted.values[which(names(ols.m$fitted.values) %in% row.names(overestimate))]



# 3.2. Spatial Autocorrelation
# 3.2.1. Global Moran's I for OLS regression residuals
require(spdep) 
lw <- grid.sp %>% sp.na.omit(margin = 1) %>%
                  coordinates %>% 
                  knearneigh(k = 6) %>%
                  knn2nb %>%
                  nb2listw
moran  <- lm.morantest(ols.m, lw)
print(moran)

# 3.2.2. Collinearity 


# 3.3. Local Model: 
# 3.3.1. standard GWR
dMat <- grid.sp %>% sp.na.omit(margin = 1) %>% 
                    coordinates %>%
                    gw.dist 
# Bandwidth selection
bw <- grid.sp %>% sp.na.omit(margin = 1) %>%
                  bw.gwr(reg.mod, data = ., kernel = "bisquare", adaptive = F, approach = "CV", dMat = dMat) 
# Fitting a GWR model
gwr.m <- grid.sp %>% sp.na.omit(margin = 1) %>% 
                     gwr.basic(reg.mod, data = ., bw = bw, kernel = "bisquare", adaptive = F, dMat = dMat)
gwr.m
summary(gwr.m$SDF)
spplot(grid.sp, zco="Sce", col=NA)
require(stringr)
names(gwr.m$SDF@data)[2:5] <- c("Abs","Nat","Rem","Rug")
spplot(gwr.m$SDF, zcol= names(gwr.m$SDF)[1:5], col=NA)

plots = lapply(names(gwr.m$SDF)[1:5], function(.x) spplot(gwr.m$SDF, .x, main = .x, col = NA))
require(gridExtra)
do.call("grid.arrange", c(plots, ncol=5))

# 3.3.2. Multiscale GWR
mgwr.m <- grid.sp %>% sp.na.omit(margin = 1) %>%
                      gwr.multiscale(reg.mod, data = ., kernel = "bisquare", adaptive = F, 
                                     criterion = "dCVR", threshold=0.00001, bws0=rep(bw, length=4), 
                                     predictor.centered=rep(TRUE, length=4))

names(mgwr.m$SDF@data)[2:5] <- c("Abs","Nat","Rem","Rug")

plots <- grid.sp %>% sp.na.omit(margin = 1) %>%
                     names %>%
                     lapply(function(.x) spplot(sp.na.omit(grid.sp, margin = 1), .x, main = .x, col = NA))
require(gridExtra)
do.call("grid.arrange", c(plots, ncol=5))

plots = lapply(names(mgwr.m$SDF)[1:5], function(.x) spplot(mgwr.m$SDF, .x, main = .x, col = NA))
require(gridExtra)
do.call("grid.arrange", c(plots, ncol=5))
spplot(mgwr.m$SDF, zcol= "residual", col=NA)

####### END PART 3: Regression - Global and Local Model


####### PART 4: Results - OLS and GWR
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
