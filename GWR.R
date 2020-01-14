### Yi-Min Chang Chien December 2019
## Aim for paper on scenicness with MGWR
## RQ: how do different factors relate to scenicness and at what scales do they operate?

### Load data and libraries
### set up data.sp etc
library(dplyr)
library(ggplot2)
library(GGally)
library(landscapemetrics)
library(tidyr)
library(maptools)
library(GISTools)
library(GWmodel)
library(MASS)
library(reshape2)
library(raster)
library(readr)
library(rgdal)
library(rgeos)
library(sp)
library(sf)
library(spatstat)
library(spatialEco)

##### 1
##### 1.1. load in Scenic-Or-Not dataset #####
bng <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
        +ellps=airy +datum=OSGB36 +units=m +no_defs"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

sc <- 
  read_tsv("http://scenicornot.datasciencelab.co.uk/votes.tsv",
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

# bng <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
#         +ellps=airy +datum=OSGB36 +units=m +no_defs"
# wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# 
# setwd("/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/")
# "ScenicOrNot_Geograph.csv" %>% read.csv() %>%
#   drop_na(wgs84_long, viewpoint_northings) %>%
#   #drop_na(viewpoint_eastings, viewpoint_northings) %>%
#   st_as_sf(coords = c("wgs84_long", "wgs84_lat"), remove=F, crs=wgs84) %>%
#   st_transform(crs=bng) %>% 
#   as("Spatial") -> sc
# 
# setwd("/Users/Yi-Min/Rsession/LANDMAP/Data/")
# wales <- readOGR("wales_ol_2001.shp") %>% spTransform(crs(sc))
# 
# setwd("/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness/OSGB_Grids-master/Shapefile")
# "OSGB_Grid_5km.shp" %>% readOGR() %>% spTransform(crs(sc)) -> grd
# #grd <- grd[which(grd$WALES=="t"),]

grid <-
  

grid <- 
  raster::getData("GADM", country = "United Kingdom", level = 1) %>%
  subset(NAME_1 %in% c('England', 'Scotland','Wales')) %>%
  disaggregate() %>%
  st_as_sf() %>%
  st_transform(crs = 27700) %>%
  st_geometry() %>%
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

test <- 
  grid.sp@data %>%
  as_tibble %>%
  mutate(Sce = over(grid.sp, sc[,"Average"], fn = median)) %>% 
  mutate('Abs' = raster::extract(Abs, grid.sp, fun = median, na.rm = TRUE),
         'Nat' = raster::extract(Nat, grid.sp, fun = median, na.rm = TRUE),
         'Rem' = raster::extract(Rem, grid.sp, fun = median, na.rm = TRUE),
         'Rug' = raster::extract(Rug, grid.sp, fun = median, na.rm = TRUE))

####### END PART 1: load and peprpare data 

####### PART 2: initial analysis
# correlations and covariance of variables
grid.sp@data[,c(1,2:5)] %>% 
  mutate_all(scale) %>%
  plot(cex = 0.5, col = grey(0.145, alpha=0.5), upper.panel=panel.smooth)

# covariance
## get some plots out, gets some table etc
grd <- st_read("sc_5k_bisquare.shp") %>% .[,c(5,1:4)]
grd$Abs <- scale(grd$Abs)
grd$Nat <- scale(grd$Nat)
grd$Rem <- scale(grd$Rem)
grd$Rug <- scale(grd$Rug)
summary(grd)
corr.m <- grd %>% st_drop_geometry() %>% cor()
corr.k <- grd %>% st_drop_geometry() %>% cor(., method = "kendall", use = "pairwise")

# Pair-wise correlation, scatter plot and distribution 
my_custom_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = I("blue"), alpha = 0.3, cex = 0.1) + 
    geom_smooth(method = "lm", lwd = 0.5, color = I("red3"), ...)
}

# for upper panel plot
my_custom_cor <- function(data, mapping, color = I("black"), sizeRange = c(1.5, 3), ...) {
  # get the x and y data to use the other code
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  ct <- cor.test(x,y, method = "pearson")
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

#setwd("/Users/Yi-Min/Python/MGWR/")
#png(filename = "SpearmanCorrelation_sqrt.png", w = 24/3, h = 24/3, units = "in", res = 300)
grd %>% 
  st_drop_geometry() %>%
  ggpairs(aes(alpha = 0.4),
          upper = list(continuous = my_custom_smooth),
          lower = list(continuous = my_custom_cor)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
#dev.off()

####### END PART 2: initial analysis


####### PART 3: Regression
### Remember the aim of the GWR analysis
# is to better understand relationship heterogentity
# between scenicness and other variables
# That is how these vary spatially / locally
reg.mod <- as.formula(Sce ~ scale(Abs) + scale(Nat) + scale(Rem) + scale(Rug))
reg.mod <- as.formula(Sce ~ scale(Abs) + scale(Nat) + scale(Rug))
reg.mod <- as.formula(Sce ~ scale(Nat) + scale(Rem) + scale(Rug))
reg.mod <- as.formula(Sce ~ Nat + Rem + Rug)
bc <- boxcox(Sce ~ Rem + Rug, data = st_drop_geometry(grd))
# 3.1. Linear Regression
ols.m <- 
  grdid.sp %>% 
  sp.na.omit(margin = 1) %>%
  lm(reg.mod, data = .)
ols.m <- lm(reg.mod, data = st_drop_geometry(grd))
summary(ols.m)
round(coef(summary(ols.m)), 3)

###### 3.1.1. examine some model selection
summary(stepAIC(ols.m, trace = 0))
round(coef(summary(stepAIC(ols.m, trace = 0))), 3)
reg.mod2 <- as.formula(summary(stepAIC(ols.m, trace = 0))[1])

###### 3.1.2. examine the variance inflation factor (VIF)
require(faraway)
vif(ols.m)

###### 3.1.3. examine and visualise the outliers of the global model
s.resids = rstandard(ols.m) # to compute some of the regression (leave-one-out deletion) diagnostics for linear and generalized linear models discussed in Belsley, Kuh and Welsch (1980), Cook and Weisberg (1982)
resid.shades = shading(c(-2,2),c("red","grey","blue")) # red: overestimation; blue: underestimation
cols = resid.shades$cols[1 + findInterval(s.resids, resid.shades$breaks)]
plot(ols.m, col = cols)
abline(lm(reg.mod), col='red', lwd = 2, lty = 2)

#png(filename = "f2.png", w = 5, h = 5, units = "in", res = 300)
#par(mar=c(0,0,0,0)) 
#grid.sp %>% 
#  sp.na.omit(margin = 1) %>%
grd %>%
  as("Spatial") %>%
  choropleth(s.resids, shading = resid.shades)
choro.legend(400000, 300000, resid.shades, fmt="%4.1g", cex = 0.5, title = 'Residuals Map')

#grid.sp %>% 
#  sp.na.omit(margin = 1) %>%
#  st_as_sf() %>%
grd %>%
  mutate(Residuals = rstandard(ols.m)) %>%
  dplyr::select(Residuals) ->
  hex.res

  ggplot(data = hex.res) +
  geom_sf(aes(fill = Residuals)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

require(classInt)
# get quantile breaks. Add .00001 offset to catch the lowest value
breaks_qt <- classIntervals(c(min(hex.res$Residuals) - .00001, hex.res$Residuals), n = 9, style = "quantile")
breaks_qt
hex.res %>% 
  mutate(res_cat = cut(Residuals, breaks_qt$brks)) %>%
  ggplot(.) + 
  geom_sf(aes(fill=res_cat), colour = NA) +
  scale_fill_brewer(palette = "RdBu") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())  

# reset the plot margins
#par(mar=c(5,4,4,2))
#dev.off()
           
outlier_under <- which(rstandard(ols.m) > 2) 
 outlier_over <- which(rstandard(ols.m) < -2) 

###### 3.1.3. Moran's I test on the residuals of the linear regression

  
###### 3.1.4. incorporate the Geograph metadata and images for the outliers
sc <-
   read_tsv("http://scenicornot.datasciencelab.co.uk/votes.tsv",
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
  pts <-
    url %>% url() %>%
    gzcon() %>%
    readLines() %>%
    textConnection() %>%
    read.csv(sep = "\t") %>%
    as.data.frame() %>%
    left_join(pts, ., by = "gridimage_id")
  return(pts)
}

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

require(dplyr)
overestimation <-
  grid.sp %>%
  st_as_sf() %>%
  mutate(HexID = row.names(.)) %>%
  filter(HexID %in% names(outlier_over)) %>%
  left_join(data.frame(ols.m$fitted.values, HexID = names(ols.m$fitted.values), stringsAsFactors=FALSE)) %>%
  st_intersection(st_as_sf(sc[,c("Average","Votes","Geograph.URI")])) %>%
  mutate(gridimage_id = as.integer(gsub("http://www.geograph.org.uk/photo/", "", Geograph.URI))) %>%
  Geograph("http://data.geograph.org.uk/dumps/gridimage_base.tsv.gz", .) %>%
  Geograph("http://data.geograph.org.uk/dumps/gridimage_text.tsv.gz", .) %>%
  Geograph("http://data.geograph.org.uk/dumps/gridimage_geo.tsv.gz", .)

setwd("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/overestimate")
require(foreach)
urls <-
  overestimation %>%
  select(URI = Geograph.URI) %>%
  st_drop_geometry()

for (url in urls[,1]) {
  tryCatch(download.img(url),
           error = function(e) print(paste(url, 'did not work out')))    
}

# cl <- parallel::makeCluster(2)
# doParallel::registerDoParallel(cl)
# tryCatch(download.img(urls), error = function(e) print(e))
# foreach(i = nrow(urls)) %do% tryCatch(download.img(urls[i,]), error = function(e) print(past(n, 'did not work out')))

# spplot(sc[underestimate,'Average'], pch=19, cex=0.1,
#       sp.layout = list(underestimate[, 'Sce'], zcol=c("Sce"), first = FALSE)) 

###### 3.1.5. download the images of the outliers within the Lake District 
temp <- tempfile()
temp2 <- tempfile()
download.file("https://inspire.nationalparks.uk/geoserver/ldnpa_inspire/ows?service=WFS&request=GetFeature&version=2.0.0&typeName=ldnpa_inspire:LDNPA_Boundary&outputFormat=shape-zip", temp)
unzip(zipfile = temp, exdir = temp2)
np <- st_read(file.path(temp2, "LDNPA_Boundary.shp"))
rm(list = c("temp", "temp2"))

underestimation[np,] %>%
  filter(ols.m.fitted.values < Average) %>%
  select(URI = Geograph.URI) %>%
  st_drop_geometry() ->
  urls

setwd("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/underestimate/Lake District")
for (url in urls[,1]) {
  tryCatch(download.img(url),
           error = function(e) print(paste(url, 'did not work out')))    
}

underestimation[np,] %>%
  filter(ols.m.fitted.values < Average) %>%
#  filter(HexID == "ID1766") %>%
  select(HexID) %>%
  st_drop_geometry() %>%
  unique()


##### 3.1.6.
temp <- tempfile()
temp2 <- tempfile()
download.file("https://www.arcgis.com/sharing/rest/content/items/3ce248e9651f4dc094f84a4c5de18655/data", temp)
unzip(zipfile = temp, exdir = temp2)
read.csv(file.path(temp2, "RUC11_OA11_EW.csv"), stringsAsFactors = FALSE) ->
  RUC_OA
rm(list = c("temp", "temp2"))

temp <- tempfile()
temp2 <- tempfile()
download.file("https://opendata.arcgis.com/datasets/09b8a48426e3482ebbc0b0c49985c0fb_1.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D", temp)
unzip(zipfile = temp, exdir = temp2)
"Output_Area_December_2011_Full_Extent_Boundaries_in_England_and_Wales.shp" %>%
  file.path(temp2, .) %>% 
  st_read(stringsAsFactors = FALSE) ->
  OA
OA <- OA %>% left_join(RUC_OA, by = c("oa11cd" = "OA11CD"))

OA %>%
  filter(RUC11 == "Urban major conurbation" | 
         RUC11 == "Urban city and town" |
         RUC11 == "Urban minor conurbation" |
         RUC11 == "Urban city and town in a sparse setting") %>%
  group_by(RUC11) %>% 
  summarize() ->
  Urban 
  
require(classInt)
# get quantile breaks. Add .00001 offset to catch the lowest value
breaks_qt <- classIntervals(c(min(hex.res$Residuals) - .00001, hex.res$Residuals), n = 9, style = "quantile")
breaks_qt
hex.res %>% 
  mutate(res_cat = cut(Residuals, breaks_qt$brks)) %>%
  ggplot(.) + 
  geom_sf(aes(fill=res_cat), colour = NA) +
  geom_sf(data = Urban, color=alpha("green"), fill = NA) +
  scale_fill_brewer(palette = "RdBu") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())  

grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  st_as_sf() %>%
  st_transform(st_crs(Urban)) %>%
  .[Urban, ] %>%
  as("Spatial") %>%
  lm(reg.mod, data = .) %>%
  summary()


##### 3.2. examine the presence of spatial autocorrelation in the residuals of the global model
###### 3.2.1. Global Moran's I for OLS regression residuals
require(spdep) 
grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  coordinates %>% 
  knearneigh(k = 6) %>%
  knn2nb %>%
  nb2listw %>%
  lm.morantest(ols.m, .) %>%
  print()

# 3.2.2. Collinearity 


# 3.3. Local Model: 
# 3.3.1. standard GWR
dMat <-
  grid.sp %>% 
  sp.na.omit(margin = 1) %>% 
  coordinates() %>%
  gw.dist()

# Bandwidth selection
bw <-
  grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  bw.gwr(reg.mod, data = ., 
         kernel = "bisquare", 
         adaptive = F, 
         approach = "CV", 
         dMat = dMat)

# Fitting a GWR model
gwr.m <- 
  grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  gwr.basic(reg.mod, data = ., bw = bw, kernel = "bisquare", adaptive = F, dMat = dMat)
gwr.m
summary(gwr.m$SDF)

# 3.3.2. Multiscale GWR
mgwr.n <- 
  grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  gwr.multiscale(reg.mod, data = ., kernel = "gaussian", adaptive = F,
                 criterion = "dCVR", max.iterations=125, threshold=0.00001, 
                 bws0=rep(bw, length=4), predictor.centered=rep(TRUE, length=4))

names(mgwr.m$SDF@data)[2:5] <- c("Abs","Nat","Rem","Rug")

plots <- 
  grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  names %>%
  lapply(function(.x) spplot(sp.na.omit(grid.sp, margin = 1), .x, main = .x, col = NA))
require(gridExtra)
do.call("grid.arrange", c(plots, ncol=5))

plots = lapply(names(mgwr.m$SDF)[1:5], function(.x) spplot(mgwr.m$SDF, .x, main = .x, col = NA))
require(gridExtra)
do.call("grid.arrange", c(plots, ncol=5))
spplot(mgwr.m$SDF, zcol= "residual", col=NA)

####### END PART 3: Regression - Global and Local Model

####### PART 4: Results - Tables and Figures #######
##### 4.1. Linear regression results (TABLE 1) 
 
ols.m %>%
  summary() %>%
  coef() %>%
  round(3) %>%
  data.frame() ->
  tab1

grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  st_as_sf() %>%
  st_transform(st_crs(Urban)) %>%
  .[Urban, ] %>%
  as("Spatial") %>%
  lm(reg.mod, data = .) ->
  ols.m.urban

# categorising the settings into urban, suburban, and rural areas
mapping = data.frame(RUC11=unique(OA$RUC11), category = c("Urban","Suburban","Rural","Rural","Rural",
                                                          "Suburban","Rural","Rural","Rural","Suburban"))

# plot fitted and observed scatter plot
grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  st_as_sf() %>%
  st_transform(st_crs(OA)) %>%
  mutate(fitted.values = ols.m$fitted.values) %>%
  select("Actual.Scenicness" = "Sce", "Estimated.Scenicness" = "fitted.values") %>%
  st_join(OA[,"RUC11"], left = TRUE) %>%
  st_drop_geometry() %>%
  merge(mapping, by= "RUC11") ->
  data

grid.sp %>% 
  sp.na.omit(margin = 1) ->
  grd

grd %>% 
  gCentroid(., byid = TRUE) %>% 
  coordinates() %>% 
  as.data.frame() ->
  grd_cent

grd <- SpatialPolygonsDataFrame(grd, data = data.frame(grd, grd_cent))
writeOGR(grd, dsn="/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/Grid", "grd_10k", driver="ESRI Shapefile", overwrite=TRUE)


  mutate(X = st_centroid(.), "Y" = st_centroid(.)) %>%
  
  
  grd %>% gCentroid(., byid = TRUE) %>% coordinates() %>% as.data.frame() -> grd_cent 

ggscatterhist(data, x = "Actual.Scenicness", y = "Estimated.Scenicness",
              color = "category", shape = 19, size = 0.5, alpha = 0.6,
              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
              #add =  "reg.line",
              margin.plot = "density", margin.space = FALSE,
              margin.params = list(fill = "category", color = "black", size = 0.2),
              main.plot.size = 2, margin.plot.size = 1,
              title = NULL, xlab = "Actual scenicness", ylab = "Estimated scenicness",
              legend = "bottom", ggtheme = theme_bw())

ggscatterhist(data, x = "Actual.Scenicness", y = "Estimated.Scenicness",
              color = "category", shape = 19, size = 0.5, alpha = 0.6,
              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
              add =  "reg.line",
              margin.plot = "boxplot", margin.space = FALSE,
              margin.params = list(fill = "category", color = "black", size = 0.2),
              main.plot.size = 2, margin.plot.size = 1,
              title = NULL, xlab = "Actual scenicness", ylab = "Estimated scenicness",
              legend = "bottom", ggtheme = theme_bw())


##### TABLE 2.
tab2a <-
  gwr.m$SDF@data[,1:5] %>%
  apply(2, function(x) summary(x)[c(2,3,5)]) %>%
  t() %>%
  round(3)
bw.vec <- round(mgwr.m[[5]][nrow(mgwr.m[[5]]), ]/1000, 1)
bw.perc <- round(mgwr.m[[5]][nrow(mgwr.m[[5]]),]/ max(dMat)*100, 1)
tab2b <- 
  mgwr.m$SDF@data[,1:5] %>%
  apply(2, function(x) summary(x)[c(2,3,5)]) %>%
  t() %>%
  round(3)
### Python-based implementation
tab2c <- 
  gwr.m$SDF@data[,1:5] %>%
  apply(2, function(x) summary(x)[c(2,3,5)]) %>%
  t() %>%
  round(3)

tab2  <- data.frame(tab2a, round(tab2a[,3]-tab2a[,1], 3), 
                    bandwidths = bw.vec, percentage = bw.perc, 
                    tab2b, round(tab2b[,3]-tab2b[,1], 3))
colnames(tab2) <- c("Standard 1st Qu.", "Standard Median", "Standard 3rd Qu.", "Standard IQR", "bandwidths", "percentage", "FB 1st Qu.", "FB Median", "FB 3rd Qu.", "FB IQR" )
kable(tab2)


###### FIGURE 2. The residuals of the global model 
grid.sp %>% 
  sp.na.omit(margin = 1) %>%
  st_as_sf() %>%
  mutate(Residuals = ols.m$residuals) %>%
  select(Residuals) ->
  hex.res

ggplot(data = hex.res) +
  geom_sf(aes(fill = Residuals)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

require(classInt)
# get quantile breaks. Add .00001 offset to catch the lowest value
breaks_qt <- classIntervals(c(min(hex.res$Residuals) - .00001, hex.res$Residuals), n = 9, style = "quantile")
breaks_qt
hex.res %>% 
  mutate(res_cat = cut(Residuals, breaks_qt$brks)) %>%
  ggplot(.) + 
  geom_sf(aes(fill=res_cat), colour = NA) +
  scale_fill_brewer(palette = "RdBu") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())  

############################################################

###### FIGURE 3. The coefficient estimates of the GWR model
names(gwr.m$SDF) <- 
  names(gwr.m$SDF) %>% 
  gsub("scale(","", ., fixed = TRUE) %>% 
  gsub(")", "", ., fixed = TRUE)

setwd("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/Grid")
 t_val <- cbind(read.csv("gwr_tvals_5k.csv", header = FALSE), read.csv("mgwr_tvals_5k.csv", header = FALSE))
 names(t_val) <- c( "GWR_Int_TV", "GWR_Abs_TV", "GWR_Nat_TV", "GWR_Rem_TV", "GWR_Rug_TV",
                   "MGWR_Int_TV","MGWR_Abs_TV","MGWR_Nat_TV","MGWR_Rem_TV","MGWR_Rug_TV")
 grd_5k <- st_read("sc_mgwr_5k.shp")  
 names(grd_5k) <- names(grd_5k) %>% sub("mgwr", "MGWR", .) %>% sub("gwr", "GWR", .) %>% sub("int", "Int", .)
 grd_5k <- st_sf(data.frame(grd_5k, t_val))
 st_write(grd_5k, "sc_5k.shp")

 
 grd_10k <- st_read("sc_10k.shp")
summary(grd_10k)
names(grd_10k)[8:27] <- c( "GWR_Int", "GWR_Abs", "GWR_Nat", "GWR_Rem", "GWR_Rug",
                          "MGWR_Int","MGWR_Abs","MGWR_Nat","MGWR_Rem","MGWR_Rug",
                            "GWR_Int_TV", "GWR_Abs_TV", "GWR_Nat_TV", "GWR_Rem_TV", "GWR_Rug_TV",
                           "MGWR_Int_TV","MGWR_Abs_TV","MGWR_Nat_TV","MGWR_Rem_TV","MGWR_Rug_TV")

ggplot.fun <- function(data.i = sf, var = "Int", type = "pos", tab = "MGWR") {
  require(ggplot2)
  require(scales)
  TV = paste0(tab,"_",var,"_TV")
  TV = ensym(TV)
  TV_sig <-
    data.i %>% 
    st_as_sf() %>%
    filter(!!TV > 1.96 | !!TV < -1.96)
  tit = paste0(tab, ": ", var)
  Val = paste0(tab,"_", var)
  Val = sym(Val)
  p <-  
    ggplot() +
    #geom_sf(data = summarise(data.i), fill = NA) +
    geom_sf(data = data.i, aes(fill = !!Val), lwd = 0) +
    coord_sf() +
    labs(x = NULL, y = NULL, title = tit, size = 1) +
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
      p <- p + scale_fill_gradient2(low = ("#B2182B"), mid = "#F7F7F7", name = NULL,
                                    high = ("#2166AC"), midpoint = 0, space = "Lab",
                                    na.value = "white",
                                    guide = guide_colourbar(title.position = "top",
                                                            title.hjust = 0.5,
                                                            title.vjust = 0.8,
                                                            barwidth = 15))
    }
    if (type == "pos") {
      p <- p + scale_fill_distiller(type = "seq", palette = "YlGnBu", name = NULL, direction = 1,
                                    #breaks = pretty_breaks(n = 5),
                                    guide = guide_colourbar(title.position = "top",
                                                            title.hjust = 0.5,
                                                            title.vjust = 0.8,
                                                            barwidth = 15))
    }
    if (type == "neg") {
      p <- p + scale_fill_distiller(type = "seq", palette = "YlOrRd", name = NULL, direction = 1,
                                      guide = guide_colourbar(title.position = "top",
                                                              title.hjust = 0.5,
                                                              title.vjust = 0.8,
                                                              barwidth = 15))
    }
    
   p <- p + 
     geom_sf(data = TV_sig, fill = NA, colour = "black", alpha = 0.5, lwd = 0.1) +
     coord_sf()
  return(p)
}
p1 <- ggplot.fun(data.i = grd_10k, var = "Int", type = "pos", tab = "GWR")
p2 <- ggplot.fun(data.i = grd_10k, var = "Abs", type = "div", tab = "GWR")
p3 <- ggplot.fun(data.i = grd_10k, var = "Nat", type = "div", tab = "GWR")
p4 <- ggplot.fun(data.i = grd_10k, var = "Rem", type = "div", tab = "GWR")    
p5 <- ggplot.fun(data.i = grd_10k, var = "Rug", type = "div", tab = "GWR")    

p6 <- ggplot.fun(data.i = grd_10k, var = "Int", type = "pos", tab = "MGWR")
p7 <- ggplot.fun(data.i = grd_10k, var = "Abs", type = "div", tab = "MGWR")
p8 <- ggplot.fun(data.i = grd_10k, var = "Nat", type = "pos", tab = "MGWR")
p9 <- ggplot.fun(data.i = grd_10k, var = "Rem", type = "pos", tab = "MGWR")    
p10 <- ggplot.fun(data.i = grd_10k, var = "Rug", type = "div", tab = "MGWR")    

## multiplot function
multiplot <- function(plot.list, file, cols=3, layout=NULL) {
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

## map together
# setwd("/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness")
# png(filename = "GWR_MGWR_wilderness.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot(list(p1, p2, p3, p4, p5), cols = 5)
#dev.off()

par(mar = c(0,0,0,0))
multiplot(list(p6, p7, p8, p9, p10), cols = 5)

###### results of MGWR
setwd("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/Grid")
test <- read.csv("mgwr_tvals_20200102.csv")
grd %>%
  

####### End PART 4: Results - Tables and Figures #######   

if (type == "div") {
  p <- p + scale_fill_gradient2(low = ("#CB181D"), mid = "white", name = NULL,
                                high = ("#2171B5"), midpoint = 0, space = "Lab",
                                na.value = "white",
                                guide = guide_colourbar(title.position = "top",
                                                        title.hjust = 0.5,
                                                        title.vjust = 0.8,
                                                        barwidth = 15))
  


scale_fill_manual(values = plasma(9),
                  breaks = brks,
                  name = NULL,
                  drop = FALSE,
                  labels = levels(data.i$brks),
                  guide = guide_legend(direction = "horizontal",
                                       keyheight = unit(2, units = "mm"),
                                       keywidth = unit(70/length(labels), units = "mm"),
                                       title.position = 'top',
                                       title.hjust = 0.5,
                                       label.hjust = 1,
                                       nrow = 1,
                                       byrow = T,
                                       reverse = T,
                                       label.position = "bottom")) +

  
  
  
  
  
  scale_fill_manual(values = plasma(8),
                    breaks = brks,
                    name = NULL,
                    drop = FALSE,
                    #labels = labels_scale,
                    guide = guide_legend(direction = "horizontal",
                                         keyheight = unit(2, units = "mm"),
                                         keywidth = unit(70 / length(labels), units = "mm"),
                                         #title.position = 'top',
                                         title.hjust = 0.5,
                                         label.hjust = 1,
                                         nrow = 1,
                                         byrow = T,
                                         reverse = T,
                                         label.position = "bottom")
  )
