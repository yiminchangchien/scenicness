library(rgdal)
library(raster)
library(tidyverse)
library(broom)
library(rgeos)
library(GGally)
library(reshape2)
library(ggplot2)
library(rasterVis)
library(grid)
library(scales)
library(viridis)
library(ggthemes)

## FIGURE 1. mapping dependent and independent variables
setwd("/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness")
 wal.shp <- readOGR(dsn = "./Wales boundary/wales_ol_2001.shp")
  np.shp <- readOGR(dsn = "./NationalParks/NRW_NATIONAL_PARKPolygon.shp")
aonb.shp <- readOGR(dsn = "./AreaofOutstandingNaturalBeauty/NRW_AONBPolygon.shp")
  sc.shp <- readOGR(dsn = "/Users/Yi-Min/Python/MGWR/sc_mgwr_20190811.shp", stringsAsFactors = FALSE)

bng   <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
          +ellps=airy +datum=OSGB36 +units=m +no_defs"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
          
  sc.shp <- spTransform(sc.shp, bng)
 wal.shp <- spTransform(wal.shp,bng)
  np.shp <- spTransform(np.shp, bng)
aonb.shp <- spTransform(aonb.shp, bng)
aonb.shp <- aonb.shp[wal.shp, ]
aonb.shp <- aonb.shp[-3, ]

  sc.df <- as.data.frame(sc.shp, region = "OBJECTID")
 wal.df <- tidy(wal.shp)
  np.df <- tidy(np.shp)
aonb.df <- tidy(aonb.shp)

# mapping the dependent variables

r1 <- ggplot(wal.df) +
	    geom_polygon(aes(x = long, y = lat, group = group),
	   			     colour="grey", fill="grey") + # fill = "grey92"
 		geom_point(data = sc.df, aes(x = coords.x1, y = coords.x2, colour = sc.df$Sce_mean), size = 0.3) + 
 		scale_colour_distiller(palette = "YlOrBr", direction = 1, name = "Mean of Scenic Rating",
 		 					   guide = guide_colourbar(title.position = "top",
 		 					                           title.hjust = 0.5,
 		 					                           title.vjust = 0.8,
 		 					                           barwidth = 15)) +
 		geom_polygon(data = np.df, aes(x = long, y = lat, group = group),
   		 			 fill = "transparent", color = "black") +
   		geom_polygon(data = aonb.df, aes(x = long, y = lat, group = group),
   		 			 fill = "transparent", color = "black") +
 		coord_equal() +	# coord_map()	  		 
 		labs(x = NULL, y = NULL, title = "Sce", size = 4) +
 		theme(axis.line = element_blank(),
  			  axis.text = element_blank(),
  		 	  axis.ticks = element_blank(),
  		 	  legend.position = "bottom",
  		 	  legend.direction = "horizontal",
  		 	  panel.grid.major = element_blank(),
  		 	  panel.grid.minor = element_blank(),
  		 	  panel.border = element_blank(),
  			  panel.background = element_blank()) 
  		 #labs(colour = "Median of Scenic Rating", position = "top") 
  		 #guides(colour = guide_legend(title.position = "top"), guide = "colourbar") 



# mapping all the independent variables
setwd("/Users/Yi-Min/Rsession/ScenicOrNot/predictor variables/")
Abs <- raster("./Wilderness Dimensions/absence_Wales.tif")
Nat <- raster("./Wilderness Dimensions/natural_Wales.tif")
Rem <- raster("./Wilderness Dimensions/remote_Wales.tif")
Rug <- raster("./Wilderness Dimensions/rugged_Wales.tif")
LCM <- stack("./CEH Land Cover Map/LCM2015_Wales_1km_percent_cover_aggregate_class.tif")


#x = (x - x.mean(axis=0)) / x.std(axis=0)
#y = (y - y.mean(axis=0)) / y.std(axis=0)


## raster plot function
raster.plot <- function(raster = Abs, name = "Abs", colormap = "OrRd") {
	raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    names(raster_df) <- c(name, "x", "y")

	ggplot(wal.df) + 
	geom_polygon(aes(x = long, y = lat, group = group), 
	 	 		 colour="grey92", fill="grey92") +
  	geom_tile(data = raster_df, aes(x = x, y = y, fill = raster_df[, 1]), alpha = 0.8) +
  	coord_equal() + 
  	labs(x = NULL, y = NULL, title = name, size = 4) + 
  	#theme_map() + 
  	    #scale_fill_viridis(option = colormap, direction = -1, name = NULL,
    	#			 	   guide = guide_colourbar(title.position = "top",
	#					    				       title.hjust = 6,
	#					     	   			   title.vjust = 0.8,
	#					     	   			   barwidth = 10))
    scale_fill_distiller(palette = colormap, direction = 1, name = NULL,
						   guide = guide_colourbar(#title.position = "top",
						       					   #title.hjust = 6,
						     				       #title.vjust = 0.8,
						      				       barwidth = 15)) + 
    theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  axis.ticks = element_blank(),
  		  legend.position = "bottom",
  		  legend.direction = "horizontal",
  		  panel.grid.major = element_blank(),
  		  panel.grid.minor = element_blank(),
  		  panel.border = element_blank(),
  		  panel.background = element_blank())  # legend.key.width = unit(2, "cm")

}
   	
  
 r2 <- raster.plot(raster = Abs, name = "Abs", colormap = "Greens")
 r3 <- raster.plot(raster = Nat, name = "Nat", colormap = "Blues")
 r4 <- raster.plot(raster = Rem, name = "Rem", colormap = "PuRd")
 r5 <- raster.plot(raster = Rug, name = "Rug", colormap = "BuPu")
 r6 <- raster.plot(raster = LCM[[1]], name = "LCM1", colormap = "Purples")
 r7 <- raster.plot(raster = LCM[[2]], name = "LCM2", colormap = "YlGn")
 r8 <- raster.plot(raster = LCM[[3]], name = "LCM3", colormap = "Oranges")
 r9 <- raster.plot(raster = LCM[[4]], name = "LCM4", colormap = "BuGn")
r10 <- raster.plot(raster = LCM[[5]], name = "LCM5", colormap = "GnBu")
r11 <- raster.plot(raster = LCM[[6]], name = "LCM6", colormap = "PuBuGn")
r12 <- raster.plot(raster = LCM[[7]], name = "LCM7", colormap = "YlGnBu")
r13 <- raster.plot(raster = LCM[[8]], name = "LCM8", colormap = "YlOrRd")
r14 <- raster.plot(raster = LCM[[9]], name = "LCM9", colormap = "OrRd")
r15 <- raster.plot(raster = LCM[[10]], name = "LCM10", colormap = "Greys")

## map dependent and independent variables together
setwd("/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness")
png(filename = "FIGURE1a.png", w = 10*2, h = 6*3, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(r1, r6, r11, r2, r7, r12, r3, r8, r13, r4, r9, r14, r5, r10, r15), cols = 5)
dev.off()

  
## FIGURE 3. GWR v.s. MGWR estimates for the wilderness variables 
setwd("/Users/Yi-Min/Python/MGWR")
sc.mgwr <- readOGR(dsn = "/Users/Yi-Min/Python/MGWR/sc_mgwr_20190811.shp", stringsAsFactors = FALSE)
 sc.gwr <- readOGR(dsn = "/Users/Yi-Min/Python/MGWR/sc_gwr_20190929.shp", stringsAsFactors = FALSE)
mgwr_tv <- read.csv(header = FALSE, "/Users/Yi-Min/Python/MGWR/mgwr_tvals_20190811.csv")
 gwr_tv <- read.csv(header = FALSE, "/Users/Yi-Min/Python/MGWR/gwr_tvals_20190929.csv", )
mgwr.df <- as.data.frame(sc.mgwr[26:40], region = "OBJECTID")
 gwr.df <- as.data.frame(sc.gwr[26:40], region = "OBJECTID")
   test <- cbind(gwr.df, gwr_tv)
 
names(mgwr.df)[1:15] <- c("Int", "Abs", "Nat", "Rem", "Rug", 
						  "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", 
					      "LCM6", "LCM7", "LCM8", "LCM9", "LCM10")
names(mgwr_tv)[1:15] <- c("Int", "Abs", "Nat", "Rem", "Rug", 
						  "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", 
					      "LCM6", "LCM7", "LCM8", "LCM9", "LCM10")
 names(gwr.df)[1:15] <- c("Int", "Abs", "Nat", "Rem", "Rug", 
						  "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", 
					      "LCM6", "LCM7", "LCM8", "LCM9", "LCM10")
 names(gwr_tv)[1:15] <- c("Int", "Abs", "Nat", "Rem", "Rug", 
						  "LCM1", "LCM2", "LCM3", "LCM4", "LCM5", 
					      "LCM6", "LCM7", "LCM8", "LCM9", "LCM10")

## plot function
ggplot.func <- function(data.i = mgwr.df, TV = t_value, i = 2, type = "pos", tab = "MGWR") {
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
		p <- p + scale_colour_gradient2(low = ("#CB181D"), mid = "white", name = NULL,
								 high = ("#2171B5"), midpoint = 0, space = "Lab",
								 na.value = "white",
								 guide = guide_colourbar(title.position = "top",
								 							title.hjust = 0.5,
								 							title.vjust = 0.8,
								 							barwidth = 15))
	}
	if (type == "pos") {
		p <- p + scale_colour_distiller(type = "seq", palette = "GnBu", name = NULL, direction = 1,
								   guide = guide_colourbar(title.position = "top",
								   						   title.hjust = 0.5,
								   						   title.vjust = 0.8,
								   						   barwidth = 15))
	}
	if (type == "neg") {
		p <- p + scale_colour_distiller(type = "seq", palette = "OrRd", name = NULL, direction = 1,
								   guide = guide_colourbar(title.position = "top",
								   						   title.hjust = 0.5,
								   						   title.vjust = 0.8,
								   						   barwidth = 15))
	}	
	p
}

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
## FIGURE 3. mapping GWR and MGWR estimates for the wildness covariates
summary(gwr.df[, 1:5])
p1 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 1, type = "div", tab = "GWR") 
p2 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 2, type = "pos", tab = "GWR") 
p3 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 3, type = "div", tab = "GWR") 
p4 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 4, type = "div", tab = "GWR") 
p5 <- ggplot.func(data.i = gwr.df, TV = gwr_tv, i = 5, type = "div", tab = "GWR")
## MGWR
summary(mgwr.df[, 1:5])
 p6 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 1, type = "pos", tab = "MGWR") 
 p7 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 2, type = "pos", tab = "MGWR") 
 p8 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 3, type = "pos", tab = "MGWR") 
 p9 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 4, type = "div", tab = "MGWR") 
p10 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 5, type = "pos", tab = "MGWR")
## map together
setwd("/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness")
png(filename = "GWR_MGWR_wilderness.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(p1, p6, p2, p7, p3, p8, p4, p9, p5, p10), cols = 5)
dev.off()

## FIGURE 4. MGWR estimates for the LC variables 
summary(mgwr.df[, 6:15])
p9 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 6, type = "pos", tab = "MGWR") 
p10 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 7, type = "neg", tab = "MGWR") 
p11 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 8, type = "neg", tab = "MGWR") 
p12 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 9, type = "pos", tab = "MGWR") 
p13 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 10, type = "pos", tab = "MGWR") 
p14 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 11, type = "div", tab = "MGWR") 
p15 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 12, type = "neg", tab = "MGWR") 
p16 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 13, type = "pos", tab = "MGWR")
p17 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 14, type = "div", tab = "MGWR")
p18 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 15, type = "neg", tab = "MGWR")

png(filename = "MGWR_LCM.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(p9, p14, p10, p15, p11, p16, p12, p17, p13, p18), cols = 5)
dev.off()






## predictor variables
summary(mgwr.df[, 1:4])
p1 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 1, type = "pos", tab = "") 
p2 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 2, type = "pos", tab = "") 
p3 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 3, type = "pos", tab = "") 
p4 <- ggplot.func(data.i = mgwr.df, TV = mgwr_tv, i = 4, type = "pos", tab = "")
summary(mgwr.df[, 5:14])
p5 <- ggplot.func(data.i = mgwr.df, i = 5, type = "pos", tab = "") 
p6 <- ggplot.func(data.i = mgwr.df, i = 6, type = "pos", tab = "") 
p7 <- ggplot.func(data.i = mgwr.df, i = 7, type = "pos", tab = "") 
p8 <- ggplot.func(data.i = mgwr.df, i = 8, type = "pos", tab = "") 
p9 <- ggplot.func(data.i = mgwr.df, i = 9, type = "pos", tab = "") 
p10 <- ggplot.func(data.i = mgwr.df, i = 10, type = "pos", tab = "") 
p11 <- ggplot.func(data.i = mgwr.df, i = 11, type = "pos", tab = "") 
p12 <- ggplot.func(data.i = mgwr.df, i = 12, type = "pos", tab = "")
p13 <- ggplot.func(data.i = mgwr.df, i = 13, type = "pos", tab = "")
p14 <- ggplot.func(data.i = mgwr.df, i = 14, type = "pos", tab = "")

png(filename = "LCM.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot2(list(p5, p10, p6, p11, p7, p12, p8, p13, p9, p14), cols = 5)
dev.off()

