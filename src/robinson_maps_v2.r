

figures_path = "figures/"
clean_data_path = "cleandata/"
ratings_path = "output/"

acc <- read.csv(paste0(clean_data_path, "country_level_accuracy.csv"), header = TRUE)
RCP8.5_80 <- read.csv(paste0(ratings_path, "Kahn_8.5_2100_estimates.csv"))
RCP2.6_80 <- read.csv(paste0(ratings_path, "Kahn_2.6_2100_estimates.csv"))

RCP8.5_80$X <- NULL
RCP2.6_80$X <- NULL
acc$X <- NULL

acc <- dplyr::select(acc, country, ISO2, actual, est, notch)

colnames(acc) <- c( "country","id","actual","est","notch")
colnames(RCP8.5_80) <- c("country","id","actual", "est","est_lower","est_upper")
colnames(RCP2.6_80) <- c("country","id","actual", "est","est_lower","est_upper")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_countries_rob  <- spTransform(NE_countries, CRSobj = PROJ)
NE_graticules_rob <- spTransform(NE_graticules, CRSobj = PROJ)
NE_box_rob        <- spTransform(NE_box, CRSobj = PROJ)

prj.coord <- project(cbind(lbl.Y$lon, lbl.Y$lat), proj=PROJ)
lbl.Y.prj <- cbind(prj.coord, lbl.Y)
names(lbl.Y.prj)[1:2] <- c("X.prj","Y.prj")

prj.coord <- project(cbind(lbl.X$lon, lbl.X$lat), proj=PROJ)
lbl.X.prj <- cbind(prj.coord, lbl.X)
names(lbl.X.prj)[1:2] <- c("X.prj","Y.prj")

NE_countries_rob_fort <- tidy(NE_countries_rob, region="name")
NE_countries_rob_fort$id <- parse_country(NE_countries_rob_fort$id)

RCP8.5_80$regrade <- round(RCP8.5_80$est - RCP8.5_80$actual)
RCP2.6_80$regrade <- round(RCP2.6_80$est - RCP2.6_80$actual)

map_1 <- full_join(RCP8.5_80, NE_countries_rob_fort, 
	by=c("id"))
	
map_2 <- full_join(RCP2.6_80, NE_countries_rob_fort, 
	by=c("id"))
	
map_3 <- full_join(acc, NE_countries_rob_fort, 
	by=c("id"))
	
map_1 <- dplyr::filter(map_1, id!="AQ")
map_2 <- dplyr::filter(map_2, id!="AQ")
map_3 <- dplyr::filter(map_3, id!="AQ")

#pretty_breaks <- c(-6,-4,-2,-1,0,1,2)

x_pal <- c("#73100b", "#bd261d", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", 
	"#d9ef8b", "#a6d96a", "#66bd63", "#1a9850")

pretty_breaks <- c(-6,-4,-2,-1,0,1)
minVal <- min(map_1$regrade, na.rm=T)
maxVal <- max(map_1$regrade, na.rm=T)
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

addBreaks <- function(dataframe){
  dataframe$brks <- cut(dataframe$regrade,
  breaks = brks,
  include.lowest=TRUE,
  labels=labels)
  return (dataframe)}

map_1 <- addBreaks(map_1)
#map_2 <- addBreaks(map_2)
	
brks_scale <- levels(map_1$brks)
labels_scale <- brks_scale
n <- length(brks_scale)


# __________ Plot layers
q <- ggplot() +
    geom_polygon(data=map_1, aes(fill=brks, x = long,y = lat, group=group), colour="black", size = 0.25, alpha=0.9) +
    geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
    geom_path(data=NE_graticules_rob, aes(long, lat, group=group), linetype="dotted", color="grey50", size = 0.25) +
    coord_fixed(ratio = 1) +
    theme_void() + 
      scale_fill_manual(
	breaks = (brks_scale),
	name = "Notch change",
	labels = c(labels_scale),
	values = ((x_pal[1:n])),
	na.value = "white",
	drop = FALSE,
	guide = guide_legend( 
		direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70/length(labels), units = "mm"),
        title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
        nrow = 1,
        byrow = T,
        label.position = "bottom")) +
     labs(
    title = "",
    subtitle = "") +  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
   panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color=NA),
    plot.title = element_text(size= 22, hjust=0.01, 
		color = "#4e4d47", 
		margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, 
		hjust=0.01, 
		color = "#4e4d47", 
		margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, 
		color = "#4e4d47", 
		margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = "bottom")
save_plot(paste0(figures_path,"fig13.jpg"), 
	fig=ggplot2::last_plot(), 
	width=17, 
	height=12,
	dpi=300,
	label.size = 1.4, 
	axis.textsize = 0.6, 
	axis.titlesize = 0.55,
	legend.textsize = 0.4, 
	legend.titlesize = 0.45, 
	legend.itemsize = 0.3)


pretty_breaks <- c(-6,-4,-2,-1,0,1)

minVal <- min(map_2$regrade, na.rm=T)
maxVal <- max(map_2$regrade, na.rm=T)
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

map_2 <- addBreaks(map_2)
#map_2 <- addBreaks(map_2)
	
brks_scale <- levels(map_2$brks)
labels_scale <- brks_scale
n <- length(brks_scale)






# __________ Plot layers
r <- ggplot() +
    geom_polygon(data=map_2, aes(fill=brks, x = long,y = lat, group=group), colour="black", size = 0.25, alpha=0.9) +
    geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
    geom_path(data=NE_graticules_rob, aes(long, lat, group=group), linetype="dotted", color="grey50", size = 0.25) +
    coord_fixed(ratio = 1) +
    theme_void() + 
      scale_fill_manual(
	breaks = (brks_scale),
	name = "Notch change",
	labels = c(labels_scale),
	values = ((x_pal[1:n])),
	na.value = "white",
	drop = FALSE,
	guide = guide_legend( 
		direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70/length(labels), units = "mm"),
        title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
        nrow = 1,
        byrow = T,
        label.position = "bottom")) +
     labs(
    title = "",
    subtitle = "") +
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
   panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color=NA),
   #legend.box.background =  element_rect(colour = "black"),

    plot.title = element_text(size= 22, hjust=0.01, 
		color = "#4e4d47", 
		margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, 
		hjust=0.01, 
		color = "#4e4d47", 
		margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, 
		color = "#4e4d47", 
		margin = margin(b = 0.3, r=-99, unit = "cm") ),

   # legend.position = c(0.8, 0.09))
    legend.position = "bottom")
#extendLegendWithExtremes(p)
r


save_plot(paste0(figures_path, "fig12.jpg"), 
	fig=ggplot2::last_plot(), 
	width=17, 
	height=12,
	dpi=300,
	label.size = 1.4, 
	axis.textsize = 0.6, 
	axis.titlesize = 0.55,
	legend.textsize = 0.4, 
	legend.titlesize = 0.45, 
	legend.itemsize = 0.3)



pretty_breaks <- c(1,2,3,4)

minVal <- min(map_3$notch, na.rm=T)
maxVal <- max(map_3$notch, na.rm=T)

labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

map_3$brks <- cut(map_3$notch, 
	breaks = brks,
	include.lowest=TRUE,
	labels = labels)
	
brks_scale <- levels(map_3$brks)
labels_scale <- brks_scale

x_pal <- c("#73100b", "#bd261d", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", 
	"#d9ef8b", "#a6d96a", "#66bd63", "#1a9850")



# __________ Plot layers
s <- ggplot() +
    # add Natural Earth countries projected to Robinson, give black border and fill with gray
    geom_polygon(data=map_3, aes(fill=as.factor(notch), x = long,y = lat, group=group), colour="black", size = 0.25, alpha=0.9) +
    # Note: "Regions defined for each Polygons" warning has to do with fortify transformation. Might get deprecated in future!
    # alternatively, use use map_data(NE_countries) to transform to data frame and then use project() to change to desired projection.
    # add Natural Earth box projected to Robinson
    geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
    # add graticules projected to Robinson
    geom_path(data=NE_graticules_rob, aes(long, lat, group=group), linetype="dotted", color="grey50", size = 0.25) +
    # add graticule labels - latitude and longitude
    #geom_text(data = lbl.Y.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
    #geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
    # the default, ratio = 1 in coord_fixed ensures that one unit on the x-axis is the same length as one unit on the y-axis
    coord_fixed(ratio = 1) +
    # remove the background and default gridlines
    theme_void() + 
      scale_fill_manual(
	#breaks = (brks_scale),
	#breaks = c("3", "2", "1", "0"),
	name = "Model precision",
	#labels = c(labels_scale),
	#labels = c("0", "1", "2", "3"),
	#labels = c(">-5", "-4", "-2", "-1", "0", "1", "2", "3"),
	#values = (desaturate(magma(11, alpha = 0.8)[0:11])),
	values = (rev(x_pal[4:10])),
	na.value = "white",
	#values = (desaturate(magma(7, alpha = 0.8))),
	drop = FALSE,
	guide = guide_legend( 
		direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70/length(labels), units = "mm"),
        title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
        nrow = 1,
        #reverse = T,
        byrow = T,
        label.position = "bottom")) +
     labs(
    title = "",
    subtitle = "") +
    #caption = "The range of notch changes is from -8 to +3, the legend indicates intervals") +
  
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
   panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color=NA),
   #legend.box.background =  element_rect(colour = "black"),

    plot.title = element_text(size= 22, hjust=0.01, 
		color = "#4e4d47", 
		margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, 
		hjust=0.01, 
		color = "#4e4d47", 
		margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, 
		color = "#4e4d47", 
		margin = margin(b = 0.3, r=-99, unit = "cm") ),

   # legend.position = c(0.8, 0.09))
    legend.position = "bottom")
#extendLegendWithExtremes(p)

save_plot(paste0(figures_path,"fig8.jpg"), 
	fig=ggplot2::last_plot(), 
	width=17, 
	height=12,
	dpi=300,
	label.size = 1.4, 
	axis.textsize = 0.6, 
	axis.titlesize = 0.55,
	legend.textsize = 0.4, 
	legend.titlesize = 0.45, 
	legend.itemsize = 0.3)

