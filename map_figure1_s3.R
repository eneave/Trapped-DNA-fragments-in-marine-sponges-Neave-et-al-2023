#################################################################################
## Code for Map; Figure 1A, 1B, 1C, 1D 
## 
#################################################################################

## set working directory
setwd("C:/Users/BESENEAV/OneDrive - Liverpool John Moores University/PhD/North Atlantic project/map")

## load packages; install first if necessary
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggplot2)
library(ggrepel)
library(cowplot)

## Run 'preparing_metadata_for_plots.R' script to generate metadata used for maps
## Read in metadata for map
mapdata2 <- read.csv("C:/Users/BESENEAV/OneDrive - Liverpool John Moores University/PhD/North Atlantic project/map/mapdata2_edit.csv")


## Download shape files for map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

## Set depth range as a factor
mdata$depth_range <- factor(mdata$depth_range,
                            levels = c("80-200", "200-500", "500-800", 
                                       "800-1200", "1200-1600", ">1600"))

## subset metadata for map by removing samples from other projects, 
## Gg species, and control samples
#mapdata <- mdata[-c(1:4,26,27,45:53),]

## Set theme for maps
theme_set(theme_bw())

## Figure 1A

p3 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgrey") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1, "in"), pad_y = unit(0.75, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = mapdata2, aes(x=lon_dd, y=lat_dd, fill = bioregion_costello, shape = species), size = 3.5, alpha =0.7) +
  scale_fill_manual(values = c("North American Boreal" = "#D55E00","NE Atlantic" = "#009E73",
                               "Norwegian Sea" = "#CC79A7", "Arctic Seas" = "#CC79A7")) +
  scale_shape_manual(values = c("Gh" = 22, "Gb" = 24, "Pv" = 21), name = "Species") +
  annotate(geom = "text", x = -35, y = 52, label = "North Atlantic", 
           fontface = "italic", color = "grey22", size = 5) +
  coord_sf(xlim = c(-75, 45), ylim = c(45, 85), expand = FALSE) +
  annotate(geom = "rect", xmin = -75, xmax = 0,   ymin = 60, ymax = 75,   
           fill = "black", alpha = 0.1) +  # North American Boreal
  annotate(geom = "text", x = -70, y = 73.5, label = "C", 
           fontface = "bold", color = "#D55E00", size = 6) +
  annotate(geom = "rect", xmin = -20, xmax = 22,   ymin = 53, ymax = 66,   
           fill = "black", alpha = 0.1) +  # NE Atlantic
  annotate(geom = "text", x = 3, y = 64.5, label = "B", 
           fontface = "bold", color = "#009E73", size = 6) +
  annotate(geom = "rect", xmin = 12, xmax = 40,   ymin = 64, ymax = 84,   
           fill = "black", alpha = 0.1) + # Arctic and Norwegian Seas
  annotate(geom = "text", x = 17, y = 82.5, label = "D", 
           fontface = "bold", color = "#CC79A7", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") 


## Figure 1C North American Boreal

mapdata2_nab <- subset(mapdata2, bioregion_costello == "North American Boreal")

#sort out position for the jitter
pos <- position_jitter(width = 1, height = 1, seed = 2)

pnab3 <- ggplot(data = world) +
  geom_sf(color = "#D55E00", fill = "lightgrey") +
  geom_jitter(data = mapdata2_nab, aes(x=lon_dd, y=lat_dd, shape = species, fill = depth_avg), 
              size = 3.5, alpha = 0.8, position = pos, show.legend = FALSE) +
  geom_text_repel(data = mapdata2_nab, aes(x=lon_dd, y=lat_dd, label = factor(X)), position = pos, max.overlaps = 23) +
  scale_fill_gradientn(colours = c("#fde725", "#7ad151", "#22a884", "#2a788e", "#414487", "#440154"),
                       limits = c(0,1900), breaks = c(0, 200, 500, 800, 1200, 1600, 1900), 
                       guide = guide_colourbar(reverse = TRUE)) +
  scale_shape_manual(values = c("Gh" = 22, "Gb" = 24)) + 
  coord_sf(xlim = c(-75, 0), ylim = c(60, 75), expand = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(colour=c("#D55E00")),
        axis.line.y = element_line(colour=c("#D55E00")),
        legend.position = "none") 

## Figure 1B Northeast Atlantic

mapdata2_ne <- subset(mapdata2, bioregion_costello == "NE Atlantic")

pne3 <- ggplot(data = world) +
  geom_sf(color = "#009E73", fill = "lightgrey") +
  geom_jitter(data = mapdata2_ne, aes(x=lon_dd, y=lat_dd, shape = species, fill = depth_avg), 
              size = 3.5, alpha = 0.8, position = pos, show.legend = FALSE) +
  geom_text_repel(data = mapdata2_ne, aes(x=lon_dd, y=lat_dd, label = factor(X)), position = pos, max.overlaps = 23) +
  scale_fill_gradientn(colours = c("#fde725", "#7ad151", "#22a884", "#2a788e", "#414487", "#440154"),
                       limits = c(0,1900), breaks = c(0, 200, 500, 800, 1200, 1600, 1900), 
                       guide = guide_colourbar(reverse = TRUE)) +
  scale_shape_manual(values = c("Gb" = 24, "Pv" = 21)) +
  coord_sf(xlim = c(-20, 22), ylim = c(53, 66), expand= FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(colour=c("#009E73")),
        axis.line.y = element_line(colour=c("#009E73")),
        legend.position = "none") 

## Figure 1D Norwegian and Arctic Seas

mapdata2_a <- subset(mapdata2, bioregion_costello == "Norwegian Sea" | bioregion_costello == "Arctic Seas")

pa3 <- ggplot(data = world) +
  geom_sf(color = "#CC79A7", fill = "lightgrey") +
  geom_jitter(data = mapdata2_a, aes(x=lon_dd, y=lat_dd, shape = species, fill = depth_avg), 
              size = 3.5, alpha = 0.8, position = pos) +
  geom_text_repel(data = mapdata2_a, aes(x=lon_dd, y=lat_dd, label = factor(X)), position = pos, max.overlaps = 23) +
  scale_fill_gradientn(colours = c("#fde725", "#7ad151", "#22a884", "#2a788e", "#414487", "#440154"),
                       limits = c(0,1900), breaks = c(0, 200, 500, 800, 1200, 1600, 1900), 
                       guide = guide_colourbar(reverse = TRUE, barwidth = 0.3, barheight = 15),
                       name = "Depth (m)") +
  scale_shape_manual(values = c("Gb" = 24,"Gh" = 22, "Pv" = 21),
                     labels = c(expression(italic("Geodia barretti"),italic("Geodia henstcheli"),italic("Phakellia ventilabrum"))),
                     name = "Species") +
  coord_sf(xlim = c(12, 40), ylim = c(64, 84), expand = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(colour=c("#CC79A7")),
        axis.line.y = element_line(colour=c("#CC79A7")),
        legend.direction = "vertical", legend.box = "horizontal",
        legend.title = element_text(size = 12, colour = "black", face = "bold"), 
        legend.text = element_text(size = 11, face ="bold", colour ="black")) 

## Organize the four maps into one figure

# not quite right; missing black outline; legend is messed up
final_map <-
  plot_grid(p3, pne3, pnab3, pa3, labels = c("A", "B", "C", "D"))

ggsave(filename=c("C:/Users/beseneav/OneDrive - Liverpool John Moores University/PhD/North Atlantic project/map/fig1_final_forgithub.tiff"), 
       plot = final_map, width = 11.2, height = 8.5, dpi = 1000, units = "in")


windows()
final_map
