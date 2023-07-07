############################################################################
## NMDS, accumulation curves, and associated stats Figures 2A, 2B, 2C, 2D ##
############################################################################

#####
## Prepare data for NMDS
#####

## remove any contaminiation and positive controls
motust <- subset(motus, contam == "n")

## subset fish observations
motust_f <- subset(motust, common_group == "Fish")
rm(motust)

## subset species level observations
species_f <- subset(motust_f, rank == "species")
species_f <- species_f[c(1:67),]

## prepare for data transformation
species_ft <- species_f[,-c(44,76,24,28,57,66,75,78,90,47,1:19,94:103,26,48,53,56,58,60,62,89,92,93)] # just samples (get rid of other vars, controls and samples w/ reads too low) n=54

## transform into long dataframe
fish_species_long <- t(species_ft)

## create second dataframe for presence absence
fish_species_longpa <- ifelse(fish_species_long == 0, 0, 1)

##remove extra dataframes and clean up environment
rm(motust_f,species_f,species_ft)

#####
## Create distance matrix (bray and jaccard) and NMDS
#####

library(vegan)

## Hellinger's standardization
nmds.dat.fish.hell <- decostand(fish_species_long, method = "hellinger")

## calculate Bray-Curtis dissimilarity matrix
bray.fish.dist <- vegdist(nmds.dat.fish.hell, method = "bray")

## Calculate jaccard dissimilariy matrix
jaccard.fish.dist <- vegdist(fish_species_longpa, method = "jaccard", binary =  TRUE)

## NMDS

set.seed(1995)

nmds.ord.fishsp <- metaMDS(bray.fish.dist, distance = "bray", trymax = 1000)

## try NMDS again but with 3 dimensions

nmds3.ord.fishsp <- metaMDS(bray.fish.dist, k=3, distance = "bray", trymax = 1000)

## NMDS with jaccard distances

set.seed(0326)

nmds.jaccard.ord.fishsp <- metaMDS(jaccard.fish.dist, distance = "jaccard", trymax = 1000)

## extract site scores from NMDS and add metadata
data.scores.fishsp <- as.data.frame(scores(nmds.ord.fishsp))  
data.scores.fishsp$long_id <- rownames(data.scores.fishsp)
nmds_fishsp <- merge(data.scores.fishsp, mdata_dates, by.x = "long_id")

data.scores.3.fishsp <- as.data.frame(scores(nmds3.ord.fishsp))  
data.scores.3.fishsp$long_id <- rownames(data.scores.3.fishsp)
nmds_3_fishsp <- merge(data.scores.3.fishsp, mdata_dates, by.x = "long_id")

data.scores.pafishsp <- as.data.frame(scores(nmds.jaccard.ord.fishsp))  
data.scores.pafishsp$long_id <- rownames(data.scores.pafishsp)
nmds_pafishsp <- merge(data.scores.pafishsp, mdata_dates, by.x = "long_id")

## prepare data for plotting

nmds_fishsp$depth_range <- factor(nmds_fishsp$depth_range,
                                  levels = c("80-200", "200-500", "500-800", "800-1200", "1200-1600", ">1600"))

nmds_3_fishsp$depth_range <- factor(nmds_3_fishsp$depth_range,
                                    levels = c("80-200", "200-500", "500-800", "800-1200", "1200-1600", ">1600"))

nmds_pafishsp$depth_range <- factor(nmds_pafishsp$depth_range,
                                    levels = c("80-200", "200-500", "500-800", "800-1200", "1200-1600", ">1600"))

accum.depthrange.long$Grouping <- factor(accum.depthrange.long$Grouping,
                                  levels = c("80-200", "200-500", "500-800", "800-1200", "1200-1600", ">1600"))

nmds_fishsp$bioregion_nmds <- ifelse(nmds_fishsp$bioregion_costello== "Norwegian Sea", "Norwegian and Arctic Seas",
                                     ifelse(nmds_fishsp$bioregion_costello== "Arctic Seas", "Norwegian and Arctic Seas",
                                            ifelse(nmds_fishsp$bioregion_costello== "North American Boreal", "North American Boreal",
                                                   ifelse(nmds_fishsp$bioregion_costello== "NE Atlantic", "NE Atlantic", NA))))
nmds_fishsp$bioregion_nmds <- factor(nmds_fishsp$bioregion_nmds,
                                     levels = c("North American Boreal", "NE Atlantic", "Norwegian and Arctic Seas"))

nmds_3_fishsp$bioregion_nmds <- ifelse(nmds_3_fishsp$bioregion_costello== "Norwegian Sea", "Norwegian and Arctic Seas",
                                       ifelse(nmds_3_fishsp$bioregion_costello== "Arctic Seas", "Norwegian and Arctic Seas",
                                              ifelse(nmds_3_fishsp$bioregion_costello== "North American Boreal", "North American Boreal",
                                                     ifelse(nmds_3_fishsp$bioregion_costello== "NE Atlantic", "NE Atlantic", NA))))
nmds_3_fishsp$bioregion_nmds <- factor(nmds_3_fishsp$bioregion_nmds,
                                       levels = c("North American Boreal", "NE Atlantic", "Norwegian and Arctic Seas"))

nmds_pafishsp$bioregion_nmds <- ifelse(nmds_pafishsp$bioregion_costello== "Norwegian Sea", "Norwegian and Arctic Seas",
                                       ifelse(nmds_pafishsp$bioregion_costello== "Arctic Seas", "Norwegian and Arctic Seas",
                                              ifelse(nmds_pafishsp$bioregion_costello== "North American Boreal", "North American Boreal",
                                                     ifelse(nmds_pafishsp$bioregion_costello== "NE Atlantic", "NE Atlantic", NA))))
nmds_pafishsp$bioregion_nmds <- factor(nmds_pafishsp$bioregion_nmds,
                                       levels = c("North American Boreal", "NE Atlantic", "Norwegian and Arctic Seas"))

## can remove nmds with 3 dimensions
rm(nmds_3_fishsp, nmds3.ord.fishsp, data.scores.3.fishsp) ## plotted and won't be used in final plots

#####
## Calculate species richness from the jaccard matrix to use to scale site points
#####

site.names <- rownames(fish_species_longpa)

richness.fishsp <- NULL
richness.fishsp$long_id <- site.names
richness.fishsp$richness = rowSums(fish_species_longpa[,c(1:54)])

fig2nmds <- merge(nmds_pafishsp, richness.fishsp, by = c("long_id"))

## curious if you plot NMDS1 vs. depth.. vs. richness

ggplot(nmds_pafishsp, aes(x = NMDS1, y = depth_avg)) +
  geom_point(aes(color = bioregion_nmds)) 


ggplot(fig2nmds, aes(x = NMDS1, y = richness)) +
  geom_point(aes(color = bioregion_nmds))

#####
## Calculate depth contours for jaccards matrix
#####

## adding an ordisurf depth layer in ggplot
# https://chrischizinski.github.io/rstats/ordisurf/

plot(nmds.jaccard.ord.fishsp, type="p")
plot(env_pafishsp, p.max=0.05, col="red")
ordisurf(nmds.jaccard.ord.fishsp, nmds_fishsp$depth_avg, add = TRUE, col="blue")

ordireorder <- rownames(fish_species_longpa)

library(dplyr)

fig2nmds <- fig2nmds %>%
  slice(match(ordireorder, long_id))

depthpa.sf <- ordisurf(nmds.jaccard.ord.fishsp ~ fig2nmds$depth_avg, plot = FALSE, scaling = 3)

extract.xyz <- function(obj) {
  xy <- expand.grid(x = obj$grid$x, y = obj$grid$y)
  xyz <- cbind(xy, c(obj$grid$z))
  names(xyz) <- c("x", "y", "z")
  return(xyz)
}

depth.contour.vals <- extract.xyz(obj = depthpa.sf)

#####
## Use adonis to calculate the significance of the groupings
#####

library(tidyverse)
library(vegan)

# use fig2nmds (which has metadata in it); matches the order of the distance matrix;
# from reordering done to produce the ordisurf contours
# use jaccard.fish.dist for distance matrix

## prepare data

jaccard_spmatrix <- as.tibble(as.matrix(jaccard.fish.dist))
jaccard_spmatrix$long_id <- NULL
jaccard_spmatrix$long_id <- fig2nmds$long_id

jaccard_spmatrix[["long_id"]]

jaccard_spmatrix <- jaccard_spmatrix %>%
  select(long_id, everything())

fig2meta_jdist <- inner_join(fig2nmds, jaccard_spmatrix, by="long_id")

all_dist <- fig2meta_jdist %>%
  select(all_of(.[["long_id"]])) %>%
  as.dist()

set.seed(105)

all_test <- adonis(all_dist~bioregion_nmds*species, 
                   data=fig2meta_jdist, 
                   permutations=1000)

all_test_final <- adonis(all_dist~bioregion_nmds,
                         data=fig2meta_jdist,
                         permutations=1000)

all_p <- all_test[["aov.tab"]][["Pr(>F)"]]

pairwise_p <- numeric()


## Pairwise comparisons of bioregions

## Northeast Atlantic vs North American Boreal

nea_nab <- fig2meta_jdist %>%
  filter(bioregion_nmds == "NE Atlantic" | bioregion_nmds == "North American Boreal") 

nea_nab_dist <- nea_nab %>%
  select(all_of(.[["long_id"]])) %>%
  as.dist()

nea_nab_test <- adonis(nea_nab_dist~bioregion_nmds, 
                       data=nea_nab, 
                       permutations=1000)

pairwise_p["nea_nab"] <- nea_nab_test[["aov.tab"]][["Pr(>F)"]][1]

## Northeast Atlantic vs Norwegian and Arctic seas

nea_a <- fig2meta_jdist %>%
  filter(bioregion_nmds == "NE Atlantic" | bioregion_nmds == "Norwegian and Arctic Seas") 

nea_a_dist <- nea_a %>%
  select(all_of(.[["long_id"]])) %>%
  as.dist()

nea_a_test <- adonis(nea_a_dist~bioregion_nmds, 
                     data=nea_a, 
                     permutations=1000)

pairwise_p["nea_a"] <- nea_a_test[["aov.tab"]][["Pr(>F)"]][1]

## North American Boreal vs Norwegian and Arctic seas

nab_a <- fig2meta_jdist %>%
  filter(bioregion_nmds == "North American Boreal" | bioregion_nmds == "Norwegian and Arctic Seas") 

nab_a_dist <- nab_a %>%
  select(all_of(.[["long_id"]])) %>%
  as.dist()

nab_a_test <- adonis(nab_a_dist~bioregion_nmds, 
                     data=nab_a, 
                     permutations=1000)

pairwise_p["nab_a"] <- nab_a_test[["aov.tab"]][["Pr(>F)"]][1]

## need to adjust p values for doing pairwise comparisons of everything

p.adjust(pairwise_p, method="BH") #Benjamini & Hochberg (1995)

#####
## Final figure 2 code
#####

library(ggtext)

fig2a_legend <- tibble(x=c(-0.2, -0.1, 0.4),
                       y=c(0.5, -0.55, 0.55),
                       label=c("Northeast Atlantic",
                               "Norwegian-Arctic <br> Seas",
                               "North-American <br> Boreal"))

f3a <-
  ggplot() + 
  stat_contour(data = depth.contour.vals, aes(x=x, y=y, z = z, colour= (..level..)), binwidth = 20) +
  geom_point(data = fig2nmds, aes(x = NMDS1, y = NMDS2, fill = bioregion_nmds, size = richness), 
             colour = c("black"), shape = 21, alpha=0.8)+ 
  geom_richtext(data = fig2a_legend, aes(x=x, y=y, label=label), colour = c("#009E73", "#CC79A7", "#D55E00"), size = 6.5) +
  annotate(geom = "text", x = 0.5, y= -0.6, label = "stress = 0.1817",size = 5,colour ="black")+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 12, colour = "black", face = "bold"), 
        legend.text = element_text(size = 11, face ="bold", colour ="black"),
        legend.position = c(0.1,0.5),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key=element_blank(), #hides grey background in legend
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) + 
  labs(x = "NMDS1", colour = "Depth range (m)", y = "NMDS2", size = "Richness")  + 
  labs(x = "NMDS1", y = "NMDS2", fill="Region", colour="Depth (m)") +
  scale_fill_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  scale_colour_gradientn(colours = c("#fde725", "#7ad151", "#22a884", "#2a788e", "#414487", "#440154"),
                         limits = c(0,1900), breaks = c(0, 200, 500, 800, 1200, 1600, 1900), 
                         guide = guide_colourbar(reverse = TRUE, barwidth = 0.3, barheight = 15),
                         name = "Depth (m)") +
  guides(fill = "none", size = "legend") +
  scale_size(range=c(0,8), 
             breaks = c(1,10,20,29), 
             labels = c(1,10,20,30), 
             limits = c(0,NA)) +
  xlim(-0.65,0.65) +
  ylim(-0.65,0.65)

## gamma diversity by plotting species accumulation curves

library(BiodiversityR)

accum.depthrange <- accumcomp(fish_species_long, y = fig2nmds, factor = "depth_range", plotit = F)

accum.depthrange.long <- accumcomp.long(accum.depthrange, ci=NA, label.freq=5)
accum.depthrange.long$uperror <- accum.depthrange.long$Richness + accum.depthrange.long$SD
accum.depthrange.long$lowerror <- accum.depthrange.long$Richness - accum.depthrange.long$SD
View(accum.depthrange.long)

f3b <- 
  ggplot(data=accum.depthrange.long, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(expand=c(0, 0), limits = c(0, 18), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(expand=c(0, 0), limits = c(0, 55), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=2) +
  geom_point( aes(colour=Grouping), size=4, alpha=0.9) +
  geom_ribbon(aes(colour=Grouping, fill=after_scale(alpha(colour, alpha=0.2)), ymax = uperror, ymin = lowerror), 
              show.legend=FALSE) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = c(.99, .95),
        legend.justification = c("right", "top"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key=element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +
        scale_color_manual(values = c("80-200" = "#fde725", "200-500" = "#7ad151","500-800" = "#22a884","800-1200" = "#2a788e",
                                "1200-1600" = "#414487",">1600" = "#440154")) +
  guides(colour = "legend") +
  labs(x = "No. of Sponge samples", y = "No. of Fish Species", colour = "Depth\nRange (m)") 


library(ggtext)
fig2c_legend <- tibble(x=c(-0.2, 0.2),
                       y=c(-0.3, -0.3),
                       label=c("No MPA","MPA"))

f3c<-
  ggplot(nmds_pa_NE.fishsp, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape = region), alpha=0.85)+ 
  geom_point(size = 4, aes(shape = region, colour=mpa2), alpha=0.85, show.legend = F)+ 
  geom_richtext(data = fig2c_legend, aes(x=x, y=y, label=label), colour = c("#00BFC4","#F8766D"), size = 6.5) +
  annotate(geom = "text", x = -0.1, y= 0.35, label = "stress = 0.1711",size = 5,colour ="black")+
  coord_cartesian(xlim = c(-0.4,0.4), ylim = c(-0.4,0.4)) +
  #guides(shape=guide_legend(ncol=3,nrow=3,byrow=TRUE)) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key =  element_blank(),
        legend.key.width = unit(0.5, "in")) + 
  labs(x = "NMDS1", y = "NMDS2", shape = "Aggregation") +
  scale_shape_manual(values=c(19,15,18,17,21,22,5), 
                     labels=c("Faroe Shetland\nSponge Belt", "North of Shetland",
                              "Norway", "Rockall Bank", "Shetland Shelf",
                              "Sula Reef","Sweden")) 

## Make an accumulation curve for MPAs

accum.mpa <- accumcomp(abund.pv, y = pc.pv, factor = "mpa2", plotit = F)

accum.mpa.long <- accumcomp.long(accum.mpa, ci=NA, label.freq=2)
accum.mpa.long$uperror <- accum.mpa.long$Richness + accum.mpa.long$SD
accum.mpa.long$lowerror <- accum.mpa.long$Richness - accum.mpa.long$SD

f3d <-
  ggplot(data=accum.mpa.long, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=2) +
  geom_point( aes(colour=Grouping), size=4, alpha=0.9) +
  geom_ribbon(aes(colour=Grouping, fill=after_scale(alpha(colour, alpha=0.2)), ymax = uperror, ymin = lowerror), 
              show.legend=FALSE) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = c(.97, .05),
        legend.justification = c("right", "bottom"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key=element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +
  scale_color_manual(values = c("MPA" = "#F8766D", 
                                "No MPA" = "#00BFC4")) +
  labs(x = "No. of Sponge samples", y = "No. of Fish Species", colour = "Status")

## assemble figure

library(cowplot)

bottom_panel <-
  plot_grid(f3c, f3d, labels = c("C", "D"), ncol = 2, rel_widths = c(1.8,1), align = c("v"))


figure2 <-
  plot_grid(f3a, f3b, bottom_panel, labels = c("A", "B", ""), ncol = 1, rel_heights = c(2,1.1,1.3))


ggsave(filename=c("C:/your_directory_here/fig2_nmds.jpg"), 
       plot = figure2, width = 8, height = 14, dpi = 1000, units = "in")


