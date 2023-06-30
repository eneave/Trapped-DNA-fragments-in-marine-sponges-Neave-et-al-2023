#################################################################################
## Code for Bubble Plot; Figure 3
## 
#################################################################################

## Load packages
library(tidyverse)
library(ggplot2)

#####
## Prepare long dataframes and plot data
#####

## Long dataframe including the common group
## keep relevant taxonomic information

motust <- tibble(motus[,c(1,8,10,12,14:16,20:97,101:103)]) 

## remove rows with zero reads, left over from clarion samples which were removed before recalculating total reads

motust <- motust[c(1:519),]

## remove control samples (5 positive, 5 negative)

motust <- motust[,-c(36,44,46,77,80,14,41,48,50,81)]

## Also remove sites that were not used due to having < 100 fish reads

motust_100 <- subset(motust, select=-c(Gh_KS_197.1_s_57,Gh_KS_200.5_s_58,Gb_BS_imp_18R6_s_38,Gh_Vst_045TVG_011B_i_62,
                                       Gh_Ice_ME85_3_1132_A_s_32,Gh_Vst_019TVG_003B_d_90,Gb_Sw_15O5_s_21,
                                       Gh_Vst_011TVG_004B_d_92,Gh_Vst_045TVG_005B_i_61,Gh_Vst_045TVG_012B_i_64))

## replace NAs with unassigned

motust_100$scientific_name[is.na(motust_100$scientific_name)] <- "Unassigned" 

## subset to keep reads that aren't contamination

motustf_100 <- subset(motust_100, contam == "n")

## Create long version of data frame

long.df <- motustf_100 %>%
  gather(key = "long_id", value = "reads", 
         -sequence,
         -order,
         -family,
         -genus,
         -Common.name,
         -species,
         -S.id,
         -scientific_name,
         -rank,
         -best_identity,
         -common_group,
         -biome,
         -contam)

long.df <- long.df[c(1:7020),]

## Add reads calculations

long.df$log_reads <- log(long.df$reads)
long.df$sqrt_reads <- sqrt(long.df$reads)

## Calculate proportional read counts per location 
readcounts <- as.data.frame(colSums(motustf_100[,c(8:62)]))
readcounts$long_id <- rownames(readcounts)
long.df <- merge(long.df, readcounts, by.x = "long_id")

names(long.df)[18] <- "samplereads"

long.df$PRC <- (long.df$reads/long.df$samplereads)*100

## combine mdata and dates

mdata_dates <- merge(mdata, dates, by=c("long_id"), all.x = FALSE)

## Add nmds regions to mdata

mdata_dates$bioregion_nmds <- ifelse(mdata_dates$bioregion_costello== "Norwegian Sea", "Norwegian and Arctic Seas",
                                     ifelse(mdata_dates$bioregion_costello== "Arctic Seas", "Norwegian and Arctic Seas",
                                            ifelse(mdata_dates$bioregion_costello== "North American Boreal", "North American Boreal",
                                                   ifelse(mdata_dates$bioregion_costello== "NE Atlantic", "NE Atlantic", NA))))

## Add metadata to the tibble

motust_100_long <- merge(long.df, mdata_dates, by=c("long_id"), all.x = TRUE) 

## add depth range info
motust_100_long$depth_range2 <- ifelse(motust_100_long$depth_range == "80-200", "80-500",
                                       ifelse(motust_100_long$depth_range == "200-500", "80-500", 
                                              ifelse(motust_100_long$depth_range == "500-800", "500-1200",
                                                     ifelse(motust_100_long$depth_range == "800-1200", "500-1200",
                                                            ifelse(motust_100_long$depth_range == "1200-1600", ">1200",
                                                                   ifelse(motust_100_long$depth_range == ">1600", ">1200", NA))))))

## Remove unnecessary data from the environment

rm(motust,long.df)

## Filter data for bubbleplot

## Subset desired fish reads 
fish_long <- subset(motust_100_long, common_group == "Fish") 
fish_long <- subset(fish_long, contam == "n")
fish_long2 <- fish_long
fish_long2$order[fish_long2$species.x == "Mullus surmuletus"] <- "Perciformes" ## wrong order; it needs to be assigned to order Perciformes
fish_long_species <- subset(fish_long2, rank == "species")  

fish_long_species_filtered_to50 <- subset(fish_long_species, PRC > 0)

## Subset desired mammal reads 
mammal_long <- subset(motust_100_long, common_group == "Mammal") 
mammal_long <- subset(mammal_long, contam == "n") 
mammal_long2 <- subset(mammal_long, biome == "marine")

mammal_long_species_filtered_to50 <- subset(mammal_long2, PRC > 0)

## Data for bubbleplot
figure1species <- rbind(fish_long_species_filtered_to50, mammal_long_species_filtered_to50)

## reorder order levels with fish first and sharks next, then mammals  
figure1species$order <- factor(figure1species$order,
                               levels = c("Anguilliformes","Argentiniformes","Aulopiformes",     
                                          "Beloniformes", "Caproiformes", "Carangiformes",    
                                          "Clupeiformes", "Gadiformes", "Labriformes", "Lophiiformes", 
                                          "Myctophiformes", "Notacanthiformes", 
                                          "Perciformes", "Pleuronectiformes",  "Scombriformes",  
                                          "Squaliformes", "Stomiiformes", "Zeiformes", 
                                          "Chimaeriformes", "Rajiformes", "Carcharhiniformes", # sharks 
                                          "Artiodactyla")) #mammals
## subset samples by biogeographic regions
NEA <- c("1.Gb","2.Gb","3.Gb","4.Gb","5.Gb","6.Gb","7.Gb","8.Pv","9.Pv",
         "10.Pv","11.Pv","12.Pv","13.Pv","14.Pv","15.Pv","16.Pv","17.Pv",
         "18.Pv","19.Pv","20.Pv","21.Pv","22.Pv","23.Pv","24.Pv","25.Pv",
         "26.Pv","27.Pv","28.Pv","29.Pv","30.Pv")
NAB <- c("31.Gb","32.Gb","33.Gb",
         "34.Gb","35.Gb","36.Gb","37.Gb","38.Gb","39.Gb","40.Gb","41.Gh",
         "42.Gh","43.Gh","44.Gh","45.Gh","46.Gh","47.Gh","48.Gh","49.Gh")
A <- c("50.Gb","51.Gb","52.Pv","53.Pv","54.Pv")

# New facet label names for region variable
region.labs <- c("NEA", "NAB", "NA")
names(region.labs) <- c("NE Atlantic", "North American Boreal", "Norwegian and Arctic Seas")


#####
## Bubbleplot code
#####

bp_final <-
  ggplot(figure1species, 
         aes(x = short_id, y = scientific_name, 
             fill = depth_avg, size = PRC)) + 
  scale_y_discrete(limits = rev) +
  geom_point(pch = 21) +
  scale_fill_gradientn(colours = c("#fde725", "#7ad151", "#22a884", "#2a788e", "#414487", "#440154"),
                       limits = c(0,1900), breaks = c(0, 200, 500, 800, 1200, 1600, 1900), 
                       guide = guide_colourbar(reverse = TRUE, barwidth = 1, barheight = 20), 
                       name = "Depth (m)") +
  scale_size_continuous(name = "Proportional \nread counts (%)",
                        range = c(1, 6),
                        limits = c(0,100),
                        breaks = c(0, 1, 10, 30, 50, 70),
                        labels = c("< 1%","1-10%","10-30%","30-50%","50%-70%", "> 70%")) + 
  labs(x = "", y ="") + 
  facet_grid(order ~ bioregion_nmds, scales = "free", space = "free", labeller = labeller(bioregion_nmds = region.labs)) +
  theme_light() +
  theme(strip.background.y = element_rect(fill= "lightgrey"),
        strip.background.x = element_rect(fill= "lightgrey"),
        strip.text.x= element_text(colour = "#000000"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"), 
        axis.ticks = element_line(colour = "#000000"),
        strip.text.y = element_text(colour = "#000000", angle = 360),
        axis.text.x = element_text(colour = "#000000", angle = 90, hjust = 1),
        axis.text.y = element_text(colour = "#000000", face = "italic"),
        text = element_text(size = 14),
        legend.direction = "vertical", 
        legend.box = "vertical") 


ggsave(filename="C:your_directory_here/figure4_final.tiff", 
       plot = bp_final, width = 13, height = 18, dpi = 900, units = "in")

