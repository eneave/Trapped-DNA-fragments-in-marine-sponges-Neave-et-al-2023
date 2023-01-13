#################################################################################
## Compiling and adding metadata for plotting and analysis
## Use after from_original_data_to_data_for_analysis_commented_s1.R script
## May 11, 2022
#################################################################################

#####
## Load data
#####
## set working directory
setwd("C:..your directory here")

## load tidy MOTU table generated from 'from_original_data_to_data_for_analysis_commented_s1.R' script
motus <- read.csv("C:..your directory here/natl12s_forNAtlproject_collapsed_FINAL_MOTU.csv") #this repository for contains relevant .csv file

## load master metadata csv 
mdata <- read.csv("C:..your directory here/d/all_metadata.csv") #this repository for contains relevant .csv file
colnames(mdata)[1] <- "long_id" #fix column name

#####
## Add taxonomic metadata to MOTU table
#####
## Create empty column in dataframe and label all assignments with
## common name vertebrate group they belong to
motus$common_group <- NA

motus$common_group <- ifelse(motus$class == "Actinopteri", "Fish", 
                             ifelse(motus$class == "Chondrichthyes", "Fish", 
                                    ifelse(motus$class == "Elasmobranchii", "Fish", 
                                           ifelse(motus$class == "Aves", "Bird", 
                                                  ifelse(motus$class == "Mammalia", "Mammal",
                                                         ifelse(motus$class == "Asteroidea", "Invertebrate",
                                                                ifelse(motus$class == "", "higherassign", NA)))))))

## Add a column to separate terrestrial from marine organisms

motus$biome <- ifelse(motus$scientific_name=="Homo sapiens", "land",
                      ifelse(motus$scientific_name=="Gallus gallus", "land",
                             ifelse(motus$scientific_name=="Meleagris gallopavo", "land",
                                ifelse(motus$scientific_name=="Sus scrofa", "land",
                                    ifelse(motus$scientific_name=="Rangifer tarandus", "land",
                                     ifelse(motus$scientific_name=="Ovis aries", "land",
                                            ifelse(motus$scientific_name=="Canis lupus", "land",
                                                   ifelse(motus$scientific_name=="Elephantulus fuscipes", "land",
                                                          ifelse(motus$scientific_name=="Bos taurus", "land", "marine")))))))))

## Add column to indicate what should be included in ecological analyses

motus$contam <- ifelse(motus$scientific_name=="Homo sapiens", "y",
                       ifelse(motus$scientific_name=="Elephantulus fuscipes", "y", #dusky-footed elephat shrew; contamination from NHM
                              ifelse(motus$scientific_name=="Amphiprion", "y", #anemonefish contamination from tank experiment
                                     ifelse(motus$scientific_name=="Pomacanthus imperator", "y", #imperial angelfish from aquariums (tank experiments)
                                            ifelse(motus$scientific_name=="Nemipterus zysron", "y", # Indo-Pacific Family - but heavily traded as seafood. Found in Sweden Pv sample
                                                          ifelse(motus$scientific_name=="Pangasiidae", "y", "n")))))) #Pangasianodon hypophthalmus; positive control

#####
## Add derived metadata to metadata 
#####
## Place depths into depth ranges bin
mdata$depth_range <- NA

mdata$depth_range <- ifelse(mdata$depth_avg <= 200, "80-200",
                            ifelse(mdata$depth_avg <= 500, "200-500",
                                   ifelse(mdata$depth_avg <= 800, "500-800",
                                          ifelse(mdata$depth_avg <= 1200, "800-1200",
                                                 ifelse(mdata$depth_avg <= 1600, "1200-1600",
                                                        ifelse(mdata$depth_avg > 1600, ">1600", NA))))))

## Add bio-region defined in Costello et al Nature communications 2017; cited in paper

mdata$bioregion_costello <- ifelse(mdata$region == "Arctic NAFO 0A_0B (close to Paamiut)", "North American Boreal",
                   ifelse(mdata$region == "Davis Strait", "North American Boreal",
                   ifelse(mdata$region == "Barents Sea", "Norwegian Sea",
                   ifelse(mdata$region == "Faroe Shetland Sponge Belt", "NE Atlantic",
                   ifelse(mdata$region == "W Rosemary bank", "NE Atlantic",
                   ifelse(mdata$region == "Trondheimsfjorden", "NE Atlantic",
                   ifelse(mdata$region == "Svalbard", "Arctic Seas",
                   ifelse(mdata$region == "Sweden", "NE Atlantic",
                   ifelse(mdata$region == "Orphan Knoll", "North American Boreal",
                   ifelse(mdata$region == "Iceland", "North American Boreal",
                   ifelse(mdata$region == "Between Jan Mayen_Schultz Massif (Mohn)", "North American Boreal",
                   ifelse(mdata$region == "Karassik Seamount", "Arctic Seas",
                   ifelse(mdata$region == "Vesterisbanken", "North American Boreal",
                   ifelse(mdata$region == "North of Shetland", "NE Atlantic", 
                   ifelse(mdata$region == "Norway_Korsfjorden", "NE Atlantic",
                   ifelse(mdata$region == "Rockall Bank", "NE Atlantic",
                   ifelse(mdata$region == "TromsÃfÂ¸flaket", "Norwegian Sea",
                   ifelse(mdata$region == "Sula Reef_Trondheimsfjorden", "NE Atlantic", 
                   ifelse(mdata$region == "Shetland Shelf North_Deep Wyville", "NE Atlantic", NA)))))))))))))))))))

## Add dates sampled
dates <- read.csv("C:..your directory here/sponge_sample_dates_collected.csv")
colnames(dates)[1] <- "long_id" #fix column name

mdata_dates <- merge(mdata, dates, by.x = "long_id", all.x = TRUE)

## Add bioregion data for nmds plot
mdata_dates$bioregion_nmds <- ifelse(mdata_dates$bioregion_costello== "Norwegian Sea", "Norwegian and Arctic Seas",
                                     ifelse(mdata_dates$bioregion_costello== "Arctic Seas", "Norwegian and Arctic Seas",
                                            ifelse(mdata_dates$bioregion_costello== "North American Boreal", "North American Boreal",
                                                   ifelse(mdata_dates$bioregion_costello== "NE Atlantic", "NE Atlantic", NA))))



