#################################################################################
## Tidying original OTU tables 
## Commented for github
## May 9, 2022
#################################################################################

#####
## Loading Original Files
#####

## set working directory
setwd("C:..your directory here")

## read in original MOTU files with common names added (common names added using the
## excel animal data feature)
l1_orig_correct_otu <- read.csv("C:..your directory here/l1_orig_correct_otu.csv")
l2_orig_correct_otu <- read.csv("C:..your directory here/l2_orig_correct_otu.csv")


#####
## Add sample names to the motu tables and recalculate total reads per OTU
#####
library(tibble)
library(tidyverse)

colnames(l1_orig_correct_otu[,c(17:55)])

## Rename columns in tidyverse newname = oldname

l1_orig_correct_otu_newnames <-
  l1_orig_correct_otu %>% 
  rename(Pv_NoS_Notag_s_09 = A02_1,
    Gb_Sva_20T5_s_25 = A04_1_2,
    Pv_SMK_ROV25_12_s_33 = A05_1,
    Gb_FS_MPA_20T6_s_49 = A07_1,
    Gh_KS_197.1_s_57 = A08_1,
    Gb_Sva_7G6_s_26 = B04_1_2,
    NEGATIVE_34 = B05_1,
    Gb_FS_MPA_14N4_s_50 = B07_1,
    Gh_KS_200.5_s_58 = B08_1,
    Pv_SRT_046_26_s_11 = C02_1,
    Pv_SMK_ROV25_13_s_35 = C05_1,
    Gb_FS_MPA_22V2_s_51 = C07_1,
    Pv_Sw_9_s_04 = D01_1,
    Pv_SRT_046_49_s_12 = D02_1,         
    Pv_RB_MPA_11417_s_20 = D03_1_2,       
    Pv_SMK_ROV25_14_s_36 = D05_1,
    Gb_Can_S_5E2_s_44 = D06_1,
    Pv_Sw_11_s_05 = E01_1,
    Pv_RB_11218_s_13 = E02_1,
    Pv_RB_MPA_11419_s_21 = E03_1_2,
    Pv_NwK_ST5_38_s_29 = E04_1,        
    Pv_SSNd_H044_01642_s_45 = E06_1,
    Pv_RB_MPA_1866_s_14 = F02_1,
    Pv_NwK_ST5_39_s_30 = F04_1,
    Gb_BS_imp_18R6_s_38 = F05_1,
    Pv_SSNd_H044_01643_s_46 = F06_1,
    Gb_Can_S_8H6_s_54 = F07_1,        
    Gh_Vst_045TVG_011B_i_62 = F08_1,
    POSITIVE_07 = G01_1,
    Pv_RB_1418_s_15 = G02_1,         
    Gb_BS_imp_9I4_s_39 = G05_1,        
    Pv_SSNd_H044_01645_s_47 = G06_1,
    Gb_Can_S_16P6_s_55 = G07_1,
    NEGATIVE_63 = G08_1,
    Pv_NoS_11452_s_08 = H01_1,
    Pv_RB_1420_s_16 = H02_1_2,
    POSITIVE_24 = H03_1_2,
    Gh_Ice_ME85_3_1132_A_s_32 = H04_1_2,
    POSITIVE_48 = H06_1,         
  )


## need to recalculate total reads now that samples are removed from the libraries

l1_readscorrect <- l1_orig_correct_otu_newnames[,c(17:55)]%>%
  mutate(new_total_reads = rowSums(.))
print(l1_readscorrect)


## recombine samples with sequencing information

l1 <- data.frame(l1_orig_correct_otu_newnames[,c(1:16,56:58,60)],l1_readscorrect)

#### repeat for lib2

colnames(l2_orig_correct_otu[,c(17:56)])

## Rename columns in tidyverse newname = oldname

l2_orig_correct_otu_newnames <-
  l2_orig_correct_otu  %>% 
  rename(CC_GBC579_NFE_17 = A03_2,
         Gh_Ice_A11_641_B_s_25 = A04_1_2,
         NEGATIVE_65 = A09_2,
         Gb_RSB_MPA_18R4_d_73 = A10_2,
         NEGATIVE_18 = B03_2,
         Pv_FS_MPA_01602_s_26 = B04_1_2,
         Gh_Can_M_Trip8_75.5_d_66 = B09_2,
         Gb_RSB_MPA_13M4_d_74 = B10_2,
         Gh_Vst_019TVG_003B_d_90 = B12_2,
         CC_GBC585_NFE_19 = C03_2,
         Gh_Ice_ME85_3_1123_2_C_d_59 = C08_1,
         Gh_Can_M_Trip8_75.7_d_67 = C09_2,  
         Gb_Can_S_15O6_d_75 = C10_2,
         Gh_Vst_019TVG_010B_d_91 = C12_2,   
         CC_GBC583_TM_20 = D03_1_2,
         Pv_FS_MPA_01601_s_52 = D07_1,
         Gh_Ice_ME85_3_1123_B_d_60 = D08_1,
         Gh_Can_M_Trip8_75.8_d_68 = D09_2,
         Gb_Can_S_2B6_d_76 = D10_2,
         Gh_Vst_011TVG_004B_d_92 = D12_2,
         Gb_Sw_15O5_s_21 = E03_1_2,
         Pv_FS_MPA_01603_s_53 = E07_1,
         Gh_Vst_045TVG_005B_i_61 = E08_1,
         Gb_Can_M_Trip8_75.1_d_69 = E09_2,
         Gb_Can_S_17Q3_d_77 = E10_2,
         Gh_JMR_SM_AGT03_26_d_93 = E12_2,
         Pv_NoS_11456_s_22 = F03_2, 
         Gb_Can_M_Trip8_75.2_d_70 = F09_2,
         Gb_Can_S_23W6_d_78 = F10_2,
         Gh_JMR_SM_AGT03_27_d_94 = F12_2,
         Pv_RB_11221_s_23 = G03_2,
         Gb_Can_M_Trip8_75.3_d_71 = G09_2,
         Gg_OK_274_d_95 = G12_2,
         CC_GBC580_TM_16 = H02_1_2,
         Pv_RB_MPA_1865_s_24 = H03_1_2,
         POSITIVE_32 = H04_1_2,
         Gh_Vst_045TVG_012B_i_64 = H08_1,  
         Gb_RSB_MPA_13M5_d_72 = H09_2,
         POSITIVE_80 = H10_2,  
         Gg_OK_275_d_96 = H12_2
  )

## need to recalculate total reads now that samples are removed from the libraries

l2_readscorrect <- l2_orig_correct_otu_newnames[,c(17:56)]%>%
  mutate(new_total_reads = rowSums(.))
print(l2_readscorrect)


## recombine samples with sequencing information

l2 <- data.frame(l2_orig_correct_otu_newnames[,c(1:16,57:59,61)],l2_readscorrect)

## l1 n = 39
## l2 n = 40


#####
## Merge libraries by sequence
#####

natldata <- merge(l1, l2, by=c("sequence"), all.x = TRUE , all.y = TRUE) 

## 1. subset sequences and taxonomic assignment columns to deal with NaNs 

natldata_tax <- natldata[,c(1:20,61:79)]

natldata_tax2 <- natldata[,c(1:20,61:79)]


natldata_tax2 <-
  natldata_tax2 %>% 
  add_column(superkingdom = NA,
             sk.id = NA,
             phylum = NA,
             p.id = NA,
             class = NA,
             C.id = NA,
             order = NA,
             O.id = NA,
             family = NA,
             F.id = NA,
             genus = NA,
             G.id = NA,
             Common.name = NA,
             species = NA,
             S.id = NA,
             seq_length = NA,    
             subsequence = NA,
             cluster_weight = NA)


for (i in 1:nrow(natldata_tax2)) {
  natldata_tax2[i, 40] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 3]
  natldata_tax2[i, 41] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 4]
  natldata_tax2[i, 42] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 5]
  natldata_tax2[i, 43] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 6]
  natldata_tax2[i, 44] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 7]
  natldata_tax2[i, 45] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 8]
  natldata_tax2[i, 46] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 9]
  natldata_tax2[i, 47] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 10]
  natldata_tax2[i, 48] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 11]
  natldata_tax2[i, 49] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 12]
  natldata_tax2[i, 50] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 13]
  natldata_tax2[i, 51] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 14]
  natldata_tax2[i, 52] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 15]
  natldata_tax2[i, 53] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 16]
  natldata_tax2[i, 54] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 17]
  natldata_tax2[i, 55] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 18]
  natldata_tax2[i, 56] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 19]
  natldata_tax2[i, 57] <- natldata_tax2[i, which(!is.na(natldata_tax2[i,]))][, 20]
}


natldata_tax3 <- natldata_tax2[,c(1,40:57)]

## 2. subset samples and recalculate total reads

natldata_samp <- natldata[,c(1,21:59,80:119)]

natldata_samp[is.na(natldata_samp)] <- 0 # convert all NAs to 0

natldata_samp <-
  natldata_samp %>% 
  add_column(neg_control_lib1_noreads = 0) # add column of zeros for neg.control with no reads from lib 01 well B02


natldata_samp2 <- 
  natldata_samp[,c(2:81)]%>%
  mutate(all_total_reads = rowSums(.))

natldata_samp3 <- data.frame(natldata_samp$sequence,natldata_samp2)
colnames(natldata_samp3)[1] <- "sequence"

sum(natldata_samp3$all_total_reads) # 1726950 == all reads that could be assigned to a MOTU from the sequencing run 

## 3. recombine taxonomy and read counts by merging data frames by sequence

natldata_final <- merge(natldata_tax3, natldata_samp3, by=c("sequence"), all.x = TRUE , all.y = TRUE) 


#####
## Remove Clarion Clipperton and Geodia g. samples from final OTU table + recalculate total reads
#####
## repeat process above, first removing samples not being used for this study

## 1. subset sequences and taxonomic assignment columns to deal with NaNs 

natldata_v2 <- merge(l1, l2[,-c(21,30,35,53,54,60)], by=c("sequence"), all.x = TRUE , all.y = TRUE) 


## 1. subset sequences and taxonomic assignment columns to deal with NaNs 

natldata_tax_v2 <- natldata_v2[,c(1:20,61:79)]

natldata_tax2_v2 <- natldata_v2[,c(1:20,61:79)]


natldata_tax2_v2 <-
  natldata_tax2_v2 %>% 
  add_column(superkingdom = NA,
             sk.id = NA,
             phylum = NA,
             p.id = NA,
             class = NA,
             C.id = NA,
             order = NA,
             O.id = NA,
             family = NA,
             F.id = NA,
             genus = NA,
             G.id = NA,
             Common.name = NA,
             species = NA,
             S.id = NA,
             seq_length = NA,    
             subsequence = NA,
             cluster_weight = NA)


for (i in 1:nrow(natldata_tax2_v2)) {
  natldata_tax2_v2[i, 40] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 3]
  natldata_tax2_v2[i, 41] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 4]
  natldata_tax2_v2[i, 42] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 5]
  natldata_tax2_v2[i, 43] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 6]
  natldata_tax2_v2[i, 44] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 7]
  natldata_tax2_v2[i, 45] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 8]
  natldata_tax2_v2[i, 46] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 9]
  natldata_tax2_v2[i, 47] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 10]
  natldata_tax2_v2[i, 48] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 11]
  natldata_tax2_v2[i, 49] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 12]
  natldata_tax2_v2[i, 50] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 13]
  natldata_tax2_v2[i, 51] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 14]
  natldata_tax2_v2[i, 52] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 15]
  natldata_tax2_v2[i, 53] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 16]
  natldata_tax2_v2[i, 54] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 17]
  natldata_tax2_v2[i, 55] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 18]
  natldata_tax2_v2[i, 56] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 19]
  natldata_tax2_v2[i, 57] <- natldata_tax2_v2[i, which(!is.na(natldata_tax2_v2[i,]))][, 20]
}


natldata_tax3_v2 <- natldata_tax2_v2[,c(1,40:57)]

## 2. subset samples and recalculate total reads

natldata_samp_v2 <- natldata_v2[,c(1,21:59,80:113)]

natldata_samp_v2[is.na(natldata_samp_v2)] <- 0 # convert all NAs to 0

natldata_samp_v2 <-
  natldata_samp_v2 %>% 
  add_column(neg_control_lib1_noreads = 0) # add column of zeros for neg.control with no reads from lib 01 well B02


natldata_samp2_v2 <- 
  natldata_samp_v2[,c(2:75)]%>%
  mutate(all_total_reads = rowSums(.))

natldata_samp3_v2 <- data.frame(natldata_samp_v2$sequence,natldata_samp2_v2)
colnames(natldata_samp3_v2)[1] <- "sequence"

sum(natldata_samp3_v2$all_total_reads) # 1616338 == all reads from the sequencing run - CC and Gv

## 3. recombine taxonomy and read counts by merging data frames by sequence

natldata_final_v2 <- merge(natldata_tax3_v2, natldata_samp3_v2, by=c("sequence"), all.x = TRUE , all.y = TRUE) 


#####
## Calculate the best taxonomic identity and the scientific rank of the best identity
#####
## load in new id key from .csv file 'all_ids.csv'
ids <- read.csv("C:..your directory here/all_ids.csv")
colnames(ids)[1] <- "long_id" #fix column name
# row 80 is empty, just NAs, for reference

natldata_tidy <- natldata_final_v2

colnames(natldata_tidy)[94] <- "total_reads" #rename the total reads column

## Create columns to calculate the scientific name and rank of the best identity for OTUs at a threshold of 97% and 80%

natldata_tidy <-
  natldata_tidy %>% add_column(scientific_name = NA,
                               rank = NA,
                               best_identity = NA,
                               scientific_name_80 = NA,
                               rank_80 = NA,
                               best_identity_80 = NA)


natldata_tidy$scientific_name <- ifelse(natldata_tidy$S.id > 97, as.character(natldata_tidy$species), 
                                        ifelse(natldata_tidy$G.id > 97, as.character(natldata_tidy$genus), 
                                               ifelse(natldata_tidy$F.id > 97, as.character(natldata_tidy$family),
                                                      ifelse(natldata_tidy$O.id > 97, as.character(natldata_tidy$order),
                                                             ifelse(natldata_tidy$C.id > 97, as.character(natldata_tidy$class),
                                                                    ifelse(natldata_tidy$p.id > 97, as.character(natldata_tidy$phylum),
                                                                           ifelse(natldata_tidy$sk.id > 97, as.character(natldata_tidy$superkingdom), NA)))))))

natldata_tidy$scientific_name_80 <- ifelse(natldata_tidy$S.id > 80, as.character(natldata_tidy$species), 
                                        ifelse(natldata_tidy$G.id > 80, as.character(natldata_tidy$genus), 
                                               ifelse(natldata_tidy$F.id > 80, as.character(natldata_tidy$family),
                                                      ifelse(natldata_tidy$O.id > 80, as.character(natldata_tidy$order),
                                                             ifelse(natldata_tidy$C.id > 80, as.character(natldata_tidy$class),
                                                                    ifelse(natldata_tidy$p.id > 80, as.character(natldata_tidy$phylum),
                                                                           ifelse(natldata_tidy$sk.id > 80, as.character(natldata_tidy$superkingdom), NA)))))))


natldata_tidy$rank <- ifelse(natldata_tidy$S.id > 97, "species", 
                             ifelse(natldata_tidy$G.id > 97, "genus", 
                                    ifelse(natldata_tidy$F.id > 97, "family",
                                           ifelse(natldata_tidy$O.id > 97, "order",
                                                  ifelse(natldata_tidy$C.id > 97, "class",
                                                         ifelse(natldata_tidy$p.id > 97, "phylum",
                                                                ifelse(natldata_tidy$sk.id > 97, "superkingdom", NA)))))))

natldata_tidy$rank_80 <- ifelse(natldata_tidy$S.id > 80, "species", 
                             ifelse(natldata_tidy$G.id > 80, "genus", 
                                    ifelse(natldata_tidy$F.id > 80, "family",
                                           ifelse(natldata_tidy$O.id > 80, "order",
                                                  ifelse(natldata_tidy$C.id > 80, "class",
                                                         ifelse(natldata_tidy$p.id > 80, "phylum",
                                                                ifelse(natldata_tidy$sk.id > 80, "superkingdom", NA)))))))


natldata_tidy$best_identity <- ifelse(natldata_tidy$S.id > 97, as.numeric(natldata_tidy$S.id), 
                                      ifelse(natldata_tidy$G.id > 97, as.numeric(natldata_tidy$G.id), 
                                             ifelse(natldata_tidy$F.id > 97, as.numeric(natldata_tidy$F.id),
                                                    ifelse(natldata_tidy$O.id > 97, as.numeric(natldata_tidy$O.id),
                                                           ifelse(natldata_tidy$C.id > 97, as.numeric(natldata_tidy$C.id),
                                                                  ifelse(natldata_tidy$p.id > 97, as.numeric(natldata_tidy$p.id),
                                                                         ifelse(natldata_tidy$sk.id > 97, as.numeric(natldata_tidy$sk.id), NA)))))))

natldata_tidy$best_identity_80 <- ifelse(natldata_tidy$S.id > 80, as.numeric(natldata_tidy$S.id), 
                                      ifelse(natldata_tidy$G.id > 80, as.numeric(natldata_tidy$G.id), 
                                             ifelse(natldata_tidy$F.id > 80, as.numeric(natldata_tidy$F.id),
                                                    ifelse(natldata_tidy$O.id > 80, as.numeric(natldata_tidy$O.id),
                                                           ifelse(natldata_tidy$C.id > 80, as.numeric(natldata_tidy$C.id),
                                                                  ifelse(natldata_tidy$p.id > 80, as.numeric(natldata_tidy$p.id),
                                                                         ifelse(natldata_tidy$sk.id > 80, as.numeric(natldata_tidy$sk.id), NA)))))))



#####
## Filter and Collapse the data by 97% best identity
#####
## Code below taken from Owen Wangensteen's metabarpark, modified to use outside of linux environment

#!/usr/bin/env Rscript

# Collapses species with the same name over an identity threshold
# The input is a csv file with field names "best_identity", "species", "sequence", "total_reads".
# Sample columns are located between start_samples and end_samples (numeric).
# The output will have the most abundant sequence as the representative and the higher best_id as the best_id
# Author: Owen S. Wangensteen, owenwangensteen@ub.edu
# Metabarpark Project, metabarpark.blogspot.com
# ChallenGen Project, challengenproject.blogspot.com

#library("optparse")

#option_list = list(
#  make_option(c("-i", "--infile"), type="character", default=NULL, 
#              help="csv file including 'id' and 'sequence' fields", metavar="character"),
#  make_option(c("-o", "--outfile"), type="character", default=NULL, 
#              help="Output file name [default = input file ending in _collapsed.csv]", metavar="character"),
#  make_option(c("-t", "--threshold"), type="numeric", default= 0.70, 
#              help="Threshold for collapsing. Default = 0.70", metavar="numeric"),
#  make_option(c("-s", "--start_samples"), type="integer", default= 14, 
#              help="Sample columns start. Default = 14", metavar="numeric"),
#  make_option(c("-e", "--end_samples"), type="integer", default= 98, 
#              help="Sample columns end. Default = 98", metavar="numeric")
#); 

#opt_parser = OptionParser(option_list=option_list);
#opt = parse_args(opt_parser);

#if (is.null(opt$infile) ){
#  print_help(opt_parser)
#  stop("At least one argument must be supplied (input csv file including 'best_identity', 'species', 'sequence' & 'total_reads' fields.", call.=FALSE)
#}


# If no outfile name, then create a suitable one
#if (is.null(opt$outfile)){
#  opt$outfile <- paste(substr(opt$infile,1,nchar(opt$infile)-4),"_collapsed.csv",sep="")
#}


# Read the csv file. Check that it has more than 1 column.
#message("Reading ",opt$infile," database")
#db <- na.exclude(read.table(opt$infile,sep=";",head=T))
#if (ncol(db)==1) db <- na.exclude(read.table(opt$infile,sep=",",head=T))
#if (ncol(db)==1) stop("Unable to read csv file. Use a file with ',' or ';' separators",call.=FALSE)
#message("Database ",opt$infile," read including ",nrow(db), " sequences.")


# Check the number of species
unique_species <- length(unique(natldata_tidy$species)) #141

db_species <- na.exclude(natldata_tidy[natldata_tidy$species!="",])

rows_with_species_name <- nrow(db_species) #472

#message("Database ",opt$infile," includes ",rows_with_species_name, " sequences with species name,")
#message("Belonging to ",unique_species," unique species.")
#message("Now collapsing species with best_identity higher than ",opt$threshold,".")

# Select which sequences need to be collapsed
db_all_collapsed <- tapply(db_species$total_reads/db_species$total_reads,db_species$species,sum,na.rm=T)


db_to_collapse <- natldata_tidy[(natldata_tidy$species %in% names(db_all_collapsed[db_all_collapsed>1])) & (natldata_tidy$best_identity>97),]

db_unmodified <-  natldata_tidy[!((natldata_tidy$species %in% names(db_all_collapsed[db_all_collapsed>1])) & (natldata_tidy$best_identity>97)),]

species_to_collapse <- unique(as.character(db_to_collapse$species)) #63

# Do the collapse calculations
species_collapsed <- NULL
for (name in species_to_collapse) {
  species_db <- db_to_collapse[db_to_collapse$species==name,]
  reads_collapsed <- colSums(species_db[20:94])
  # Order dataframe by total_reads and best_id
  species_db <- species_db[with(species_db,order(total_reads,best_identity,decreasing=T)),]
  # Get the first row
  vector <- species_db[1,]
  # Change sample columns with the sum
  vector[1,20:94] <- reads_collapsed
  # Change total_reads
  vector$total_reads[1] <- sum(reads_collapsed)
  # Change best_id
  vector$best_identity[1] <- max(species_db$best_identity)
  species_collapsed <- rbind(species_collapsed,vector)
}
#message(nrow(db_to_collapse)," sequences collapsed to ",length(species_to_collapse)," species.")

# Recover the database
db_final <- rbind(species_collapsed,db_unmodified)

## save as a .csv file
write.csv(db_final,"C:..your directory here/natl12s_forNAtlproject_collapsed_FINAL_MOTU.csv", row.names = FALSE)





