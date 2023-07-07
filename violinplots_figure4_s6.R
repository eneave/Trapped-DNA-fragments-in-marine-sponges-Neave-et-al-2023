#################################################################################
## Code for Violin Plots; Figure 4
## 
#################################################################################

## Load packages
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggpubr)

## Run bubblplot_figure3_s5.R to format data and make fish_long_species dataframe

##depth 

depth_indic_species_pa <-
  fish_long_species %>%
  filter(scientific_name=="Gadus morhua" | scientific_name=="Argentina sphyraena" | scientific_name=="Gadiculus argenteus" | 
           scientific_name=="Pollachius virens" | scientific_name=="Bathylagus pacificus" | scientific_name=="Macrourus berglax"|
           scientific_name=="Trisopterus esmarkii" | scientific_name=="Gadiculus argenteus" | scientific_name=="Maurolicus muelleri",
         reads > 0)

figure4_v3_depth <-
  #figure4_v3_depth_nolegend <-
  ggplot(depth_indic_species_pa, aes(x=depth_range, y=log_reads, fill=scientific_name)) +
  geom_violin(position=position_dodge(.8)) +
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(.8), alpha=0.8) +
  scale_fill_manual(values=c("#F0E442", "black", "#33a02c", "#ff7f00",
                             "#fdbf6f", "#1f78b4", "#994F00", "#cab2d6")) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  facet_grid(rows=vars(depth_range), scales = "free", space = "free") +
  xlab("Depth (m)") + ylab ("log(Read counts)") +
  labs(fill="Depth indicators") +
  theme(strip.background.y =element_blank(),
        strip.text.y= element_blank(),
        axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"), 
        axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 11, face ="bold", colour ="black"), 
        legend.text = element_text(size = 11, face ="bold.italic", colour ="black"), 
        legend.position = "right",
        #legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        #panel.background = element_blank(), 
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) 


##region

region_indic_species_pa <-
  fish_long_species %>%
  filter(scientific_name=="Bathylagus pacificus" | scientific_name=="Trisopterus esmarkii" | scientific_name=="Microchirus variegatus" | scientific_name=="Glyptocephalus cynoglossus" |
           scientific_name=="Macrourus berglax" | scientific_name=="Pollachius virens" | scientific_name=="Gadiculus argenteus" | scientific_name=="Gadus morhua", 
         reads > 0)

region_indic_species_pa[region_indic_species_pa == "NE Atlantic"] <- "Northeast Atlantic"


figure4_v3_region <-
  #figure4_v3_region_nolegend <-
  ggplot(region_indic_species_pa, aes(x=bioregion_nmds, y=log_reads, fill=scientific_name)) +
  geom_violin(binaxis='y', stackdir='center',position=position_dodge(.8)) +
  geom_dotplot(binaxis='y', stackdir='center',position=position_dodge(.8), alpha=0.8) +
  scale_fill_manual(values=c("black","#33a02c","#ff7f00","#fb9a99",
                             "#fdbf6f", "#e31a1c", "#994F00", "#cab2d6")) +
  facet_grid(rows=vars(bioregion_nmds), scales = "free", space = "free") +
  coord_flip() +
  xlab("") +  ylab ("log(Read counts)") +
  labs(fill="Region indicators") +
  theme(strip.background.y =element_blank(),
        strip.text.y= element_blank(),
        axis.text.y = element_text(angle=90, hjust=0.5, vjust=0.5, colour = "black", 
                                   size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"), 
        axis.title.y = element_text(face = "bold", size = 16), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        #legend.title = element_text(size = 11, face ="bold", colour ="black"), 
        #legend.text = element_text(size = 11, face ="bold.italic", colour ="black"), 
        #legend.position = "right",
        legend.position = "none",
        #legend.background = element_blank(),
        #legend.box.background = element_rect(colour = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))


## MPA 

mpa_indic_species <- filter(mpa_indic_species, reads > 0)

figure4_v3_mpa <-
  #figure4_v3_mpa_nolegend <-
  ggplot(mpa_indic_species, aes(x=mpa4, y=log_reads, fill=scientific_name)) +
  geom_violin(position=position_dodge(.7)) +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(.7), alpha=0.8) +
  coord_flip() +
  xlab("") + ylab ("log(Read counts)") +
  labs(fill="MPA Status indicators") +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"), 
        axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 11, face ="bold", colour ="black"), 
        legend.text = element_text(size = 11, face ="bold.italic", colour ="black"), 
        legend.position = "right",
        #legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))


## Patch plots together

legend_A <- get_legend(figure4_v3_region)
legend_B <- get_legend(figure4_v3_depth)
legend_C <- get_legend(figure4_v3_mpa)


legends1 <-
  plot_grid(legend_A, legend_B, nrow = 1)

plots1 <-
  plot_grid(figure4_v3_region_nolegend, figure4_v3_depth_nolegend, ncol = 2,
            labels = c("A", "B"), label_size = 20,axis = c("l"), align = c("v"))

fig4revision<-
  plot_grid(plots1, legends1, figure4_v3_mpa, ncol = 1, align = "v", 
            labels = c("", " ", "C"),  label_size = 20, rel_heights = c(3,0.75,1))


ggsave(filename=c("C:your Directory here/Figure4_Final_June23.jpg"), 
       plot = fig4revision, width = 11, height = 18, dpi = 900, units = "in")


