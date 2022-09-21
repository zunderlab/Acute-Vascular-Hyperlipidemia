#Corey Williams, University of Virginia
#11 Jan, 2022
#Overlaid histograms

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(dplyr)
library(ggplot2)

INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
OUTPUT.FOLDER.NAME <- "_WT_means"
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
METADATA.FILENAME <- "/metadata.csv"

#load needed files
cells_in <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME)
metadata_in <- read.metadata(INPUT.FOLDER,METADATA.FILENAME)

#get CD40 and CD106 expression for cells of interest
cells <- cells_in %>% filter(File %in% which(metadata_in$Genotype == "WT")) %>%
  select(CD40_Er167Di,CD106_Sm154Di,File)
#add diet & timepoint info to cells
condition <- paste(metadata_in$Timepoint[cells$File],metadata_in$Diet[cells$File])
cells <- data.frame(cells,condition)
#remove 4wk chow
cells <- filter(cells,condition != "4 wk chow")

#plot
ggsave("CD40 histogram.png",ggplot(cells,aes(CD40_Er167Di,color = condition)) + 
         geom_density(size = 1.5) + 
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black"), 
               legend.key = element_blank())
       )

ggsave("VCAM histogram.png",ggplot(cells,aes(CD106_Sm154Di,color = condition)) + 
         geom_density(size = 1.5) + 
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black"), 
               legend.key = element_blank())
       )
