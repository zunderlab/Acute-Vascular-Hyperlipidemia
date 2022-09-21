#Corey Williams, University of Virginia
#07 Feb, 2022
#Look at expression of cluster-defining markers to see if they are edge of noise or bimodal

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(ggfortify)
library(dplyr)

## Input parameters ===============================================================================
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
CLUSTERS.FILENAME <- "/clusters.csv"
EXPRS.FILENAME <- "/Concat_Transformed.csv"
POINT.SIZE <- 0.2

## Read needed files ==============================================================================
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)
exprs.in <- read.concat.transformed(INPUT.FOLDER,EXPRS.FILENAME)

## Prep for plotting ==============================================================================
plotting.df <- data.frame(exprs.in,clusters.in)
colnames(plotting.df) <- c(colnames(exprs.in),"Cluster")

## Plot ===========================================================================================
ggplot() + 
  geom_point(data = dplyr::filter(plotting.df,Cluster != 7),
             aes(x = CD31_Gd155Di,y = CD40_Er167Di)) + 
  geom_point(data = dplyr::filter(plotting.df,Cluster == 7),
             aes(x = CD31_Gd155Di,y = CD40_Er167Di),color = "red") + 
  xlim(0,asinh(1e4/5)) + ylim(0,asinh(1e4/5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_blank(),axis.title = element_blank())
ggplot() + 
  geom_point(data = dplyr::filter(plotting.df,Cluster != 5),
             aes(x = CD31_Gd155Di,y = CD106_Sm154Di)) + 
  geom_point(data = dplyr::filter(plotting.df,Cluster == 5),
             aes(x = CD31_Gd155Di,y = CD106_Sm154Di),color = "red") +
  xlim(0,asinh(1e4/5)) + ylim(0,asinh(1e4/5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_blank(),axis.title = element_blank())
ggplot() + 
  geom_point(data = dplyr::filter(plotting.df,Cluster != 8),
             aes(x = CD31_Gd155Di,y = CXCL12_Tm169Di)) + 
  geom_point(data = dplyr::filter(plotting.df,Cluster == 8),
             aes(x = CD31_Gd155Di,y = CXCL12_Tm169Di),color = "red") +
  xlim(0,asinh(1e4/5)) + ylim(0,asinh(1e4/5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_blank(),axis.title = element_blank())
