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
  geom_point(data = dplyr::filter(plotting.df,Cluster != 1),
             aes(x = CD11b_Yb171Di,y = CD11c_Ho165Di)) + 
  geom_point(data = dplyr::filter(plotting.df,Cluster == 1),
             aes(x = CD11b_Yb171Di,y = CD11c_Ho165Di),color = "red") +
  xlim(0,asinh(1e4/5)) + ylim(0,asinh(1e4/5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_blank(),axis.title = element_blank())
ggplot() + 
  geom_point(data = dplyr::filter(plotting.df,Cluster != 5),
             aes(x = CD11b_Yb171Di,y = CD11c_Ho165Di)) + 
  geom_point(data = dplyr::filter(plotting.df,Cluster == 5),
             aes(x = CD11b_Yb171Di,y = CD11c_Ho165Di),color = "red") +
  xlim(0,asinh(1e4/5)) + ylim(0,asinh(1e4/5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_blank(),axis.title = element_blank())
ggplot() + 
  geom_point(data = dplyr::filter(plotting.df,Cluster != 2),
             aes(x = CD11b_Yb171Di,y = CD11c_Ho165Di)) +
  geom_point(data = dplyr::filter(plotting.df,Cluster == 2),
             aes(x = CD11b_Yb171Di,y = CD11c_Ho165Di),color = "red") + 
  xlim(0,asinh(1e4/5)) + ylim(0,asinh(1e4/5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_blank(),axis.title = element_blank())
ggplot() + 
  geom_point(data = dplyr::filter(plotting.df,Cluster != 8),
             aes(x = CD45_Y89Di,y = IgM_Eu151Di)) +
  geom_point(data = dplyr::filter(plotting.df,Cluster == 8),
             aes(x = CD45_Y89Di,y = IgM_Eu151Di),color = "red") + 
  xlim(0,asinh(1e4/5)) + ylim(0,asinh(1e4/5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_blank()#,axis.title = element_blank()
        )