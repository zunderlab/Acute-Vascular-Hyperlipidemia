#Corey Williams, University of Virginia
#15 Jul, 2019
#Plot colored by expression of markers

print("Start PlotByCluster.R")

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(ggfortify)

print("libraries loaded")

## Input parameters ===============================================================================
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
LAYOUT.FILENAME <- "/UMAP_layout.csv"
CLUSTERS.FILENAME <- "/Clusters_Subsampled_500.csv"
METADATA.FILENAME <- "/metadata.csv"
CONCAT.TRANSFORMED.FILENAME <- "/Concat_Transformed_by_Cluster_500.csv"
METADATA.TAG <- "Genotype" #Change this to the column name in metadata by which to color plot
CLUSTER.PLOT.FILENAME <- "Genotype.png"
METADATA.ORDER <- c() #Order gates for plotting #ex. c(3,1,2)
COLOR_PALETTE <- c() #Use to manually set a color palette using hex codes, 
#ex. c("#1b9e77","#d95f02","#7570b3"). In order of gates in GATES.ORDERED
POINT.SIZE <- 1

print("input parameters loaded, reading needed files")

## Read needed files ==============================================================================
layout.in <- read.layout(INPUT.FOLDER,LAYOUT.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)
metadata <- read.metadata(INPUT.FOLDER,METADATA.FILENAME)
#This is inefficient, but currently the best option I have to link file # to cells in other files
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCAT.TRANSFORMED.FILENAME) 

print("needed files read, prepping data to plot")

## Prep for plotting ==============================================================================
#set up dataframe for plotting
plotting.df <- as.data.frame(cbind(layout.in,clusters.in,
                                   metadata[concat.transformed$File,METADATA.TAG]))
colnames(plotting.df) <- c("umap_x","umap_y","cluster",METADATA.TAG)
#order tags
if (is.null(METADATA.ORDER) == FALSE){
  plotting.df[,METADATA.TAG] <- factor(plotting.df[,METADATA.TAG],
                                       levels = levels(plotting.df[,METADATA.TAG])[METADATA.ORDER])
}
#set up color scale
if (is.null(COLOR_PALETTE) == TRUE){
  color_scale <- scale_color_hue()
} else {
  color_scale <- scale_color_manual(values = COLOR_PALETTE)
}

print("data ready to plot, plotting")

## Save plots colored by metadata =================================================================
#set output folder
setwd(OUTPUT.FOLDER)
#plot metadata
ggsave(CLUSTER.PLOT.FILENAME,plot=ggplot(plotting.df,aes_string(x="umap_x",y="umap_y",
                                                                color=METADATA.TAG)) + 
         geom_point(size = POINT.SIZE)  + color_scale + 
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black")) +
         guides(colour = guide_legend(override.aes = list(shape=15, size=8))),
       #^^should work for changing size/shape of legend elements... 
       #might have to tweak size per preference
       height = 7,width = 7)

print("data plotted and file outputted")
print("End PlotByMetadata.R")