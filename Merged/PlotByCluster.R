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
LAYOUT.FILENAME <- "/UMAP_layout_1000.csv"
CLUSTERS.FILENAME <- "/Clusters_Subsampled_1000.csv"
CLUSTER.PLOT.FILENAME <- "Clusters_1000_01.png"
POINT.SIZE <- .1
ANNOTATE.CLUSTERS <- FALSE #(TRUE if you want to print cluster numbers on output plot)
#colors from https://mokole.com/palette.html
COLORS <- c("#228b22","#7f0000","#4b0082","#ff0000","#ffa500","#00ff00","#0000ff","#dda0dd","#ff1493","#7fffd4",
            "#228b22","#7f0000","#ff0000","#d2b48c","#ffa500","#00ff00","#0000ff","#dda0dd",
            "#7fffd4","#7f0000","#228b22","#ff0000","#ffa500","#00ff00","#0000ff","#dda0dd","#ff1493","#4b0082",
            "#dda0dd","#2f4f4f","#00ff00","#ff0000","#ffa500","#4b0082","#0000ff","#228b22","#00bfff","#7fffd4")
# COLORS <- c("#f76567","#f55b5b","#f25050","#f04544","#ec3937","#e82b2a","#e41a1c",
#             "#94caf7","#87bfee","#7ab4e5","#6ea9dc","#619ed3","#5393ca","#4688c1","#377eb8",
#             "#b4f7b1","#a7eea4","#9be597","#8edc8a","#82d37d","#75ca71","#68c164","#5bb857","#4daf4a",
#             "#efc0f7","#dda9e6","#cc92d5","#ba7bc4","#a965b4","#984ea3")
#colors from colorbrewer2.org/#type=qualitative&scheme=Paired&n=12
#COLORS <- c("#a6cee3","#e31a1c","#b2df8a","#33a02c","#fb9a99","#1f78b4","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a",
#            "#e31a1c","#b2df8a","#33a02c","#000000","#1f78b4","#ff7f00","#6a3d9a","#cab2d6",
#            "#a6cee3","#e31a1c","#b2df8a","#33a02c","#fb9a99","#1f78b4","#fdbf6f","#ff7f00","#6a3d9a","#cab2d6","#b15928",
#            "#a6cee3","#e31a1c","#b2df8a","#33a02c","#fb9a99","#1f78b4","#fdbf6f","#ff7f00","#6a3d9a","#cab2d6")

print("input parameters loaded, reading needed files")

## Read needed files ==============================================================================
layout.in <- read.layout(INPUT.FOLDER,LAYOUT.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)

print("needed files read, prepping data to plot")

## Prep dataframe for plotting ====================================================================
plotting.df <- as.data.frame(cbind(layout.in,clusters.in))
colnames(plotting.df) <- c("umap_x","umap_y","cluster")

print("data ready to plot, plotting")

## Save plots colored by each marker ==============================================================
#set output folder
setwd(OUTPUT.FOLDER)
#loop through variables to plot
if (ANNOTATE.CLUSTERS) {
  ggsave(CLUSTER.PLOT.FILENAME,plot = ggplot(plotting.df,aes(x=umap_x,y=umap_y, 
                                                             color=factor(cluster))) + 
           geom_point(size = POINT.SIZE) + theme(panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), 
                                                 panel.background = element_blank(), 
                                                 axis.line = element_line(colour = "black")) +
           scale_color_manual(values = COLORS) + 
           annotate("text", 
                    x = plotting.df[match(unique(plotting.df[,3]),plotting.df[,3]),1], 
                    y = plotting.df[match(unique(plotting.df[,3]),plotting.df[,3]),2], 
                    size = 5,
                    label = as.character(unique(plotting.df[,3]))) +
           guides(colour = guide_legend(override.aes = list(shape=15, size=8))),
         #^^should work for changing size/shape of legend elements... might have to tweak size per preference
         height = 7,width = 7)
} else {
  ggsave(CLUSTER.PLOT.FILENAME,plot = ggplot(plotting.df,aes(x=umap_x,y=umap_y, 
                                                             color=factor(cluster))) + 
           geom_point(size = POINT.SIZE) + theme(panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), 
                                                 panel.background = element_blank(), 
                                                 axis.line = element_line(colour = "black"),
                                                 axis.title = element_blank(),
                                                 axis.text = element_blank(),
                                                 axis.ticks = element_blank(),
                                                 plot.margin = margin()) +
           scale_color_manual(values = COLORS) + 
           guides(colour = "none"),
         #^^should work for changing size/shape of legend elements... might have to tweak size per preference
         height = 6.196,width = 3.4)
}

print("data plotted and file outputted")
print("End PlotByCluster.R")