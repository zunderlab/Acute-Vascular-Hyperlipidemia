#Corey Williams, University of Virginia
#15 Jul, 2019
#Plot colored by expression of markers

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(ggfortify)
library(ggstance)
library(ggpubr)
library(forcats)

## Input parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
OUTPUT.FOLDER.NAME <- "_Markers"
CONCATTRANSFORMED.FILENAME <- "/concat_transformed_all.csv"
LAYOUT.FILENAME <- "/UMAP_layout.csv"
PANEL.FILENAME <- "/panel.csv"
CLUSTERS.FILENAME <- "/clusters_all.csv"
VIOLIN.HEIGHT.FACTOR <- 5
COLORS <- c("#228b22","#7f0000","#4b0082","#ff0000","#ffa500","#00ff00","#0000ff","#dda0dd","#ff1493","#7fffd4",
            "#228b22","#7f0000","#ff0000","#d2b48c","#ffa500","#00ff00","#0000ff","#dda0dd",
            "#7fffd4","#7f0000","#228b22","#ff0000","#ffa500","#00ff00","#0000ff","#dda0dd","#ff1493","#4b0082",
            "#dda0dd","#2f4f4f","#00ff00","#ff0000","#ffa500","#4b0082","#0000ff","#228b22","#00bfff","#7fffd4")

## Read needed files
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME)
panel <- read.panel(INPUT.FOLDER,PANEL.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)

## Prep dataframe for plotting
plotting.vars <- get.plotting.annotate(panel)
plotting.df <- as.data.frame(cbind(concat.transformed[,plotting.vars],clusters.in))
colnames(plotting.df)[ncol(plotting.df)] <- "cluster"

#Make list of violin plots by cluster
plist = sapply(plotting.vars, function(marker.plotting) {
  # if (marker.plotting == plotting.vars[1]){
  #   ggplot(plotting.df, 
  #          aes(x = plotting.df[,marker.plotting], y = fct_rev(factor(cluster)), 
  #              fill = factor(cluster))) + 
  #     geom_violinh(trim=FALSE,scale = "width") + xlim(0,8.5) +
  #     xlab(marker.plotting) + 
  #     theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
  #           panel.background = element_blank(),axis.line = element_line(colour = "black"),
  #           legend.position = "none",axis.text.x = element_blank(),axis.title.y = element_blank(),
  #           axis.title.x = element_text(size = 8))
  # }
  # else {
  #   ggplot(plotting.df, 
  #          aes(x = plotting.df[,marker.plotting], y = fct_rev(factor(cluster)), 
  #              fill = factor(cluster))) + 
  #     geom_violinh(trim=FALSE,scale = "width") + xlim(0,8.5) +
  #     xlab(marker.plotting) + 
  #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #           panel.background = element_blank(), axis.line = element_line(colour = "black"), 
  #           legend.position = "none",axis.text = element_blank(),axis.title.y = element_blank(),
  #           axis.title.x = element_text(size = 8))
  # }
  ggplot(plotting.df, 
         aes(x = plotting.df[,marker.plotting], y = fct_rev(factor(cluster)), 
             fill = factor(cluster),color = factor(cluster))) + 
    geom_violinh(trim=FALSE,scale = "width") + xlim(0,8.5) +
    xlab(marker.plotting) + 
    scale_fill_manual(values = COLORS) + #scale_color_manual(values = COLORS) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          legend.position = "none",
          axis.text = element_blank(),axis.title.y = element_blank(),
          axis.title.x = element_blank(),plot.margin = margin(),axis.ticks.y = element_blank())
}, simplify=FALSE)
#save Violin plots
ggsave("ViolinPlots_big.png",
       annotate_figure(ggarrange(plotlist = plist,ncol=length(plist)) + theme(plot.margin = margin())),
       height=4.656*2,width=2.98*2,units = "in")