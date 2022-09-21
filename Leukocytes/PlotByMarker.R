#Corey Williams, University of Virginia
#15 Jul, 2019
#Plot colored by expression of markers

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(ggfortify)
library(dplyr)
library(data.table)

## Input parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
OUTPUT.FOLDER.NAME <- "_Markers"
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
LAYOUT.FILENAME <- "/UMAP_layout.csv"
PANEL.FILENAME <- "/panel.csv"
QUANTILE <- 0.02
POINT.SIZE <- 0.2
PLOT.HEIGHT <- 1.8
PLOT.WIDTH <- 1.8

## Read needed files
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME)
layout.in <- read.layout(INPUT.FOLDER,LAYOUT.FILENAME)
panel <- read.panel(INPUT.FOLDER,PANEL.FILENAME)

## Transform data based on quantiles
#take only plotting variables
plotting.vars <- get.plotting.annotate(panel)
exprs_quantile <- concat.transformed[,plotting.vars]
#transform to upper and lower quantile
exprs_quantile <- apply(exprs_quantile,2,function(this_marker){
  replace(this_marker,
          this_marker < quantile(this_marker,QUANTILE),
          quantile(this_marker,QUANTILE)) %>% 
    replace(this_marker > quantile(this_marker,1-QUANTILE),
            quantile(this_marker,1-QUANTILE))})
#record percentiles
quantile_vals <- apply(exprs_quantile,2,function(this_marker){
  c(min(this_marker),max(this_marker))})
rownames(quantile_vals) <- c(QUANTILE,1-QUANTILE)
#untransform percentiles
quantile_vals <- apply(quantile_vals,1,function(this_quant){
  sinh(this_quant)*panel$asinh.factor[panel$Plotting == 1]})

## Prep dataframe for plotting
plotting.df <- data.frame(exprs_quantile,layout.in)

## Save plots colored by each marker
#make output folder
time.now <- Sys.time()
output.dir <- paste0(OUTPUT.FOLDER,"/",substr(time.now,start=1,stop=10),"_",
                     substr(time.now,start=12,stop=13),".",substr(time.now,start=15,stop=16),".",
                     substr(time.now,start=18,stop=19),OUTPUT.FOLDER.NAME)
dir.create(output.dir)
setwd(output.dir)
#loop through variables to plot
for (var.to.plot in plotting.vars){
  ggsave(paste0(var.to.plot,".png"),plot = ggplot(plotting.df,aes_string(x="umap_x",y="umap_y",
                                                                         color=var.to.plot)) + 
           geom_point(size = POINT.SIZE) + scale_color_viridis_c() + 
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 axis.text = element_blank(),axis.title = element_blank(),
                 axis.ticks = element_blank()) + 
           guides(color = "none"),
         height = PLOT.HEIGHT, width = PLOT.WIDTH)
}
#save quantile values
fwrite(quantile_vals %>% as.data.frame(),paste0(QUANTILE,"_percentile.csv"),row.names = TRUE)
#restore wd
setwd("..")