#Corey Williams, University of Virginia
#15 Jul, 2019
#Plot colored by expression of markers

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(ggfortify)

## Input parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
OUTPUT.FOLDER.NAME <- "_DotPlots"
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
PANEL.FILENAME <- "/panel.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
PLOT.HEIGHT <- 4
PLOT.WIDTH <- 4
POINT.SIZE <- 0.005

## Read needed files
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME)
panel <- read.panel(INPUT.FOLDER,PANEL.FILENAME)
clusters.in <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)

## Take only clusters 2 & 9
concat.transformed <- concat.transformed[which(clusters.in %in% c(2,9)),]

## Plot
#make output folder
time.now <- Sys.time()
output.dir <- paste0(OUTPUT.FOLDER,"/",substr(time.now,start=1,stop=10),"_",
                     substr(time.now,start=12,stop=13),".",substr(time.now,start=15,stop=16),".",
                     substr(time.now,start=18,stop=19),OUTPUT.FOLDER.NAME)
dir.create(output.dir)
setwd(output.dir)
#save plots
ggsave("aSMA.png",ggplot(concat.transformed,aes(x = GFP_Dy162Di,y = aSMA_Tb159Di)) + 
         geom_point(size = POINT.SIZE) + xlim(c(0,asinh(1e4))) + ylim(c(0,asinh(1e4))) +  
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"),
               axis.text = element_blank(),axis.title = element_blank()),
       width = PLOT.WIDTH, height = PLOT.HEIGHT
       )
ggsave("Myh11.png",ggplot(concat.transformed,aes(x = GFP_Dy162Di,y = Myh11_Sm147Di)) + 
         geom_point(size = POINT.SIZE) + xlim(c(0,asinh(1e4))) + ylim(c(0,asinh(1e4))) +  
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"),
               axis.text = element_blank(),axis.title = element_blank()),
       width = PLOT.WIDTH, height = PLOT.HEIGHT
       )
ggsave("SM22a.png",ggplot(concat.transformed,aes(x = GFP_Dy162Di,y = SM22a_Dy161Di)) + 
         geom_point(size = POINT.SIZE) + xlim(c(0,asinh(1e4))) + ylim(c(0,asinh(1e4))) +  
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"),
               axis.text = element_blank(),axis.title = element_blank()),
       width = PLOT.WIDTH, height = PLOT.HEIGHT
       )
setwd("..")