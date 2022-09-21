#Corey Williams, University of Virginia
#08 Apr, 2021
#Make comparisons between condition pairings, using heatmaps and corrected p-values

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(gplots)
library(ggfortify)

## Input parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
METADATA.FILENAME <- "/metadata.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
CONCAT.TRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
OUTPUT.FOLDER.NAME <- "_Abundances"

## Read needed files
#Read metadata
metadata_in <- read.metadata(INPUT.FOLDER,METADATA.FILENAME)
#Read clusters
clusters <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)
#Read concat transformed
concat.transformed <- read.concat.transformed(INPUT.FOLDER,CONCAT.TRANSFORMED.FILENAME)

## Get % abundance for each file
cluster.abundances <- sapply(unique(concat.transformed[,"File"]),
                             function(x){sapply(sort(unique(clusters)),
                                                function(y){sum(clusters[
                                                  which(concat.transformed[,"File"]==x)]==y)/
                                                    length(which(concat.transformed[,"File"]==x))})})

## Make box plots
#get condition for each file
condition <- paste(metadata_in$Genotype,metadata_in$Diet,metadata_in$Timepoint)
#Replace spaces with underscores
#condition <- str_replace_all(condition," ","_")

#prep abundances for plotting
abundances_plotting <- data.frame(t(cluster.abundances),condition)
#Keep only WT animals
abundances_plotting <- dplyr::filter(abundances_plotting,condition %in% c("WT chow 1 wk","WT chow 4 wk","WT HFD 1 wk","WT HFD 4 wk"))
#Remove WT from names
abundances_plotting$condition <- abundances_plotting$condition %>% 
  replace(abundances_plotting$condition == "WT chow 1 wk","1 wk chow") %>%
  replace(abundances_plotting$condition == "WT chow 4 wk","4 wk chow") %>%
  replace(abundances_plotting$condition == "WT HFD 1 wk","1 wk WD") %>%
  replace(abundances_plotting$condition == "WT HFD 4 wk","4 wk WD")
#reorder condition names
abundances_plotting$condition <- factor(abundances_plotting$condition,levels = c("1 wk chow","4 wk chow","1 wk WD","4 wk WD"))

#Take just 1wk points
abundances_plotting <- dplyr::filter(abundances_plotting,condition %in% c("1 wk chow","1 wk WD","4 wk WD"))
#colnames(abundances_plotting) <- c(paste0("Cluster",clusters %>% unique %>% sort),"condition")
colnames(abundances_plotting) <- c("CD40loMyh11lo","CD40hiMyh11hi","CD40loMyh11mid",
                                   "CD40midMyh11hi","CD40midMyh11mid","SMAloMyh11mid","SMAlo",
                                   "CD34hi","CD40midMyh11lo","CXCL12","condition")

#make output folder
time.now <- Sys.time()
output.dir <- paste0(OUTPUT.FOLDER,"/",substr(time.now,start=1,stop=10),"_",
                     substr(time.now,start=12,stop=13),".",substr(time.now,start=15,stop=16),".",
                     substr(time.now,start=18,stop=19),OUTPUT.FOLDER.NAME)
dir.create(output.dir)
setwd(output.dir)
#plot
for(this_cluster in colnames(abundances_plotting)){
  ggsave(paste0(this_cluster,".pdf"),
         ggplot(abundances_plotting,aes_string(x = "condition",y = this_cluster)) + 
           geom_boxplot() + geom_point() + 
           theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                 panel.background = element_blank(),axis.line = element_line(colour = "black"),
                 axis.title = element_blank(),
                 axis.text.x = element_text(angle = 270,hjust = 0,vjust = 0.5)) + 
           guides(colour = guide_legend(override.aes = list(shape=15, size=8))),
         width = 1.8,height = 1.8)
}
#restore working directory
setwd("..")
