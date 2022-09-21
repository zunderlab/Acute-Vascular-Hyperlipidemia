# Corey Williams, University of Virginia
# 20 Sep, 2021
# Compare mean expression levels for different markers

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(dplyr)
library(ggfortify)
library(stringr)

INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
OUTPUT.FOLDER.NAME <- "_WT_means"
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
METADATA.FILENAME <- "/metadata.csv"

cells_in <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME)
metadata_in <- read.metadata(INPUT.FOLDER,METADATA.FILENAME)

#get means for each file
means <- sapply(1:max(cells_in$File),function(this_file){
  apply(cells_in %>% dplyr::filter(File == this_file),2,mean)})
exprs <- data.frame(t(means),metadata_in)

#WT chow, 1wk vs. 4wk
sapply(colnames(cells_in)[1:(ncol(cells_in)-1)],function(this_marker){
  t.test(dplyr::filter(exprs,Timepoint == "1 wk",Diet == "chow",Genotype == "WT")[,this_marker],
         dplyr::filter(exprs,Timepoint == "4 wk",Diet == "chow",Genotype == "WT")[,this_marker])$p.value
}) %>% p.adjust(method = "fdr") %>% sort
#WT 1wk, chow vs. WD
sapply(colnames(cells_in)[1:(ncol(cells_in)-1)],function(this_marker){
  t.test(dplyr::filter(exprs,Timepoint == "1 wk",Diet == "chow",Genotype == "WT")[,this_marker],
         dplyr::filter(exprs,Timepoint == "1 wk",Diet == "HFD",Genotype == "WT")[,this_marker])$p.value
}) %>% p.adjust(method = "fdr") %>% sort
#WT 4wk, chow vs. WD
sapply(colnames(cells_in)[1:(ncol(cells_in)-1)],function(this_marker){
  t.test(dplyr::filter(exprs,Timepoint == "4 wk",Diet == "chow",Genotype == "WT")[,this_marker],
         dplyr::filter(exprs,Timepoint == "4 wk",Diet == "HFD",Genotype == "WT")[,this_marker])$p.value
}) %>% p.adjust(method = "fdr") %>% sort
#WT WD, 1wk vs. 4wk
sapply(colnames(cells_in)[1:(ncol(cells_in)-1)],function(this_marker){
  t.test(dplyr::filter(exprs,Timepoint == "1 wk",Diet == "HFD",Genotype == "WT")[,this_marker],
         dplyr::filter(exprs,Timepoint == "4 wk",Diet == "HFD",Genotype == "WT")[,this_marker])$p.value
}) %>% p.adjust(method = "fdr") %>% sort
#WT 1wk chow vs. KO 1 wk chow
sapply(colnames(cells_in)[1:(ncol(cells_in)-1)],function(this_marker){
  t.test(dplyr::filter(exprs,Timepoint == "1 wk",Diet == "chow",Genotype == "WT")[,this_marker],
         dplyr::filter(exprs,Timepoint == "1 wk",Diet == "chow",Genotype == "KO")[,this_marker])$p.value
}) %>% p.adjust(method = "fdr") %>% sort
#KO chow, 1wk vs. 4wk
sapply(colnames(cells_in)[1:(ncol(cells_in)-1)],function(this_marker){
  t.test(dplyr::filter(exprs,Timepoint == "1 wk",Diet == "chow",Genotype == "KO")[,this_marker],
         dplyr::filter(exprs,Timepoint == "4 wk",Diet == "chow",Genotype == "KO")[,this_marker])$p.value
}) %>% p.adjust(method = "fdr") %>% sort
#KO 1wk, chow vs. WD
sapply(colnames(cells_in)[1:(ncol(cells_in)-1)],function(this_marker){
  t.test(dplyr::filter(exprs,Timepoint == "1 wk",Diet == "chow",Genotype == "KO")[,this_marker],
         dplyr::filter(exprs,Timepoint == "1 wk",Diet == "HFD",Genotype == "KO")[,this_marker])$p.value
}) %>% p.adjust(method = "fdr") %>% sort
#KO WD, 1wk vs. 4wk
sapply(colnames(cells_in)[1:(ncol(cells_in)-1)],function(this_marker){
  t.test(dplyr::filter(exprs,Timepoint == "1 wk",Diet == "HFD",Genotype == "KO")[,this_marker],
         dplyr::filter(exprs,Timepoint == "4 wk",Diet == "HFD",Genotype == "KO")[,this_marker])$p.value
}) %>% p.adjust(method = "fdr") %>% sort

## Make box plots
#get condition for each file
condition <- paste(metadata_in$Genotype,metadata_in$Diet,metadata_in$Timepoint)
#Replace spaces with underscores
#condition <- str_replace_all(condition," ","_")

#prep means for plotting
means_plotting <- data.frame(t(means),condition)
#Keep only WT animals
means_plotting <- dplyr::filter(means_plotting,condition %in% c("WT chow 1 wk","WT chow 4 wk","WT HFD 1 wk","WT HFD 4 wk"))
#Remove WT from names
means_plotting$condition <- means_plotting$condition %>% 
  replace(means_plotting$condition == "WT chow 1 wk","1 wk chow") %>%
  replace(means_plotting$condition == "WT chow 4 wk","4 wk chow") %>%
  replace(means_plotting$condition == "WT HFD 1 wk","1 wk WD") %>%
  replace(means_plotting$condition == "WT HFD 4 wk","4 wk WD")
#reorder condition names
means_plotting$condition <- factor(means_plotting$condition,levels = c("1 wk chow","4 wk chow","1 wk WD","4 wk WD"))

#Take just 1wk points
means_plotting <- dplyr::filter(means_plotting,condition %in% c("1 wk chow","1 wk WD"))

#make output folder
time.now <- Sys.time()
output.dir <- paste0(OUTPUT.FOLDER,"/",substr(time.now,start=1,stop=10),"_",
                     substr(time.now,start=12,stop=13),".",substr(time.now,start=15,stop=16),".",
                     substr(time.now,start=18,stop=19),OUTPUT.FOLDER.NAME)
dir.create(output.dir)
setwd(output.dir)
#plot
for(this_marker in rownames(means)){
  ggsave(paste0(this_marker,".pdf"),
         ggplot(means_plotting,aes_string(x = "condition",y = this_marker)) + 
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