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

## Input parameters
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
METADATA.FILENAME <- "/metadata.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
CONCAT.TRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
FILENAME.OUT <- "Comparisons"

## Read needed files
#Read metadata
metadata <- read.metadata(INPUT.FOLDER,METADATA.FILENAME)
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

## Comparisons
#WT, chow-fed 4wks
WT_4wk_chow_test <- apply(cluster.abundances,1,function(this_cluster){
  t.test(this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "chow" & 
                              metadata$Timepoint == "1 wk")],
         this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "chow" & 
                              metadata$Timepoint == "4 wk")])})
WT_4wk_chow_fold <- apply(cluster.abundances,1,function(this_cluster){
  mean(this_cluster[which(metadata$Genotype == "WT" & 
                            metadata$Diet == "chow" & 
                            metadata$Timepoint == "4 wk")])/
    mean(this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "chow" & 
                              metadata$Timepoint == "1 wk")])})
#WT, 1wk WD
WT_1wk_WD_test <- apply(cluster.abundances,1,function(this_cluster){
  t.test(this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "chow" & 
                              metadata$Timepoint == "1 wk")],
         this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "HFD" & 
                              metadata$Timepoint == "1 wk")])})
WT_1wk_WD_fold <- apply(cluster.abundances,1,function(this_cluster){
  mean(this_cluster[which(metadata$Genotype == "WT" & 
                            metadata$Diet == "HFD" & 
                            metadata$Timepoint == "1 wk")])/
    mean(this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "chow" & 
                              metadata$Timepoint == "1 wk")])})
#WT, WD 4wks
WT_4wk_WD_test <- apply(cluster.abundances,1,function(this_cluster){
  t.test(this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "chow" & 
                              metadata$Timepoint == "1 wk")],
         this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "HFD" & 
                              metadata$Timepoint == "4 wk")])})
WT_4wk_WD_fold <- apply(cluster.abundances,1,function(this_cluster){
  mean(this_cluster[which(metadata$Genotype == "WT" & 
                            metadata$Diet == "HFD" & 
                            metadata$Timepoint == "4 wk")])/
    mean(this_cluster[which(metadata$Genotype == "WT" & 
                              metadata$Diet == "chow" & 
                              metadata$Timepoint == "1 wk")])})

#Get p-values for multiple comparisons correct
WT_4wk_chow_p <- sapply(WT_4wk_chow_test,function(this_cluster){this_cluster$p.value})
WT_1wk_WD_p <- sapply(WT_1wk_WD_test,function(this_cluster){this_cluster$p.value})
WT_4wk_WD_p <- sapply(WT_4wk_WD_test,function(this_cluster){this_cluster$p.value})
p_vals <- cbind(WT_1wk_WD_p,WT_4wk_WD_p)

#Do multiple comparisons correction
p_vals_corrected <- p.adjust(p_vals,method = "fdr") %>% matrix(nrow = clusters %>% unique() %>% length())

## Plotting
#Prep values for plotting
plotting_vals <- cbind(WT_1wk_WD_fold,WT_4wk_WD_fold) %>% log()
colnames(plotting_vals) <- c("WT_1wk_WD","WT_4wk_WD")
#rownames(plotting_vals) <- c("Diff-high","CD74+","VCAM+","Diff-mid","Diff-low","Diff-mid","CD40+","CXCL12+")

#set palette to plot
#function from https://slowkow.com/notes/pheatmap-tutorial/#uniform-breaks
quantile_breaks <- function(xs, n = 10) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n))
  breaks[!duplicated(breaks)]
}
#scale 0 to 1
neg_breaks <- quantile_breaks(plotting_vals[plotting_vals < 0],n = 5)
pos_breaks <- quantile_breaks(plotting_vals[plotting_vals > 0],n = 5)
col_breaks <- c(neg_breaks,pos_breaks) %>% as.numeric()
color_heat <- brewer.pal(n = 9,name = "RdBu") %>% rev()
#make function to help hierarchical clustering
custom_dist <- function(x){dist(rank(x) %>% matrix(nrow = max(clusters)))}

#plot
#png("WT comparison heatmap_control.png",width = 3.5,height = 3.5,units = "in",res = 166)
pdf("WT comparison heatmap_control.pdf")
heatmap.2(plotting_vals,Colv = FALSE,distfun = custom_dist,col = color_heat,breaks = col_breaks,key = FALSE,trace = "none",,lhei = c(1,10),lwid = c(1,10),cexRow = 1,cexCol = 1,margins = c(7,10))
dev.off()

#output p values
colnames(p_vals) <- c("WT_1wk_WD","WT_4wk_WD")
#rownames(p_vals) <- c("Diff-high","CD74+","VCAM+","Diff-mid","Diff-low","Diff-mid","CD40+","CXCL12+")
colnames(p_vals_corrected) <- c("WT_1wk_WD","WT_4wk_WD")
#rownames(p_vals_corrected) <- c("Diff-high","CD74+","VCAM+","Diff-mid","Diff-low","Diff-mid","CD40+","CXCL12+")
write.csv(p_vals_corrected,"WT_p_values_vs_control.csv") #use write.csv instead of fwrite to keep rownames