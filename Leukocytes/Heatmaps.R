#Corey Williams, University of Virginia
#21 Jul, 2019
#Adapting code from Nowicka et al., F1000 Res., 2016 to make heatmaps

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(matrixStats)
library(dplyr)
library(RColorBrewer)
library(pheatmap)
library(viridis)

INPUT.FOLDER <- getwd()
CONCATTRANSFORMED.FILENAME <- "/Concat_Transformed.csv"
CLUSTERS.FILENAME <- "/clusters.csv"
PANEL.FILENAME <- "/panel.csv"
FONTSIZE <- 16 #base fontsize for plot
FONTSIZE_NUMBER <- 10 #fontsize for numbers displayed in cells
FONTSIZE_CLUSTERING <- 12 #base fontsize for plot showing clustering vars
FONTSIZE_NUMBER_CLUSTERING <- 15 #fontsize for numbers displayed in cells in clustering var plot
HCLUST_CLUSTERS <- FALSE #whether inputted clusters are hierarchically clustered on heatmap
HCLUST_MARKERS <- TRUE #Whether markers are hierarchically clustered on heatmap
CLUSTER_REORDER <- c(1,4,9,7,3,5,8,6,10,2)

#load exprs matrix
expr <- as.matrix(read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME))

#load cluster vector
cell_clustering1 <- read.clusters(INPUT.FOLDER,CLUSTERS.FILENAME)

#load panel
panel <- read.panel(INPUT.FOLDER,PANEL.FILENAME)

#get clustering vars
clustering.vars <- get.clustering.annotate(panel)
plotting.vars <- get.plotting.annotate(panel)

#make color palette
color_clusters <- palette(rainbow(max(cell_clustering1)))
color_clusters <- palette(rainbow(max(cell_clustering1))) #unclear why, but need this line twice

## data transformation
#rng <- colQuantiles(expr, probs = c(0.01, 0.99))
#expr01 <- t((t(expr) - rng[, 1]) / (rng[, 2] - rng[, 1]))
#expr01[expr01 < 0] <- 0
#expr01[expr01 > 1] <- 1
expr01 <- apply(expr,2,function(x){x/max(x)})

plot_clustering_heatmap_wrapper <- function(expr, expr01,cell_clustering, color_clusters, cluster_merging = NULL,fontsize_heatmap,fontsize_number_heatmap){
  
  # Calculate the median expression
  expr_median <- data.frame(expr, cell_clustering = cell_clustering) %>%
    group_by(cell_clustering) %>% summarize_all(list(median))
  expr01_median <- data.frame(expr01, cell_clustering = cell_clustering) %>%
    group_by(cell_clustering) %>% summarize_all(list(median))

  # Calculate cluster frequencies
  clustering_table <- as.numeric(table(cell_clustering))
  clustering_prop <- round(clustering_table / sum(clustering_table) * 100, 2)
  
  # Sort the cell clusters with hierarchical clustering
  #d <- dist(expr_median[, colnames(expr)], method = "euclidean")
  #cluster_rows <- hclust(d, method = "average")
  cluster_rows <- HCLUST_CLUSTERS
  cluster_cols <- HCLUST_MARKERS
  
  expr_heat <- as.matrix(expr01_median[, colnames(expr01)])
  rownames(expr_heat) <- expr01_median$cell_clustering

  #scale data 0 to 1
  #range01 <- function(x){(x-min(x))/(max(x)-min(x))} # set up function to normalize on 0-1 scale
  #expr_heat <- apply(expr_heat,2,range01)
  expr_heat <- apply(expr_heat,2,function(x){
    if(max(x) == 0){
      x}
    else{
      x/max(x)}
    })
  
  # Colors for the heatmap
  #color_heat <- colorRampPalette(rev(brewer.pal(n = 9, name = "RdYlBu")))(100)
  color_heat <- viridis(100)
  legend_breaks = seq(from = 0, to = 1, by = 0.2)
  labels_row <- paste0(expr01_median$cell_clustering, " (", clustering_prop ,
                       "%)")
  
  # Annotation for the original clusters
  annotation_row <- data.frame(Cluster = factor(expr01_median$cell_clustering))
  rownames(annotation_row) <- rownames(expr_heat)
  color_clusters1 <- color_clusters[1:nlevels(annotation_row$Cluster)]
  names(color_clusters1) <- levels(annotation_row$Cluster)
  annotation_colors <- list(Cluster = color_clusters1)
  
  # Annotation for the merged clusters
  if(!is.null(cluster_merging)){
    cluster_merging$new_cluster <- factor(cluster_merging$new_cluster)
    annotation_row$Cluster_merging <- cluster_merging$new_cluster
    color_clusters2 <- color_clusters[1:nlevels(cluster_merging$new_cluster)]
    names(color_clusters2) <- levels(cluster_merging$new_cluster)
    annotation_colors$Cluster_merging <- color_clusters2
  }
  
  pheatmap(expr_heat, color = color_heat,
           cluster_rows = cluster_rows, cluster_cols = cluster_cols, labels_row = labels_row,
           fontsize = fontsize_heatmap, fontsize_number = fontsize_number_heatmap,  legend = FALSE, show_rownames = FALSE, 
           #show_colnames = FALSE
  )
  
}

#reorder clusters
cell_clustering1 <- CLUSTER_REORDER[cell_clustering1]

dev.off() #not sure why this is needed, but was having trouble making two plots without
png("Heatmap.png",width = 1162,height = 664)
plot_clustering_heatmap_wrapper(expr = expr,
                                expr01 = expr01,
                                cell_clustering = cell_clustering1,color_clusters = color_clusters,fontsize_heatmap = FONTSIZE,fontsize_number_heatmap = FONTSIZE_NUMBER)
dev.off()

#png("Heatmap_clustering.png",width = 1162/3,height = 664/3)
pdf("Heatmap_clustering.pdf")
plot_clustering_heatmap_wrapper(expr = expr[,clustering.vars],
                                expr01 = expr01[,clustering.vars],
                                cell_clustering = cell_clustering1,color_clusters = color_clusters,fontsize_heatmap = FONTSIZE_CLUSTERING,fontsize_number_heatmap = FONTSIZE_NUMBER_CLUSTERING)
dev.off()

#png("Heatmap_plotting.png",width = 1162/3,height = 664/3)
pdf("Heatmap_plotting.pdf")
plot_clustering_heatmap_wrapper(expr = expr[,plotting.vars],
                                expr01 = expr01[,plotting.vars],
                                cell_clustering = cell_clustering1,color_clusters = color_clusters,fontsize_heatmap = FONTSIZE_CLUSTERING,fontsize_number_heatmap = FONTSIZE_NUMBER_CLUSTERING)
dev.off()