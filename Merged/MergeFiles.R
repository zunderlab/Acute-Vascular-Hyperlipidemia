#Corey Williams, University of Virginia
#21 Jun, 2021
#Merge files for UMAP containing all cell types

rm(list = ls())
.libPaths( c( .libPaths(), "~/local/R_libs/") )

library(ZunderPipelineFunctions)
library(data.table)
library(dplyr)

#filenames
INPUT.FOLDER <- getwd()
OUTPUT.FOLDER <- INPUT.FOLDER
CONCATTRANSFORMED.FILENAME_VSMC <- "/Concat_Transformed_VSMC.csv"
CONCATTRANSFORMED.FILENAME_Endo <- "/Concat_Transformed_Endothelium.csv"
CONCATTRANSFORMED.FILENAME_Leuk <- "/Concat_Transformed_CD45.csv"
CONCATTRANSFORMED.FILENAME_Fib <- "/Concat_Transformed_Fibroblast.csv"
CLUSTER.FILENAME_VSMC <- "/clusters_VSMC.csv"
CLUSTER.FILENAME_Endo <- "/clusters_Endothelium.csv"
CLUSTER.FILENAME_Leuk <- "/clusters_CD45.csv"
CLUSTER.FILENAME_Fib <- "/clusters_Fibroblast.csv"

#read files in
concat.transformed_VSMC <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME_VSMC)
concat.transformed_Endo <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME_Endo)
concat.transformed_Leuk <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME_Leuk)
concat.transformed_Fib <- read.concat.transformed(INPUT.FOLDER,CONCATTRANSFORMED.FILENAME_Fib)
clusters_VSMC <- read.clusters(INPUT.FOLDER,CLUSTER.FILENAME_VSMC)
clusters_Endo <- read.clusters(INPUT.FOLDER,CLUSTER.FILENAME_Endo)
clusters_Leuk <- read.clusters(INPUT.FOLDER,CLUSTER.FILENAME_Leuk)
clusters_Fib <- read.clusters(INPUT.FOLDER,CLUSTER.FILENAME_Fib)

#concatenate
concat_concat <- rbind(concat.transformed_VSMC,
                       concat.transformed_Endo,
                       concat.transformed_Leuk,
                       concat.transformed_Fib)
concat_clusters <- c(clusters_VSMC,
                     clusters_Endo + max(clusters_VSMC),
                     clusters_Leuk + max(clusters_VSMC) + max(clusters_Endo),
                     clusters_Fib + max(clusters_VSMC)+max(clusters_Endo)+max(clusters_Leuk)) %>%
  as.list()

current_dir <- getwd()
setwd(OUTPUT.FOLDER)
fwrite(concat_concat,file = "concat_transformed_all.csv")
fwrite(concat_clusters,file = "clusters_all.csv")
setwd(current_dir)