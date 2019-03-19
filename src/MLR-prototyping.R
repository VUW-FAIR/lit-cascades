#Plot size params
#png
plot_res = 500
plot_pointsize = 3
plot_width = 1920
plot_height = 1080
plot_units = "px"
#plot
plot_cex = 1
plot_cex_clus = 0.3
plot_cex_main = 0.3
plot_cex_txt = 0.3
plot_cex_txt_clus = 0.2
plot_cex_lab = 0.8
plot_cex_axis = 0.3


#data
library(tidyverse)
library(gridExtra)

setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/pda500-1000words-advs-lemma-book-centric/")

## Creating a list with all co-ocurrence matrices for all outputs
allmatrices <- list.files(paste0("."),
                          pattern = "(.*)_network_matrix.csv",
                          full.names = T)

alllinks <- list.files(paste0("."),
                       pattern = "(.*)_links.csv",
                       full.names = T)

allnodes <- list.files(paste0("."),
                       pattern = "(.*)_nodes.csv",
                       full.names = T)


## Fetching the data of the co-ocurrence matrices
cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))
link <- lapply(alllinks, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))
node <- lapply(allnodes, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))

for(bb in 1:length(cooc)){
  rownames(cooc[[bb]]) <- cooc[[bb]][,1]
  cooc[[bb]][,1] <- NULL
}




for (sent in cooc) {
  ## cluster text
  test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                       pca=TRUE, perplexity = max(1,floor(nrow(sent)/3)-1), 
                       theta=0, dims=2)
  
  #with dbscan
  ds <- dbscan::dbscan(scale(test$Y), 20)
  cc <- randomcoloR::distinctColorPalette(max(ds$cluster))
  sub_title <- "dbscan"
  #plot(test$Y[,1], test$Y[,2], col=cc[ds$cluster], cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0("DBSCAN"))
  #text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=rownames(sent))
  
  
  #withkmeans
  fit_cluster_kmeans <- fpc::kmeansruns(test$Y,krange=2:(nrow(sent)/2),critout=F,runs=5,criterion="ch")
  cc <- randomcoloR::distinctColorPalette(max(fit_cluster_kmeans$cluster))
  sub_title <- "dbscan"
  plot(test$Y[,1], test$Y[,2], col=cc[fit_cluster_kmeans$cluster], cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0("DBSCAN"))
  text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=rownames(sent))
}
