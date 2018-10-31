setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/src/")

library(tidyverse)
library(readr)

trait_words <- read_csv('../resources/pda500.txt', col_names = F) %>%
  .[[1]] %>% tolower(.) %>% unique(.)

## Creating empty data frame for storage
trait_df <- as.data.frame(matrix(nrow = length(trait_words),
                                 ncol = length(trait_words),
                                 data = 0,
                                 dimnames = list(trait_words,
                                                 trait_words)))

## Making rowname column into rownames to keep matrix numeric
trait_df <- dplyr::add_rownames(trait_df, "X")


## Creating a list with all co-ocurrence matrices for all outputs
allmatrices <- list.files(paste0("../resources/output/", "1000", "/"),
                          pattern = "(.*)_network_matrix.csv",
                          full.names = T)
## Fetching the data of the co-ocurrence matrices
cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))

for(bb in 1:length(cooc)){
  colnames(cooc[[bb]])[1] <- "X"
}


## Fusing the individuals matrices into a single filled matrix
#cooc[[length(cooc) + 1]] <- trait_df
#binded <- plyr::rbind.fill(cooc)
#binded[is.na(binded)] <- 0
## Making rows unique
#result <- aggregate(binded[-1], by = list(binded$X), FUN = sum)

plotlist <- list()
for (mat in cooc){
  rownames(mat) <- mat$X
  mat <- mat[-1]
  mat <- mat[,order(names(mat))]
  MDS <- smacof::mds(mat, type = "ordinal", ndim = 3)
  coordinates <- as.data.frame(MDS$conf) %>%
    tibble::rownames_to_column()
  p <- plotly::plot_ly(coordinates, x = ~D1, y = ~D2, z = ~D3, 
                       name = coordinates$rowname)
  plotlist[[length(plotlist) + 1]] <- p
  
  MDS <- smacof::mds(mat, type = "ordinal", ndim = 2)
  coordinates <- as.data.frame(MDS$conf) %>%
    tibble::rownames_to_column()
  plot(coordinates$D1, coordinates$D2, pch = ".")
  text(coordinates$D1, coordinates$D2, pos = 4,
       cex = .5, labels = coordinates$rowname)
  
  library('fpc')
  d <- dbscan(mat,10,showplot = 2)
  d
  library(factoextra)
  fviz_cluster(d,mat, ellipse.type = "convex")
  
  ## Removing 0 from matrix
  #cleaned_result <- result[-which(rowSums(result) <= 10),
  #                         -which(colSums(result) <= 10)]
  
  ### Converting similarity into a dissimilarity matrix 
  #cleaned_result <- max(cleaned_result) - cleaned_result
  #rownames(cleaned_result) <- colnames(cleaned_result)
}



### Stress plot 
#stress_values <- c()
#for(ii in 1:10){
#  distance_MDS <- smacof::mds(cleaned_result, ndim = ii,
#                              type =   "ordinal")
#  stress_values[ii] <- distance_MDS$stress
#}
### Ordinal PCA

##3 Ties breaking Berg Gronenen 2015

### Unique ? Common ?







## Probabilities?
#probs <- result / rowSums(result)
#is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
#probs[is.nan.data.frame(probs)] <- 0
#probs <- probs[-which(rowSums(probs) == 0 ),-which(rowSums(probs) == 0 )]
#psych::principal(probs, nfactors = 5, rotate = "varimax") %>%
#  psych::fa.sort()
