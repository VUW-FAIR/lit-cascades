library(tidyverse)

library(gridExtra)  

# Function ----------------------------------------------------------------
plot_cluster <- function(data, var_cluster, palette)  
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
  geom_point(size= 1) +
  geom_label(aes(label = rowname)) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") + 
    scale_colour_brewer(palette = palette) 
}

#evaluate kmeans cluster quality
kmIC = function(fit){
  
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(data.frame(AIC = D + 2*m*k,
                    BIC = D + log(n)*m*k))
}


## Creating a list with all co-ocurrence matrices for all outputs
allmatrices <- list.files(paste0("resources/output/", "sentence", "/"),
                            pattern = "(.*)_network_matrix.csv",
                            full.names = T)

alllinks <- list.files(paste0("resources/output/", "sentence", "/"),
                            pattern = "(.*)_links.csv",
                            full.names = T)

allnodess <- list.files(paste0("resources/output/", "sentence", "/"),
                       pattern = "(.*)_nodes.csv",
                       full.names = T)


## Fetching the data of the co-ocurrence matrices
cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))
link <- lapply(alllinks, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))
# testing some recurrence network analysis
g <- igraph::graph_from_data_frame(link[[1]])
igraph::E(g)$weight <- link[[1]]$target-link[[1]]$source
g_mat <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")
test <- Rtsne::Rtsne(g_mat,check_duplicates=FALSE,
                     pca=TRUE, perplexity = 2, theta=0.5, dims=2)
plot(test$Y)
text(test$Y, labels=rownames(g_mat))



for(bb in 1:length(cooc)){
  rownames(cooc[[bb]]) <- cooc[[bb]][,1]
  cooc[[bb]][,1] <- NULL
  
  # rare co-occurring terms
  rareT <- which(rowSums(cooc[[bb]]) <= ceiling(max(rowSums(cooc[[bb]])) * .10))
  # frequent co-occ terms
  freqT <- which(rowSums(cooc[[bb]]) > ceiling(max(rowSums(cooc[[bb]])) * .10))
  # rare terms only co-occurring with rare terms
  finalRare <- which(colSums(cooc[[bb]][freqT,]) == 0)
  
  cooc[[bb]] <- cooc[[bb]][-finalRare,-finalRare]
  #cooc[[bb]] <- cooc[[bb]][-which(rowSums(cooc[[bb]]) < ceiling(max(rowSums(cooc[[bb]])) * .10)),
  #                         -which(colSums(cooc[[bb]]) < ceiling(max(colSums(cooc[[bb]])) * .10))]
  
}

tsn_list <- list()
names_list <- list()
for(sent in cooc){

  test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                     pca=TRUE, perplexity = 2, theta=0.5, dims=2)
  
  plot(test$Y)
  text(test$Y, labels=rownames(sent))
  
  tsn_list[[length(tsn_list) + 1]] <- test
  names_list[[length(names_list) + 1]] <- rownames(sent)
}


for(number in 1:length(tsn_list)){
  d_tsne_1 <- as.data.frame(tsn_list[[number]]$Y) 
  d_tsne_1 <- cbind(d_tsne_1, rowname = names_list[[number]]) 
   
  rng<-2:20 #K from 2 to 20
  tries <-1000 #Run the K Means algorithm 100 times 
  avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points 
  avgkIC <-double(length(rng))
  for(v in rng){ # For each value of the range variable
     v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
     tmpkIC <-double(tries)
     for(i in 1:tries){
       k.temp <-kmeans(d_tsne_1[-3],centers=v) #Run kmeans
       v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
       tmpkIC[i]  <- kmIC(k.temp)$BIC
     }
     avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss 
     avgkIC[v-1] <-mean(tmpkIC) 
  }
  components_number <-  sum(abs(diff(avg.totw.ss)) >= (max(abs(diff(avg.totw.ss))) * .05))
  
  ## keeping original data
  d_tsne_1_original = d_tsne_1
  
  ## Creating k-means clustering model, and assigning the result to the data used to create the tsne
  fit_cluster_kmeans = kmeans(scale(d_tsne_1[-3]), components_number)  
  d_tsne_1_original$cl_kmeans = factor(fit_cluster_kmeans$cluster)
  
  ## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne
  fit_cluster_hierarchical=hclust(dist(scale(d_tsne_1[-3])))
  
  ## setting 3 clusters as output
  d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical,
                                                    k = components_number)) 
  
  plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Accent")  
  plot_h=plot_cluster(d_tsne_1_original, "cl_hierarchical", "Set1")
  
  ## and finally: putting the plots side by side with gridExtra lib...
  grid.arrange(plot_k, plot_h,  ncol=2)

}

#random forest example

rf.fit <- randomForest::randomForest(x = cooc[[1]], y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=3)
metricsgraphics::mjs_plot(rf.cluster, x=PC1, y=PC2) %>%
  metricsgraphics::mjs_point(color_accessor=rf.clusters) %>%
  metricsgraphics::mjs_labs(x="principal comp 1", y="principal comp 2")
