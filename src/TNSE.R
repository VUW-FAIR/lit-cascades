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

allnodes <- list.files(paste0("resources/output/", "sentence", "/"),
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


#model based example
library(mclust)
fit <- mclust::Mclust(cooc[[1]])
plot(fit) # plot results 
summary(fit) # display the best model
#evaluate cluster solutions
#cluster.stats(cooc[[1]], fit1$cluster, fit2$cluster)


## additional TIC features - testing
casc <- c()
inter <- c()
ent <- data.frame(ww = numeric(0), xx = numeric(0), yy = numeric(0), zz = numeric(0))
wien <- c()
colnames(links) <- c('source', 'target', 'tag')

coordinates <- c()
spec <- list()
div = 1

g1 <- igraph::make_empty_graph(n = 0, directed = TRUE)
struct <- c()
props <- c()
for(z in 1:nrow(nodes)){
  if(nodes[z,]$title == ""){
    if(nrow(ent) > 0){
      ent <- rbind(ent, ent[nrow(ent),])
      wien <- rbind(wien, wien[nrow(wien),])
      
    }else{
      ent <- rbind(ent, c(0, 1, 0, 1))
      wien <- rbind(wien, c(0, 1, 1))
    }
    coordinates <- rbind(coordinates, c(as.numeric(nodes[z, 1]), 0, 0))
    
  }else{
    inter <- rbind(inter, paste(sort(unlist(strsplit(nodes[z,2],', '))), collapse = ', '))
    nextI <- digest(paste(sort(unlist(strsplit(nodes[z,2], ', '))), collapse = ', '), algo = "md5")
    if(length(spec) == 0){
      coordinates <- rbind(coordinates, c(as.numeric(nodes[z, 1]), 1, 1))
      spec[[nextI]] <- c(1, 1)
    }
    else{
      if(is.null(spec[[nextI]])){
        spec[[nextI]] <- c(1,div)
        coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],div))
        div <- div+1
      }else{
        spec[[nextI]] <- c(spec[[nextI]][1]+1,spec[[nextI]][2])
        coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],spec[[nextI]][2]))
      }
    }
    
    interact <- list()
    cooccure <- list()
    #temp1 <- c()
    for(v in 1:nrow(inter)){
      interactions <- unlist(strsplit(unlist(inter[v,1]),', '))
      #temp1 <- rbind(temp1, gtools::combinations(length(interactions), 2, interactions))
      for( m in 1:length(interactions)){
        if(is.null(interact[[interactions[m]]])) interact[[interactions[m]]] <- 1
        else interact[[interactions[m]]] <- interact[[interactions[m]]] + 1
      }
    }
    df <- data.frame(unlist(interact))
    tmp <- df[,1] / colSums(df)
    df$loga <- log(tmp)
    df$piloga <- tmp * log(tmp)
    if(is.nan((-1 * (colSums(df)[3])) / log(nrow(df)))){
      ent <- rbind(ent,c(entropy.empirical(df[,1], unit = "log2"), 1, -1 * (colSums(df)[3]), 1))
    } else{
      ent <- rbind(ent, c(entropy.empirical(df[,1], unit = "log2"), (-1 * (colSums(df)[3])) / log2(nrow(df)),-1*(colSums(df)[3]),(-1*(colSums(df)[3]))/log(nrow(df))))
    }
    
    if(nrow(df) == 1){
      wien <- rbind(wien, c(0, 1, 1))
    } else{
      H <- vegan::diversity(df[,1])
      S <- nrow(df)
      J <- H/log(S)
      wien <- rbind(wien,c(H, J, S))
    }}
  
  #}
  colnames(ent)<-c('empEntropy', 'evenness_log2', 'entropy', 'evenness')
  colnames(wien)<-c('ShannonWiener', 'Pielou', 'Richness')
  
  #add node
  g1 <- igraph::add_vertices(g1,1,attr = list(id = as.numeric(nodes[z,1])))
  
  #add all links to node
  theLinks <- unique(links[which(links[,2] == nodes[z,1]),1:2])
  for(srclnk in theLinks[,1]){
    g1 <- igraph::add_edges(g1, c(which(V(g1)$id == srclnk), which(V(g1)$id == as.numeric(nodes[z,1]))))
  }
  
  #degd <- degree.distribution(g1)
  wtc <- igraph::cluster_walktrap(g1)
  struct <- rbind(struct, c(igraph::diameter(g1), igraph::edge_density(g1), igraph::modularity(wtc), igraph::cohesion(g1)))
}

write.table(cbind(coordinates, wien, struct),
            file=paste0("resources/output/", sliceSize, "/", theSource,
                        "_temporal_statistics.csv"), row.names = F, col.names = F, sep = ";")

# further analysis tests

link[[1]]$dist <- link[[1]]$target-link[[1]]$source
link[[1]]$hlp <- 1
linkFeatures <- cbind(aggregate(dist ~ tag,link[[1]],mean),aggregate(hlp ~ tag,link[[1]],sum))
rownames(linkFeatures) <- linkFeatures$tag

# what recurrs but what does not co-occur
recNOTco <- linkFeatures$tag[-which(linkFeatures$tag %in% rownames(cooc[[1]]))]

# what co-occurs but doesn't recur
coNOTrec <- rownames(cooc[[1]])[-which(rownames(cooc[[1]]) %in% linkFeatures$tag)]

#what does recur and co-occur
corec <- rownames(cooc[[1]])[which(rownames(cooc[[1]]) %in% linkFeatures$tag)]

#all terms
fusedTerms <- as.data.frame(sort(unique(c(recNOTco,coNOTrec,corec))),stringsAsFactors = F)
colnames(fusedTerms) <- c("terms")
rownames(fusedTerms) <- fusedTerms$terms
fusedTerms$coocs <- 0
fusedTerms$distcoocs <- 0
fusedTerms$recs <- 0
fusedTerms$distrecs <- 0

#co-occ featrues
fusedTerms[coNOTrec,2] <- colSums(cooc[[1]][coNOTrec])
fusedTerms[corec,2] <- colSums(cooc[[1]][corec])

fusedTerms[coNOTrec,3] <- colSums(cooc[[1]][coNOTrec] > 0)
fusedTerms[corec,3] <- colSums(cooc[[1]][corec] > 0)

fusedTerms$coocProp <- 0
fusedTerms[which(fusedTerms$coocs>0),6] <- fusedTerms[which(fusedTerms$coocs>0),3] / fusedTerms[which(fusedTerms$coocs>0),2]

fusedTerms[corec,4] <- linkFeatures[corec,4]
fusedTerms[recNOTco,4] <- linkFeatures[recNOTco,4]

fusedTerms[corec,5] <- linkFeatures[corec,2]
fusedTerms[recNOTco,5] <- linkFeatures[recNOTco,2]

ktmp <- kmeans(fusedTerms[-c(1:3)],5)
foo <- fusedTerms[-c(1:3)]
PCA <-prcomp(foo)$x
plot(PCA, col=ktmp$cluster)
text(x=PCA[,1], y=PCA[,2], cex=0.6, pos=4, labels=(row.names(foo)))
plot(PCA[,2],PCA[,3], col=ktmp$cluster)
text(x=PCA[,2], y=PCA[,3], cex=0.6, pos=4, labels=(row.names(foo)))
plot(PCA[,1],PCA[,3], col=ktmp$cluster)
text(x=PCA[,1], y=PCA[,3], cex=0.6, pos=4, labels=(row.names(foo)))


# testing some recurrence network analysis
g <- igraph::graph_from_data_frame(link[[1]])
igraph::E(g)$weight <- link[[1]]$target-link[[1]]$source
g_mat <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")
test <- Rtsne::Rtsne(g_mat,check_duplicates=FALSE,
                     pca=TRUE, perplexity = 2, theta=0.5, dims=2)
plot(test$Y)
text(test$Y, labels=rownames(g_mat))