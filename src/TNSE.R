library(tidyverse)

library(gridExtra)

setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/pda1710-1000words-advs-lemma-book-centric/")

# Function ----------------------------------------------------------------
plot_cluster <- function(data, var_cluster, palette)  
{
  ggplot(data, aes_string(x = "V1", y = "V2", color = var_cluster)) +
  geom_point(size = 1) +
  geom_label(aes(label = rowname)) +
  guides(colour=guide_legend(override.aes = list(size = 6))) +
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
kmIC <-  function(fit){
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = -fit$tot.withinss/2
  return(data.frame(AIC = D + 2 * m * k,
                    BIC = D + log(n) * m * k))
}

getClusterNumber <- function(dat, percent =.05){
  rng <- 2:18 #K from 2 to 20
  tries <- 100 #Run the K Means algorithm 100 times 
  avg.totw.ss <- integer(length(rng)) #Set up an empty vector to hold all of points 
  avgkIC <- double(length(rng))
  
  for(v in rng){ # For each value of the range variable
    v.totw.ss <- integer(tries) #Set up an empty vector to hold the 100 tries
    tmpkIC <- double(tries)
    for(i in 1:tries){
      k.temp <- kmeans(dat, centers = v) #Run kmeans
      v.totw.ss[i] <- k.temp$tot.withinss#Store the total withinss
      tmpkIC[i]  <- BIC(k.temp)
    }
    avg.totw.ss[v-1] <- mean(v.totw.ss) #Average the 100 total withinss 
    avgkIC[v-1] <-mean(tmpkIC) 
  }
  #components_number <-  sum(abs(diff(avg.totw.ss)) >= (max(abs(diff(avg.totw.ss))) * percent))
  print(avgkIC)
  components_number <- which(avgkIC == min(avgkIC)) + 1
  return(components_number)
}



tsne_plotting <- function(tsn_list){
  
  for(number in 1:length(tsn_list)){
    d_tsne_1 <- as.data.frame(tsn_list[[number]]$Y) 
    d_tsne_1 <- cbind(d_tsne_1, rowname = names_list[[number]]) 
    
    #components_number <- getClusterNumber(d_tsne_1[-3],percent)
    
    ## keeping original data
    d_tsne_1_original <-  d_tsne_1
    
    ## Creating k-means clustering model, and assigning the result to the data used to create the tsne
    #print(components_number)
    #fit_cluster_kmeans <-  kmeans(scale(d_tsne_1[-3]), components_number)  
    fit_cluster_kmeans <- fpc::kmeansruns(d_tsne_1[-3],krange=2:(nrow(d_tsne_1)/2),critout=F,runs=5,criterion="ch")
    
    colpal <- randomcoloR::distinctColorPalette(fit_cluster_kmeans$bestk)
    
    d_tsne_1_original$cl_kmeans <- factor(fit_cluster_kmeans$cluster)
    
    ## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne
    #fit_cluster_hierarchical <- hclust(dist(scale(d_tsne_1[-3])))
    
    ## setting 3 clusters as output
    #d_tsne_1_original$cl_hierarchical <-  factor(cutree(fit_cluster_hierarchical, k = fit_cluster_kmeans$bestk)) 
    
    #plot_k <- plot_cluster(d_tsne_1_original, "cl_kmeans", "Paired")  
    #plot_h <- plot_cluster(d_tsne_1_original, "cl_hierarchical", "Paired")
    
    ## and finally: putting the plots side by side with gridExtra lib...
    #grid.arrange(plot_k, plot_h,  ncol = 2)
    
    plot(d_tsne_1_original$V1, d_tsne_1_original$V2, col=colpal[fit_cluster_kmeans$cluster],pch=20,main = paste0("Co-occurrence ",gsub("resources/output/sentence//|\\_links\\.csv","",alllinks[[number]]),
                                                                                                          " k=",fit_cluster_kmeans$bestk,
                                                                                                          " ch=",max(fit_cluster_kmeans$crit)))
    text(x=d_tsne_1_original$V1, y=d_tsne_1_original$V2, cex=0.6, pos=4, labels=(d_tsne_1_original$rowname))
    #legend("topright", inset=c(-0.2,0), legend = paste("Cluster", fit_cluster_kmeans$cluster), pch=20, col=colpal[fit_cluster_kmeans$cluster], box.lty=0)
    
    # save cluster membership for confusion matrix
    write.csv2(data.frame(clusters=fit_cluster_kmeans$cluster),paste0(gsub("\\_links\\.csv","",alllinks[[number]]),"-TSNE-cluster.csv"), row.names = F, col.names = F, sep = ";")
  }
}


structuralFeatures <- function(book, nodes, links, cooc){
  
  #rownames(cooc) <- cooc[,1]
  #cooc[,1] <- NULL
  
  casc <- c()
  inter <- c()
  ent <- data.frame(ww = numeric(0), xx = numeric(0), yy = numeric(0), zz = numeric(0))
  wien <- c()
  colnames(links) <- c('source', 'target', 'tag')
  colnames(nodes) <- c('id','title','label')
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
      nextI <- digest::digest(paste(sort(unlist(strsplit(nodes[z,2], ', '))), collapse = ', '), algo = "md5")
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
        ent <- rbind(ent,c(entropy::entropy.empirical(df[,1], unit = "log2"), 1, -1 * (colSums(df)[3]), 1))
      } else{
        ent <- rbind(ent, c(entropy::entropy.empirical(df[,1], unit = "log2"), (-1 * (colSums(df)[3])) / log2(nrow(df)),-1*(colSums(df)[3]),(-1*(colSums(df)[3]))/log(nrow(df))))
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
    
    rownames(ent) <- c(1:nrow(ent))
    rownames(wien) <- c(1:nrow(wien))
    
    #add node
    g1 <- igraph::add_vertices(g1,1,attr = list(id = as.numeric(nodes[z,1])))
    
    #add all links to node
    theLinks <- unique(links[which(links[,2] == nodes[z,1]),1:2])
    for(srclnk in theLinks[,1]){
      g1 <- igraph::add_edges(g1, c(which(igraph::V(g1)$id == srclnk), which(igraph::V(g1)$id == as.numeric(nodes[z,1]))))
    }
    
    #degd <- degree.distribution(g1)
    wtc <- igraph::cluster_walktrap(g1)
    struct <- rbind(struct, c(igraph::diameter(g1), igraph::edge_density(g1), igraph::modularity(wtc), igraph::cohesion(g1)))
  }
  
  write.table(cbind(coordinates, wien, struct),
              file=paste0(book,
                          "_temporal_statistics.csv"), row.names = F, col.names = F, sep = ";")
  
  nodeFeatures <- as.data.frame(cbind(nodes$title,wien,struct[,c(2,3)]),stringsAsFactors = F)
  nodeFeatures$order <- rownames(nodeFeatures)
  colnames(nodeFeatures) <- c("terms","entropy","evenness","richness","density","modularity","order")
  nodeFeatures$order <- as.numeric(nodeFeatures$order)
  nodeFeatures$entropy <- as.numeric(nodeFeatures$entropy)
  nodeFeatures$evenness <- as.numeric(nodeFeatures$evenness)
  nodeFeatures$richness <- as.numeric(nodeFeatures$richness)
  nodeFeatures$density <- as.numeric(nodeFeatures$density)
  nodeFeatures$modularity <- as.numeric(nodeFeatures$modularity)
  nodeFeatures$entropy[which(is.nan(nodeFeatures$entropy))] <- 0
  nodeFeatures$density[which(is.nan(nodeFeatures$density))] <- 0
  nodeFeatures <- nodeFeatures[which(nodeFeatures$terms!=""),]
  for(i in 1:nrow(nodeFeatures)){
    #print(length(unlist(strsplit(nodeFeatures$terms[i],", "))))
    if(length(unlist(strsplit(nodeFeatures$terms[i],", "))) > 1){
      nextTerms <- unlist(strsplit(nodeFeatures$terms[i],", "))
      nodeFeatures$terms[i] <- nextTerms[1]
      for(j in 2:length(nextTerms)){
        nodeFeatures <- rbind(nodeFeatures,data.frame(terms=nextTerms[j],entropy=nodeFeatures$entropy[i],evenness=nodeFeatures$evenness[i],richness=nodeFeatures$richness[i],density=nodeFeatures$density[i],modularity=nodeFeatures$modularity[i],order=nodeFeatures$order[i]))
      }
    }
  }
  
  nodeFeatures <- nodeFeatures[order(nodeFeatures$order),]
  print(book)
  termFeaturesNodes <- cbind(aggregate(entropy ~ terms,nodeFeatures,mean),#FUN = function(x){mean(diff(x))}),
                             aggregate(evenness ~ terms,nodeFeatures,mean),
                             aggregate(richness ~ terms,nodeFeatures,mean),
                             aggregate(density ~ terms,nodeFeatures,mean),
                             aggregate(modularity ~ terms,nodeFeatures,mean))
  termFeaturesNodes$entropy[which(is.nan(termFeaturesNodes$entropy))] <- 0
  termFeaturesNodes <- termFeaturesNodes[with(termFeaturesNodes, order(terms)), ]
  
  #ktmp <- kmeans(scale(termFeaturesNodes[,-c(1,3,5,7,9)]),getClusterNumber(termFeaturesNodes[,-c(1,3,5,7,9)], percent = .05))
  #feature_perf <- c()
  #ktmp <- fpc::kmeansruns(scale(termFeaturesNodes[,2]),krange=2:12,critout=F,runs=2,criterion="ch")
  #feature_perf <- cbind(feature_perf,max(ktmp$crit))
  #ktmp <- fpc::kmeansruns(scale(termFeaturesNodes[,4]),krange=2:12,critout=F,runs=2,criterion="ch")
  #feature_perf <- cbind(feature_perf,max(ktmp$crit))
  #ktmp <- fpc::kmeansruns(scale(termFeaturesNodes[,6]),krange=2:12,critout=F,runs=2,criterion="ch")
  #feature_perf <- cbind(feature_perf,max(ktmp$crit))
  #ktmp <- fpc::kmeansruns(scale(termFeaturesNodes[,8]),krange=2:12,critout=F,runs=2,criterion="ch")
  #feature_perf <- cbind(feature_perf,max(ktmp$crit))
  #ktmp <- fpc::kmeansruns(scale(termFeaturesNodes[,10]),krange=2:12,critout=F,runs=2,criterion="ch")
  #feature_perf <- cbind(feature_perf,max(ktmp$crit))
  #which(feature_perf == max(feature_perf))
  
  # test <- Rtsne::Rtsne(scale(termFeaturesNodes[,-c(1,3,5,7,9)]),check_duplicates=FALSE,
  #                       pca=TRUE, perplexity = 5, theta = 0.5, dims = 2)
  # d_tsne_1_original <-  test$Y
  # #
  # fit_cluster_kmeans <- fpc::kmeansruns(scale(d_tsne_1_original),krange = 2:(nrow(d_tsne_1_original)/2),critout=F,runs=5,criterion="ch")
  # colpal <- randomcoloR::distinctColorPalette(fit_cluster_kmeans$bestk)
  # plot(d_tsne_1_original[,1], d_tsne_1_original[,2], col=colpal[fit_cluster_kmeans$cluster],pch=20,main = paste0("Node features TSNE ",book,
  #                                                                                                               " k=",fit_cluster_kmeans$bestk,
  #                                                                                                               " ch=",max(fit_cluster_kmeans$crit)))
  # text(x=d_tsne_1_original[,1], y=d_tsne_1_original[,2], cex=0.6, pos=4, labels=termFeaturesNodes$terms)
  # ktmp <- fpc::kmeansruns(scale(termFeaturesNodes[,-c(1,3,5,7,9)]),krange=2:(nrow(termFeaturesNodes[,-c(1,3,5,7,9)])/2),critout=F,runs=2,criterion="ch")
  # foo <- scale(termFeaturesNodes[-c(1,3,5,7,9)])
  # colpal <- randomcoloR::distinctColorPalette(ktmp$bestk)
  # PCA <-prcomp(foo)$x
  # plot(PCA, col=colpal[ktmp$cluster],pch=20,main = paste0("Node features ",book,
  #                                                        " k=",ktmp$bestk,
  #                                                         " ch=",max(ktmp$crit)))
  # text(x=PCA[,1], y=PCA[,2], cex=0.6, pos=4, labels=(termFeaturesNodes$terms))
  # plot(PCA[,2],PCA[,3], col=colpal[ktmp$cluster],pch=20,main = paste0("Node features ",book,
  #                                                                      " k=",ktmp$bestk,
  #                                                                      " ch=",max(ktmp$crit)))
  # text(x=PCA[,2], y=PCA[,3], cex=0.6, pos=4, labels=(termFeaturesNodes$terms))
  # plot(PCA[,1],PCA[,3], col=colpal[ktmp$cluster],pch=20,main = paste0("Node features ",book,
  #                                                                      " k=",ktmp$bestk,
  #                                                                      " ch=",max(ktmp$crit)))
  # text(x=PCA[,1], y=PCA[,3], cex=0.6, pos=4, labels=(termFeaturesNodes$terms))
  # 
  # # save cluster membership for confusion matrix
  # write.csv2(data.frame(clusters=ktmp$cluster),paste0(book,"-nodeFeatures-cluster.csv"), row.names = F, col.names = F, sep = ";")

  # further analysis tests
  
  links$dist <- links$target-links$source
  links$hlp <- 1
  linkFeatures <- cbind(aggregate(dist ~ tag,links,mean),aggregate(hlp ~ tag,links,sum))
  rownames(linkFeatures) <- linkFeatures$tag
  
  # what recurrs but what does not co-occur
  recNOTco <- linkFeatures$tag[-which(linkFeatures$tag %in% rownames(cooc))]
  
  # what co-occurs but doesn't recur
  coNOTrec <- rownames(cooc)[-which(rownames(cooc) %in% linkFeatures$tag)]
  
  #what does recur and co-occur
  corec <- rownames(cooc)[which(rownames(cooc) %in% linkFeatures$tag)]
  
  #all terms
  fusedTerms <- as.data.frame(sort(unique(c(recNOTco,coNOTrec,corec))),stringsAsFactors = F)
  colnames(fusedTerms) <- c("terms")
  
  #what does occur but neither recur nor co-occur
  occ <- as.data.frame(unique(termFeaturesNodes$terms[which(!termFeaturesNodes$terms %in% fusedTerms$terms)]),stringsAsFactors = F)
  colnames(occ) <- c("terms")
  fusedTerms <- rbind(fusedTerms,occ)
  fusedTerms$terms <- sort(fusedTerms$terms)
  
  rownames(fusedTerms) <- fusedTerms$terms
  fusedTerms$coocs <- 0
  fusedTerms$distcoocs <- 0
  fusedTerms$recs <- 0
  fusedTerms$distrecs <- 0
  
  #co-occ featrues
  fusedTerms[coNOTrec,2] <- colSums(cooc[coNOTrec])
  fusedTerms[corec,2] <- colSums(cooc[corec])
  
  fusedTerms[coNOTrec,3] <- colSums(cooc[coNOTrec] > 0)
  fusedTerms[corec,3] <- colSums(cooc[corec] > 0)
  
  fusedTerms$coocProp <- 0
  fusedTerms[which(fusedTerms$coocs>0),6] <- fusedTerms[which(fusedTerms$coocs>0),3] / fusedTerms[which(fusedTerms$coocs>0),2]
  
  fusedTerms[corec,4] <- linkFeatures[corec,4]
  fusedTerms[recNOTco,4] <- linkFeatures[recNOTco,4]
  
  fusedTerms[corec,5] <- linkFeatures[corec,2]
  fusedTerms[recNOTco,5] <- linkFeatures[recNOTco,2]
  
  structuralFeatures <- cbind(fusedTerms,termFeaturesNodes)
  
  
  #ktmp <- kmeans(scale(structuralFeatures[,-c(1,2,3,7,9,11,13,15)]),getClusterNumber(structuralFeatures[,-c(1,2,3,7,9,11,13,15)], percent = .05))
  # test <- Rtsne::Rtsne(scale(structuralFeatures[,-c(1,2,3,7,9,11,13,15)]),check_duplicates=FALSE,
  #                      pca=TRUE, perplexity = 2, theta=0.5, dims=2)
  # d_tsne_1_original <-  test$Y
  # fit_cluster_kmeans <- fpc::kmeansruns(scale(d_tsne_1_original),krange=2:(nrow(d_tsne_1_original)/2),critout=F,runs=5,criterion="ch")
  # colpal <- randomcoloR::distinctColorPalette(fit_cluster_kmeans$bestk)
  # plot(d_tsne_1_original[,1], d_tsne_1_original[,2], col=colpal[fit_cluster_kmeans$cluster],pch=20,main = paste0("Combined TSNE ",book,
  #                                                                                                              " k=",fit_cluster_kmeans$bestk,
  #                                                                                                              " ch=",max(fit_cluster_kmeans$crit)))
  # text(x=d_tsne_1_original[,1], y=d_tsne_1_original[,2], cex=0.6, pos=4, labels=rownames(structuralFeatures))
  # 
  # ktmp <- fpc::kmeansruns(scale(structuralFeatures[,-c(1,2,3,7,9,11,13,15)]),krange=2:(nrow(structuralFeatures[,-c(1,2,3,7,9,11,13,15)])/2),critout=F,runs=2,criterion="ch")
  # 
  # colpal <- randomcoloR::distinctColorPalette(ktmp$bestk)
  # 
  # foo <- scale(structuralFeatures[,-c(1,2,3,7,9,11,13,15)])
  # PCA <-prcomp(foo)$x
  # plot(PCA, col=colpal[ktmp$cluster],pch=20,main = paste0("Combined features ",book,
  #                                                         " k=",ktmp$bestk,
  #                                                         " ch=",max(ktmp$crit)))
  # text(x=PCA[,1], y=PCA[,2], cex=0.6, pos=4, labels=(row.names(foo)))
  # plot(PCA[,2],PCA[,3], col=colpal[ktmp$cluster],pch=20,main = paste0("Combined features ",book,
  #                                                                     " k=",ktmp$bestk,
  #                                                                     " ch=",max(ktmp$crit)))
  # text(x=PCA[,2], y=PCA[,3], cex=0.6, pos=4, labels=(row.names(foo)))
  # plot(PCA[,1],PCA[,3], col=colpal[ktmp$cluster],pch=20,main = paste0("Combined features ",book,
  #                                                                     " k=",ktmp$bestk,
  #                                                                     " ch=",max(ktmp$crit)))
  # text(x=PCA[,1], y=PCA[,3], cex=0.6, pos=4, labels=(row.names(foo)))
  # 
  # 
  # # save cluster membership for confusion matrix
  # write.csv2(data.frame(clusters=ktmp$cluster),paste0(book,"-allFeatures-cluster.csv"), row.names = F, col.names = F, sep = ";")
  write.csv2(structuralFeatures,paste0(book,"-allFeatures-output.csv"), row.names = F, col.names = F, sep = ";")
}
# Start of the Code -------------------------------------------------------


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
  
  # rare co-occurring terms
  #rareT <- which(rowSums(cooc[[bb]]) <= ceiling(max(rowSums(cooc[[bb]])) * .15))
  # frequent co-occ terms
  #freqT <- which(rowSums(cooc[[bb]]) > ceiling(max(rowSums(cooc[[bb]])) * .15))
  # rare terms only co-occurring with rare terms
  #finalRare <- which(colSums(cooc[[bb]][freqT,]) == 0)
  
  #cooc[[bb]] <- cooc[[bb]][-finalRare,-finalRare]
  
  #if(length(which(rowSums(cooc[[bb]])==0)) > 0){
  #  cooc[[bb]] <- cooc[[bb]][-(which(rowSums(cooc[[bb]])==0)),-(which(rowSums(cooc[[bb]])==0))]
  #}
  #cooc[[bb]] <- cooc[[bb]][-which(rowSums(cooc[[bb]]) < ceiling(max(rowSums(cooc[[bb]])) * .10)),
  #                         -which(colSums(cooc[[bb]]) < ceiling(max(colSums(cooc[[bb]])) * .10))]
  
}

tsn_list <- list()
names_list <- list()
for(sent in cooc){

  test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                     pca=TRUE, perplexity = max(1,floor(length(rowSums(sent))/3)-1), theta=0, dims=2)
  
  #plot(test$Y)
  #text(test$Y, labels=rownames(sent))
  
  tsn_list[[length(tsn_list) + 1]] <- test
  names_list[[length(names_list) + 1]] <- rownames(sent)
}


tsne_plotting(tsn_list)

# structural feature analysis
for(nextBook in 1:length(link)){
  structuralFeatures(gsub("\\_links\\.csv","",alllinks[[nextBook]]),node[[nextBook]],link[[nextBook]],cooc[[nextBook]])
}

#random forest example

# rf.fit <- randomForest::randomForest(x = cooc[[1]], y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
# hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
# rf.cluster = cutree(hclust.rf, k=3)
# metricsgraphics::mjs_plot(rf.cluster, x=PC1, y=PC2) %>%
#   metricsgraphics::mjs_point(color_accessor=rf.clusters) %>%
#   metricsgraphics::mjs_labs(x="principal comp 1", y="principal comp 2")
# 
# 
# #model based example
# library(mclust)
# fit <- mclust::Mclust(cooc[[1]])
# plot(fit) # plot results 
# summary(fit) # display the best model
# #evaluate cluster solutions
# #cluster.stats(cooc[[1]], fit1$cluster, fit2$cluster)
# 
# # testing some recurrence network analysis
# g <- igraph::graph_from_data_frame(link[[1]])
# igraph::E(g)$weight <- link[[1]]$target-link[[1]]$source
# g_mat <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")
# test <- Rtsne::Rtsne(g_mat,check_duplicates=FALSE,
#                      pca=TRUE, perplexity = 2, theta=0.5, dims=2)
# plot(test$Y)
# text(test$Y, labels=rownames(g_mat))
