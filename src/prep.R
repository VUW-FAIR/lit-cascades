# Preprocessing script for a tool prototype that enables close and distant reading of literature.
# For demonstration purposes the tool is configured to work on various Victorian Novels from Charles Dickens and other authors.
# Authors: Markus Luczak-Roesch, Tom Goldfinch, Johannes A. Karl

# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
#                                               "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

#import libs
library(plyr)
library(tm)
library(stringr)
library(RTextTools)
library(digest)
library(entropy)
library(scatterplot3d)
library(RColorBrewer)
library(tidyr)
library(igraph)
library(ggplot2)
library(plotly)
library(gtools)
library(openNLP)
library(RWeka)
library(vegan)
library(poweRlaw)

#housekeeping and helpers
options(scipen = 999)

# Functions ---------------------------------------------------------------


degree.distribution <- function (graph, cumulative = FALSE, ...) 
{
  if (!is.igraph(graph)) {
    stop("Not a graph object")
  }
  cs <- igraph::degree(graph, ...)
  hi <- hist(cs, -1:max(cs), plot = FALSE)$count
  if (!cumulative) {
    res <- hi
  }
  else {
    res <- rev(cumsum(rev(hi)))
  }
  res
}
## TIC
tic_generate <- function(inputsequence) {
      nodes <- c()
      links <- c()
      roots <- c()
      last_node <- list()
      
      for(pp in 1:nrow(inputsequence)){
        tags <- unlist(strsplit(as.character(inputsequence[which(inputsequence[,1] == pp),2]), split=", "))
        nodes <- rbind(nodes, c(inputsequence[which(inputsequence[,1]== pp),1],
                                as.character(inputsequence[which(inputsequence[,1] == pp),2]),
                                inputsequence[which(inputsequence[,1] == pp),1]))
        
        for(jj in 1:length(tags)){
          cur_tag <- tags[jj]
          if(!is.null(unlist(last_node[cur_tag]))){ 
            source_node <- last_node[cur_tag]
            target_node <- pp
            links <- rbind(links, c(source_node, target_node, cur_tag))
          } else {
            roots <- rbind(roots, c(pp, cur_tag))
          }
          last_node[cur_tag] <- pp
        }
        
      }
      return(list(nodes, links, roots))
}

#set working directoy
setwd("C:/Users/Johannes.Karl/Documents/GitHub/tic-personality-words/src")

#get all text and char files
allTextFiles <- list.files("../resources/Text Files")

for(sliceSize in list(1000, "sentence")){
  dir.create(file.path("../resources/output/", sliceSize), showWarnings = FALSE)
  
  for(nextRun in 1:length(allTextFiles)){
    theSource <- gsub(' ','_', gsub('[[:digit:]][[:digit:]] ','',
                                    gsub(' text.txt','',allTextFiles[nextRun])))

    sourceText <- readChar(paste0('../resources/Text Files/',allTextFiles[nextRun]),
                           file.info(paste0('../resources/Text Files/',allTextFiles[nextRun]))$size)
    processedText <- gsub("\\r\\n", " ", sourceText, perl = T)
    processedText <- gsub("\\n", " ", processedText, perl = T)
    
    
    #split by number of words and chapters
    if(is.numeric(sliceSize)){
    full_text <- strsplit(processedText, "(?i:Chapter [0-9A-Z]+[\\.]?[\\s.]?)", perl=T)[[1]]
    words300B <- c()
    tmp <- lapply(full_text, function(x){
      snippet <- strsplit(x, "\\s+")[[1]]
      if(length(snippet > 1)){
        groupA <- rep(seq(ceiling(length(snippet) / sliceSize)), each = sliceSize)[1:length(snippet)]
        words300A <- split(snippet, groupA)
        words300B <- c(words300B, words300A)
        words300B
      }
    })
        for(s in 1:length(tmp)){
      if(length(tmp[[s]]) > 0){
        words300B <- c(words300B,tmp[[s]])
      }
    }
    
    } else if (sliceSize == "sentence"){
      sent_token_annot <- openNLP::Maxent_Sent_Token_Annotator()
      NLP_text <- NLP::as.String(processedText)
      sent_annotate <- NLP::annotate(NLP_text, sent_token_annot)
      sentences <- NLP_text[sent_annotate]
      words300B <- sentences
    }
    

        #character list
        trait_words <- tolower(readLines('../resources/pda500.txt'))
    
        outer_lapply <- lapply(words300B, function(xx){
            inner_lapply <- Filter(Negate(is.null), lapply(trait_words, function(kk){
            matched <- gregexpr(paste("[\\s\\\\\"](", kk, ")[\\';,.:\\s\\\\\"]", sep=""),
                                       paste(xx, collapse = ' '), perl = TRUE)
            if(attr(matched[[1]], "match.length") != -1){
              kk
            }
          }))
          inner_lapply
        })
      
      
       unique_trait <- lapply(outer_lapply, function(tt){
          paste(sort(unique(unlist(tt))), collapse = ", ")
      }) 
      charDS <- data.frame(x = c(1:length(unique_trait)),
                           y = unlist(unique_trait),
                           stringsAsFactors = FALSE)
      

    
    tic <- tic_generate(charDS)
    
## Extracting nodes, links, roots from network model computed in tic_generate.
    nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tags = unlist(tic[[1]][,2]),
                        dpub = unlist(tic[[1]][,3]), stringsAsFactors = F)
    links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                        tag = unlist(tic[[2]][,3]), stringsAsFactors = F)
    roots <- data.frame(root_node_id = unlist(tic[[3]][,1]),
                        tag = unlist(tic[[3]][,2]), stringsAsFactors = F)

# TIC Statistics ----------------------------------------------------------
## Can we replace this with a lapply call for a list, saves two lines
    write.table(nodes,file=paste0("../resources/output/", sliceSize,"/", theSource,"_nodes.csv"), row.names = F)
    write.table(links,file=paste0("../resources/output/", sliceSize,"/", theSource,"_links.csv"), row.names = F)
    write.table(roots,file=paste0("../resources/output/", sliceSize,"/", theSource,"_roots.csv"), row.names = F)
    
    linksDelta <- as.integer(links$target) - as.integer(links$source)
    ggsave(paste0("../resources/output/",sliceSize,"/",theSource,"_links_delta.jpg"),
           plot = as.data.frame(linksDelta) %>% 
             mutate(., row = c(1:nrow(.))) %>%
             ggplot() +
             aes(x = row , y = linksDelta) +
             geom_histogram(stat = "identity") +
             NULL)
    linksDelta.count <- plyr::count(linksDelta)
    ggsave(paste0("../resources/output/",sliceSize,"/",theSource,"_links_delta_distri.jpg"),
           plot =     ggplot(linksDelta.count) +
             aes(x = x , y = freq) +
             geom_point() +
             NULL)

    ggsave(paste0("../resources/output/",sliceSize,"/",theSource,"_links_delta_distri_loglog.jpg"),
           plot = ggplot(linksDelta.count) +
             aes(x = x , y = freq) +
             geom_point() +
             scale_x_log10() +
             scale_y_log10() +
             NULL)  
    #power law?
    
    m_bl = poweRlaw::displ$new(linksDelta)
    est = poweRlaw::estimate_xmin(m_bl)
    m_bl$setXmin(est)
    m_ln = poweRlaw::dislnorm$new(linksDelta)
    est = poweRlaw::estimate_xmin(m_ln)
    m_ln$setXmin(est)
    m_pois = poweRlaw::dispois$new(linksDelta)
    est = poweRlaw::estimate_xmin(m_pois)
    m_pois$setXmin(est)
    
    jpeg(paste0("../resources/output/",sliceSize,"/",theSource,"_links_delta_distri_plaw.jpg"))
    plot(m_bl, ylab="CDF")
    text(100,0.15,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
    lines(m_bl, col=2)
    lines(m_ln, col=3)
    lines(m_pois, col=4)
    dev.off()
    
    uniqueLinks <- data.frame(id1=character(0),id2=character(0),label=character(0))
    convLinks <- as.data.frame(links,stringsAsFactors = F)
    for(z in 1:nrow(convLinks)){
      uniqueLinks <- rbind(uniqueLinks,data.frame(id1=unlist(convLinks[z,1]),id2=unlist(convLinks[z,2]),label=paste(convLinks[which(unlist(convLinks[,1])==unlist(convLinks[z,1]) & unlist(convLinks[,2])==unlist(convLinks[z,2])),3],collapse=', '),stringsAsFactors = F))
    }
    uniqueLinks <- unique(uniqueLinks)
    colnames(uniqueLinks) <- c("id1","id2","label")
    g <- graph.data.frame(uniqueLinks,directed = TRUE)
    
    nLabels <- c()
    for(z in V(g)$name){
      nLabels <- c(nLabels,paste(unique(unlist(strsplit(paste(uniqueLinks[which(uniqueLinks$id1==z | uniqueLinks$id2==z),3],collapse = ', '),', '))),collapse = ', '))
    }
    
    V(g)$content <- nLabels
    
    V(g)$frame.color <- "white"
    V(g)$color <- "orange"
    
    deg <-  igraph::degree(g, mode="all")
    V(g)$size <- deg * 3
    
    E(g)$width <- 0.1

    minC <- rep(-Inf, vcount(g))
    maxC <- rep(Inf, vcount(g))
    minC[1] <- maxC[1] <- 0
    co <- layout_with_fr(g, minx = minC, maxx = maxC,
                         miny = minC, maxy = maxC)
    
    pdf(paste("../resources/output/",sliceSize,"/",theSource,"_gutenberg_dh_net.pdf",sep=''))
    plot(g, layout = co, vertex.size = 2, vertex.label.cex = 0.2, edge.label.cex = 0.2,
         edge.arrow.size = 0.1, rescale = TRUE, vertex.label.dist = 0)
    dev.off()
    
    deg <-  igraph::degree(g, mode="all")
    pdf(paste("../resources/output/",sliceSize,"/",theSource,"_gutenberg_dh_net_degdistri.pdf",sep=''))
    deg.dist <- degree_distribution(g, cumulative = T, mode = "all")
    plot( x = 0:max(deg), y = 1-deg.dist, pch = 19, cex = 1.2, col = "orange",
          xlab = "Degree", ylab = "Cumulative Frequency")
    dev.off()
    
    degd <- degree.distribution(g)
    wtc <- cluster_walktrap(g)
    gstat <- c(diameter(g), min(degd), max(degd), mean(degd), edge_density(g), modularity(wtc))
    write.table(c(theSource,gstat), paste0("../resources/output/", sliceSize, "/", theSource,"_netstat_combined.csv"), append = T, col.names = F, row.names = F, sep = ";")
    write.table(gstat, paste0("../resources/output/", sliceSize,"/", theSource,"_netstat.csv"),  col.names = F, row.names = F, sep = ";")
    
    nodes <- as.data.frame(nodes, stringsAsFactors=F)
    colnames(nodes) <- c('id','title','label')

    
#### create the character network from the cascade
     socN1 <- c()
     counter_lin <- 0
     for(lin in 1:nrow(nodes)){
       nex <- unlist(strsplit(nodes$title[lin], ", "))
       if(length(nex) > 1){
         socN1 <- rbind(socN1, paste(nex, collapse = ', '))


         socEdges <- c()
         for(lin in 1:length(socN1)){
            socEdges <- rbind(socEdges, gtools::combinations(length(unlist(strsplit(socN1[lin], ', '))), 2, unlist(strsplit(socN1[lin],', '))))
         }}
       if(lin %% (round(nrow(nodes) / 10)) == 0){
         h <- graph.data.frame(unique(socEdges),directed=FALSE)
         V(h)$frame.color <- "white"
         V(h)$color <- "orange"

         E(h)$width <- 0.1

         minC <- rep(-Inf, vcount(h))
         maxC <- rep(Inf, vcount(h))
         minC[1] <- maxC[1] <- 0
         co <- layout_with_fr(h, minx = minC, maxx = maxC,
                              miny = minC, maxy = maxC)
         counter_lin <- counter_lin + 1
         pdf(paste0("../resources/output/", sliceSize, "/", theSource,"_gutenberg_dh_socnet_", counter_lin, ".pdf"))
                  plot(h, layout = co, 
         vertex.size = 2, vertex.label.cex = 0.2, edge.label.cex = 0.2, 
         edge.arrow.size = 0.1, rescale = TRUE, vertex.label.dist = 0)
         dev.off()
       }
     }

     h <- graph.data.frame(unique(socEdges), directed = F)

 
     write.table(unique(socEdges), file = paste0("../resources/output/",
                                            sliceSize,"/", theSource, "_socnet_edgelist.csv"), sep = ';', row.names = F, col.names = F)

     V(h)$frame.color <- "white"
     V(h)$color <- "orange"

     E(h)$width <- 0.1
    

     minC <- rep(-Inf, vcount(h))
     maxC <- rep(Inf, vcount(h))
     minC[1] <- maxC[1] <- 0
     co <- layout_with_fr(h, minx = minC, maxx = maxC,
                          miny = minC, maxy = maxC)



 
     pdf(paste("../resources/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet.pdf",sep=''))
     plot(h, layout = co, vertex.size = 2, vertex.label.cex = 0.2,
          edge.label.cex = 0.2, edge.arrow.size = 0.1, rescale = TRUE,
          vertex.label.dist = 0)
     dev.off()
    socEdges <- plyr::count(socEdges)
    colnames(socEdges) <- c("id1", "id2", "label")
    h <- graph.data.frame(socEdges, directed = F)
    E(h)$weight <- E(h)$label
    V(h)$frame.color <- "white"
    V(h)$color <- "orange"

    E(h)$width <- 0.1
    

    minC <- rep(-Inf, vcount(h))
    maxC <- rep(Inf, vcount(h))
    minC[1] <- maxC[1] <- 0
    co <- layout_with_fr(h, minx = minC, maxx = maxC,
                          miny = minC, maxy = maxC)
    
    pdf(paste("../resources/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet2.pdf",sep=''))
      plot(h, layout=co,
      vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2,
      edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
    dev.off()

    netm <- as_adjacency_matrix(h, attr = "weight", sparse = F)
    colnames(netm) <- V(h)$name
    rownames(netm) <- V(h)$name
    
    ### Trying to convert netm to a ggplotable data frame

      as.data.frame(netm) %>% 
      tibble::rownames_to_column() %>%
      gather(., ... = -rowname)  %>%
      ggplot() +
      aes(rowname, key) +
      geom_tile(aes(fill = value), color = "white") +
      scale_fill_gradient(low = "white", high = "red1") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      NULL
      ggsave(paste0("../resources/output/", sliceSize, "/", theSource, "_gutenberg_dh_socnet_heatmap.pdf"))
    
    #stats for the social graph
    degd <- degree.distribution(h)
    wtc <- cluster_walktrap(h)
    gstat <- c(diameter(h),min(degd),max(degd),mean(degd),edge_density(h),modularity(wtc))
    write.table(c(theSource,gstat),paste0("../resources/output/",sliceSize,"/",theSource,"_socnetstat_combined.csv"), append = T, col.names = F, row.names = F, sep = ";")
    write.table(gstat,paste("../resources/output/",sliceSize,"/",theSource,"_socnetstat.csv",sep=''),col.names = F,row.names = F, sep = ";")
    write.table(links[,c("target", "tag")], file = paste0("../resources/output/", sliceSize, "/", theSource, '_gutenberg_targets.txt'))
    write.table(links[,c("source", "tag")], file = paste0("../resources/output/", sliceSize, "/", theSource, "_gutenberg_sources.txt"))
    
    ####
    links <- links %>%
             mutate(rownumber = seq.int(nrow(.)))
    ggsave(paste0("../resources/output/",sliceSize,"/",theSource,"_gutenberg_links_source_nrow.jpg"),
           plot = links %>%
                  ggplot() +
                  aes(x = target, y = rownumber, group = 1) +
                  geom_point() +
                  labs(y = "Rownumber") +
                  theme_classic())
    
    ggsave(paste0("../resources/output/",sliceSize,"/",theSource,"_gutenberg_links_targets.jpg"),
           plot = links %>%
                  ggplot() +
                  aes(x = target) +
                  geom_bar() +
                  labs(y = "Count") +
                  theme_classic())
    
    ggsave(paste0("../resources/output/",sliceSize,"/",theSource,"_gutenberg_links_sources.jpg"),
           plot = links %>%
             ggplot() +
             aes(x = source) +
             geom_bar() +
             labs(y = "Count") +
             theme_classic())



    
    
    casc <- c()
    inter <- c()
    ent <- data.frame(ww = numeric(0), xx = numeric(0), yy = numeric(0), zz = numeric(0))
    wien <- c()
    colnames(links) <- c('source', 'target', 'tag')
    
    coordinates <- c()
    spec <- list()
    div = 1
    
    g1 <- make_empty_graph(n = 0, directed = TRUE)
    struct <- c()
    
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
        for(v in 1:nrow(inter)){
          interactions <- unlist(strsplit(unlist(inter[v,1]),', '))
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
      g1 <- add_vertices(g1,1,attr = list(id = as.numeric(nodes[z,1])))
      
      #add all links to node
      theLinks <- unique(links[which(links[,2] == nodes[z,1]),1:2])
      for(srclnk in theLinks[,1]){
        g1 <- add_edges(g1, c(which(V(g1)$id == srclnk), which(V(g1)$id == as.numeric(nodes[z,1]))))
      }
      
      #degd <- degree.distribution(g1)
      wtc <- cluster_walktrap(g1)
      struct <- rbind(struct, c(diameter(g1), edge_density(g1), modularity(wtc)))
    }
    #colnames(coordinates) <- c("t","specificity","diversity")
    #data.frame(coordinates) %>%
      #ggplot() +
      #aes(x = specificity, y = diversity, colour = t) +
      #geom_jitter()
    
    
    jpeg(paste("../resources/output/",sliceSize,"/",theSource,"_gutenberg_coordinates.jpg",sep=''))
    scatterplot3d(coordinates[,2],coordinates[,1],coordinates[,3],pch=16, highlight.3d=TRUE,type="h",xlab="Specificity",ylab="Node index",zlab="Diversity")
    dev.off()
    
    write.table(cbind(coordinates, wien, struct),
                file=paste0("../resources/output/", sliceSize, "/", theSource,
                            "_temporal_statistics.csv"), row.names = F, col.names = F, sep = ";")
    
    write.table(ent, file = paste0("../resources/output/", sliceSize, "/",
                                   theSource, "_gutenberg_entropy.txt"), sep = ";")
    
    write.table(wien, file = paste0("../resources/output/", sliceSize, "/",
                                    theSource, "_gutenberg_diversity.txt"), sep = ";")
    ent_plot <- as.data.frame(ent) %>%
                mutate(rownumber = seq.int(nrow(.)))
    
    wien_plot <- as.data.frame(wien) %>%
      mutate(rownumber = seq.int(nrow(.)))
    
    ggsave(paste0("../resources/output/", sliceSize, "/", theSource,"_gutenberg_entropy.jpg"),
           plot = ent_plot %>%
                  ggplot() +
                  aes(x = rownumber, y = .$empEntropy, group = 1) +
                  geom_line() +
                  labs(y = "Entropy") +
                  theme_classic())
    
    ggsave(paste0("../resources/output/", sliceSize, "/", theSource,"_gutenberg_evenness.jpg"),
           plot = ent_plot %>%
                  ggplot() +
                  aes(x = rownumber, y = .$evenness_log2, group = 1) +
                  geom_line() +
                  labs(y = "Log Evenness") +
                  theme_classic())

    ggsave(paste0("../resources/output/", sliceSize, "/",
                  theSource, "_gutenberg_shannonwiener.jpg"),
           plot = wien_plot %>%
                  ggplot() +
                  aes(x = rownumber, y = .$ShannonWiener, group = 1) +
                  geom_line() +
                  labs(y = "Shannon Wiener") +
                  theme_classic())
    
    ggsave(paste0("../resources/output/",sliceSize,"/",theSource,"_gutenberg_pielou.jpg"),
           plot = wien_plot %>%
                  ggplot() +
                  aes(x = rownumber, y = .$Pielou, group = 1) +
                  geom_line() +
                  labs(y = "Pielou") +
                  theme_classic())

    ggsave(paste0("../resources/output/",sliceSize,"/",theSource,"_gutenberg_richness.jpg"),
           plot = wien_plot %>%
                  ggplot() +
                  aes(x = rownumber, y = .$Richness, group = 1) +
                  geom_line() +
                  labs(y = "Richness") +
                  theme_classic())

  }  
}
