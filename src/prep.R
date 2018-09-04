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
        trait_words <- tolower(readLines('../resources/Personal Traits.txt'))
    
        outer_lapply <- lapply(words300B, function(xx){
            xx <- gsub(",|\\.|;|:|\\'\\\\\"",'', xx)
            xx <- unlist(strsplit(xx, split = ' '))
            matched <- match(trait_words,xx)
            trait_words[which(!is.na(matched))]
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
    write.table(nodes,file=paste0("../resources/output/", sliceSize,"/",
                                  theSource,"_nodes.csv"), row.names = F)
    write.table(links,file=paste0("../resources/output/", sliceSize,"/",
                                  theSource,"_links.csv"), row.names = F)
    write.table(roots,file=paste0("../resources/output/", sliceSize,"/",
                                  theSource,"_roots.csv"), row.names = F)
print(paste0(theSource,"1"))
    
    uniqueLinks <- data.frame(id1 = character(0),
                              id2=character(0),
                              label = character(0))
    convLinks <- as.data.frame(links, stringsAsFactors = F)
    for(z in 1:nrow(convLinks)){
      uniqueLinks <- rbind(uniqueLinks,
                           data.frame(id1=unlist(convLinks[z,1]),
                                      id2=unlist(convLinks[z,2]),
                                      label=paste(convLinks[which(unlist(convLinks[,1])==unlist(convLinks[z,1]) & unlist(convLinks[,2])==unlist(convLinks[z,2])),3],collapse=', '),stringsAsFactors = F))
    }
    uniqueLinks <- unique(uniqueLinks)
    colnames(uniqueLinks) <- c("id1","id2","label")
    g <- graph.data.frame(uniqueLinks,directed = TRUE)
    
    nLabels <- c()
    for(z in V(g)$name){
      nLabels <- c(nLabels,paste(unique(unlist(strsplit(paste(uniqueLinks[which(uniqueLinks$id1==z | uniqueLinks$id2==z),3],collapse = ', '),', '))),collapse = ', '))
    }
   print(paste0(theSource,"2"))
    
    deg <-  igraph::degree(g, mode="all")
    
    degd <- degree.distribution(g)
    wtc <- cluster_walktrap(g)
    gstat <- c(diameter(g), min(degd), max(degd), mean(degd), edge_density(g), modularity(wtc))
    write.table(c(theSource,gstat), paste0("../resources/output/", sliceSize, "/", theSource,"_netstat_combined.csv"), append = T, col.names = F, row.names = F, sep = ";")
    write.table(gstat, paste0("../resources/output/", sliceSize,"/", theSource,"_netstat.csv"),  col.names = F, row.names = F, sep = ";")
    
    nodes <- as.data.frame(nodes, stringsAsFactors=F)
    colnames(nodes) <- c('id','title','label')
print(paste0(theSource,"3"))
    
#### create the character network from the cascade
     socN1 <- c()
     counter_lin <- 0
     for(lin in 1:nrow(nodes)){
       nex <- unlist(strsplit(nodes$title[lin], ", "))
       if(length(nex) > 1){
         socN1 <- rbind(socN1, paste(nex, collapse = ', '))


         socEdges <- lapply(socN1, function(lin){
           templin <- unlist(strsplit(lin, ', '))
           gtools::combinations(length(templin),
                                       2, templin)
         })
       }
     }
    socEdges <- do.call("rbind", socEdges)

    write.table(unique(socEdges), file = paste0("../resources/output/",
                                         sliceSize,"/", theSource,
                                         "_socnet_edgelist.csv"), sep = ';',
                                         row.names = F, col.names = F)
    socEdges <- plyr::count(socEdges)
    colnames(socEdges) <- c("id1", "id2", "label")
    h <- graph.data.frame(socEdges, directed = F)
print(paste0(theSource,"3.5"))
    E(h)$weight <- E(h)$label
    netm <- as_adjacency_matrix(h, attr = "weight", sparse = F)
print(paste0(theSource,"3.7"))
    colnames(netm) <- V(h)$name
    rownames(netm) <- V(h)$name
    write.table(netm, paste0("../resources/output/",
                             sliceSize,"/",theSource,"_network_matrix.csv"), 
                col.names = NA, row.names = T,
                fileEncoding = "UTF-8",
                sep = " ")
print(paste0(theSource,"4"))
  }  
}
