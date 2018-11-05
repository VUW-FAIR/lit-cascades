coreNLP::initCoreNLP()
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
library(rJava)


#housekeeping and helpers
options(scipen = 999)

neg <- c("not", "never", "scarcely", "seldom", "barely", "rarely", "infrequently")
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
setwd("/home/STAFF/karljo/tic-personality-words/")

#get all text and char files
allTextFiles <- list.files("resources/Text Files")

for(sliceSize in list(1000, "sentence")){
  dir.create(file.path("resources/output/", sliceSize), showWarnings = FALSE)
  
  for(nextRun in 1:length(allTextFiles)){
    theSource <- gsub(' ','_', gsub('[[:digit:]][[:digit:]] ','',
                                    gsub(' text.txt','',allTextFiles[nextRun])))
    
    sourceText <- readChar(paste0('resources/Text Files/',allTextFiles[nextRun]),
                           file.info(paste0('resources/Text Files/',allTextFiles[nextRun]))$size)
    processedText <- gsub("\\r\\n", " ", sourceText, perl = T)
    processedText <- gsub("\\n", " ", processedText, perl = T)
    
    sent_token_annot <- openNLP::Maxent_Sent_Token_Annotator()
    NLP_text <- NLP::as.String(processedText)
    sent_annotate <- NLP::annotate(NLP_text, sent_token_annot)
    words300B <- NLP_text[sent_annotate]

  }
}