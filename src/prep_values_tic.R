# Preprocessing script for a tool prototype that enables analysis capable of identifying salient psychological and narrative 
# dimensions associated with cultural values, personality, emotions and morality in texts
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

#set working directoy
setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/")

#Rprof(profile_out <- "profile.txt")

#import libs
#coreNLP::initCoreNLP(type = c("english"),mem="96g")

#housekeeping and helpers
options(scipen = 999)
allwords <- FALSE
stemTrait <- FALSE
# Functions ---------------------------------------------------------------


degree.distribution <- function (graph, cumulative = FALSE, ...) 
{
  if (!igraph::is.igraph(graph)) {
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

#get all text and char files
allTextFiles <- list.files("resources/Text Files")

#for(sliceSize in list("sentence")){
  #for(sliceSize in list(1000)){
  for(sliceSize in list(1000,"sentence")){
  dir.create(file.path("resources/output/", sliceSize), showWarnings = FALSE)
  
  for(nextRun in 1:length(allTextFiles)){
    theSource <- gsub(' ','_', gsub('[[:digit:]][[:digit:]] ','',
                                    gsub(' text.txt','',allTextFiles[nextRun])))
    
    sourceText <- readChar(paste0('resources/Text Files/',allTextFiles[nextRun]),
                           file.info(paste0('resources/Text Files/',allTextFiles[nextRun]))$size)
    processedText <- gsub("\\r\\n", " ", sourceText, perl = T)
    processedText <- gsub("\\n", " ", processedText, perl = T)
    
    
    #split by number of words and chapters
    if(is.numeric(sliceSize)){
      full_text <- strsplit(processedText, "(?i:Chapter [0-9A-Z]+[\\.]?[\\s.]?)", perl=T)[[1]]
      #corpus <- tm::VCorpus(tm::VectorSource(processedText))
      #corpus <- tm::tm_map(corpus, tm::content_transformer(tm::removePunctuation))
      #corpus <- tm::tm_map(corpus, tm::content_transformer(tm::removeNumbers))
      #tdm <- as.matrix(tm::TermDocumentMatrix(corpus,
      #                                        control = list(wordLength = c(3, Inf))))
      #random500 <- sample(rownames(tdm), 500)
      #readr::write_lines(random500, paste0("resources/output/", theSource,"_random.csv"))
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
          for(t in 1:length(tmp[[s]])){
            words300B <- c(words300B,paste(unlist(tmp[[s]][t]),collapse = " "))
          }
        }
      }
      rm(tmp)
      gc()
      
    } else if (sliceSize == "sentence"){
      sent_token_annot <- openNLP::Maxent_Sent_Token_Annotator()
      NLP_text <- NLP::as.String(processedText)
      sent_annotate <- NLP::annotate(NLP_text, sent_token_annot)
      sentences <- NLP_text[sent_annotate]
      words300B <- sentences
      write.csv(words300B, paste0("resources/output/", sliceSize,"/", theSource,"_sentences.csv"),  col.names = F, row.names = F, sep = ";")
      rm(NLP_text)
      rm(sent_annotate)
      gc()
    }
    
    
    #character list
    # random word lists -> allwords <- TRUE
    #trait_words <- tolower(readLines('resources/random-500.txt'))
    #trait_words <- tolower(readLines(paste0("resources/output/",theSource,"_random.csv")))
    
    #trait_words <- gsub("\\s*", "", tolower(readLines('resources/Personal Traits.txt')))
    #trait_words <- tolower(readLines('resources/pda500.txt'))
    spacyr::spacy_initialize()
    #trait_words <- unique(tolower(readLines('resources/pda1710_no_abbreviation.csv')))
    trait_words <- unique(tolower(read.table('resources/values.txt',sep=";")$V1))
    
    outer_lapply <- lapply(words300B, function(xx){
      result = tryCatch({
        
        #if(stemTrait){
        #  tm <- tm::Corpus(tm::VectorSource(xx))
        #  tm <- tm::tm_map(tm, tm::stemDocument)
        #  xx <- toString(tm[[1]]$content)
        #}
        
        #annotation <- coreNLP::annotateString(xx)
        annotation <- spacyr::spacy_parse(xx,tag=T,dependency=T)
        negs <- annotation[annotation$dep_rel == "neg",]
        neg_deps <- c()
        for(ne in 1:nrow(negs)){
          neg_deps <- c(neg_deps, annotation[which(annotation$sentence_id==negs[ne,2] & annotation$token_id==negs[ne,8]),4])
        }
        #tokens <- coreNLP::getToken(annotation)
        #Deps <- coreNLP::getDependency(annotation, type = "basic")
        #Coref <- coreNLP::getCoreference(annotation)
        #neg_deps <- Deps[Deps$type == "neg",]$governor
        #advs <- tokens[tokens$POS == "JJ" | tokens$POS == "JJS" | tokens$POS == "JJR",c("lemma","token")]
        advs <- annotation[annotation$pos == "ADJ",c("lemma","token")]
        if(allwords){
          xx <- gsub(",|\\.|;|:|\\'\\\\\"",'', xx)
          xx <- unlist(strsplit(xx, split = ' '))
        } else{
          xx <- tolower(advs$token)
          xx_lem <- advs$lemma
        }
        if(stemTrait){
          matched <- pmatch(trait_words,xx)
          traitwords_out <- xx[matched[which(!is.na(matched))]]
        } else{
          matched <- match(xx,trait_words)
          traitwords_out <- xx[which(!is.na(matched))]
          traitwords_out_lem <- xx_lem[which(!is.na(matched))]
        }
        
        ## Replacing lemmatised word with token word to allow for matching of negative dependencies
        #traitwords_out[which(traitwords_out == tokens$lemma[which(traitwords_out == tokens$lemma)])] <- tolower(tokens$token[which(tokens$lemma == traitwords_out)])
        traitwords_out[which(traitwords_out %in% neg_deps)] <- tolower(paste0("#-",traitwords_out[which(traitwords_out %in% neg_deps)]))
        rm(annotation)
        #rm(tokens)
        #rm(Deps)
        rm(neg_deps)
        rm(advs)
        gc()
        return(traitwords_out)
      }, warning = function(warning_condition) {}, error = function(error_condition) {}, finally={})
    })
    spacyr::spacy_finalize()
    
    unique_trait <- lapply(outer_lapply, function(tt){
      paste(sort(unique(unlist(tt))), collapse = ", ")
    }) 
    charDS <- data.frame(x = c(1:length(unique_trait)),
                         y = unlist(unique_trait),
                         stringsAsFactors = FALSE)
    
    #garbage collection  
    rm(words300B)
    rm(sentences)
    rm(outer_lapply)
    rm(unique_trait)
    gc()
    
    tic <- tic_generate(charDS)
    
    rm(charDS)
    gc()
    
    ## Extracting nodes, links, roots from network model computed in tic_generate.
    nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tags = unlist(tic[[1]][,2]),
                        dpub = unlist(tic[[1]][,3]), stringsAsFactors = F)
    links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                        tag = unlist(tic[[2]][,3]), stringsAsFactors = F)
    roots <- data.frame(root_node_id = unlist(tic[[3]][,1]),
                        tag = unlist(tic[[3]][,2]), stringsAsFactors = F)
    
    # TIC Statistics ----------------------------------------------------------
    ## Can we replace this with a lapply call for a list, saves two lines
    write.table(nodes,file=paste0("resources/output/", sliceSize,"/",
                                  theSource,"_nodes.csv"), row.names = F)
    write.table(links,file=paste0("resources/output/", sliceSize,"/",
                                  theSource,"_links.csv"), row.names = F)
    write.table(roots,file=paste0("resources/output/", sliceSize,"/",
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
    g <- igraph::graph.data.frame(uniqueLinks,directed = TRUE)
    
    nLabels <- c()
    for(z in igraph::V(g)$name){
      nLabels <- c(nLabels,paste(unique(unlist(strsplit(paste(uniqueLinks[which(uniqueLinks$id1==z | uniqueLinks$id2==z),3],collapse = ', '),', '))),collapse = ', '))
    }
    print(paste0(theSource,"2"))
    
    deg <-  igraph::degree(g, mode="all")
    
    degd <- degree.distribution(g)
    wtc <- igraph::cluster_walktrap(g)
    gstat <- c(igraph::diameter(g), min(degd), max(degd), mean(degd), igraph::edge_density(g), igraph::modularity(wtc))
    write.table(c(theSource,gstat), paste0("resources/output/", sliceSize, "/", theSource,"_netstat_combined.csv"), append = T, col.names = F, row.names = F, sep = ";")
    write.table(gstat, paste0("resources/output/", sliceSize,"/", theSource,"_netstat.csv"),  col.names = F, row.names = F, sep = ";")
    
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
    
    write.table(unique(socEdges), file = paste0("resources/output/",
                                                sliceSize,"/", theSource,
                                                "_socnet_edgelist.csv"), sep = ';',
                row.names = F, col.names = F)
    socEdges <- plyr::count(socEdges)
    colnames(socEdges) <- c("id1", "id2", "label")
    h <- igraph::graph.data.frame(socEdges, directed = F)
    print(paste0(theSource,"3.5"))
    igraph::E(h)$weight <- igraph::E(h)$label
    netm <- igraph::as_adjacency_matrix(h, attr = "weight", sparse = F)
    print(paste0(theSource,"3.7"))
    colnames(netm) <- igraph::V(h)$name
    rownames(netm) <- igraph::V(h)$name
    write.table(netm, paste0("resources/output/",
                             sliceSize,"/",theSource,"_network_matrix.csv"), 
                col.names = NA, row.names = T,
                fileEncoding = "UTF-8",
                sep = " ")
    print(paste0(theSource,"4"))
    
    #garbage collection  
    rm(socEdges)
    rm(links)
    rm(nodes)
    rm(roots)
    rm(convLinks)
    rm(g)
    rm(h)
    rm(netm)
    rm(SocN1)
    rm(tic)
    rm(uniqueLinks)
    rm(wtc)
    gc()
  }  
}

#Rprof()
#summaryRprof(profile_out)




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
library(smacof)

setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/value-dict-1000words-advs-lemma-book-centric/")

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

thresholds <- list(0,0.05,0.1,0.15,0.25,-0.8,-0.6,-0.4,-0.2)

for(thresh in thresholds){
  if (!file.exists(as.character(thresh))){
    dir.create(as.character(thresh))
  }
  
  ## Fetching the data of the co-ocurrence matrices
  cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))
  link <- lapply(alllinks, function(x) read.table(x, header = T, check.names = F,
                                                  stringsAsFactors = F))
  node <- lapply(allnodes, function(x) read.table(x, header = T, check.names = F,
                                                  stringsAsFactors = F))
  
  for(bb in 1:length(cooc)){
    rownames(cooc[[bb]]) <- cooc[[bb]][,1]
    cooc[[bb]][,1] <- NULL
    if(thresh > 0){
      # rare co-occurring terms
      rareT <- which(rowSums(cooc[[bb]]) <= ceiling(max(rowSums(cooc[[bb]])) * thresh))
      # frequent co-occ terms
      freqT <- which(rowSums(cooc[[bb]]) > ceiling(max(rowSums(cooc[[bb]])) * thresh))
      # rare terms only co-occurring with rare terms
      #finalRare <- which(colSums(cooc[[bb]][freqT,]) == 0)
      if(length(rareT)>0){
        cooc[[bb]] <- cooc[[bb]][-rareT,-rareT]
      }
    } else if(thresh < 0){
      # rare co-occurring terms
      rareT <- which(rowSums(cooc[[bb]]) <= ceiling(max(rowSums(cooc[[bb]])) * (-1 * thresh)))
      # frequent co-occ terms
      freqT <- which(rowSums(cooc[[bb]]) > ceiling(max(rowSums(cooc[[bb]])) * (-1 * thresh)))
      # rare terms only co-occurring with rare terms
      #finalRare <- which(colSums(cooc[[bb]][freqT,]) == 0)
      if(length(rareT)>0){
        cooc[[bb]] <- cooc[[bb]][rareT,rareT]
      }
    }
  }
  
  
  
  index <- 0
  for (sent in cooc) {
    index <- index + 1
    ## cluster text
    #sent[lower.tri(sent)] <- 0
    #Mydissimilarity_10values_r <- smacof::sim2diss(cor(cooc[[1]]), method = "rank", to.dist = T)
    write.table(sent, paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_network_matrix.csv"),sep=" ")
    test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                         pca=TRUE, perplexity = max(1,floor(nrow(sent)/3)-1), 
                         theta=0, dims=2)
    
    ## factor andlysis
    #if(thresh==0){
    tryCatch({
      loadings2 <- NULL
      loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
      write.table(loadings2$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-2factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
      write.table(loadings2$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-2factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings2)})
    tryCatch({
      loadings3 <- NULL
      loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
      write.table(loadings3$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-3factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
      write.table(loadings3$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-3factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings3)})
    tryCatch({
      loadings4 <- NULL
      loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
      write.table(loadings4$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-4factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
      write.table(loadings4$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-4factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings4)})
    tryCatch({
      loadings5 <- NULL
      loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
      write.table(loadings5$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-5factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
      write.table(loadings5$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-5factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings5)})
    
    tryCatch({
      loadings8 <- NULL
      loadings8 <- psych::fa(cor(sent),nfactors = 8,fm="minres",rotate = "varimax")
      write.table(loadings8$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-8factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings8 <- psych::fa(cor(sent),nfactors = 8,fm="minres",rotate = "varimax")
      write.table(loadings8$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-8factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings8)})
    tryCatch({
      loadings9 <- NULL
      loadings9 <- psych::fa(cor(sent),nfactors = 9,fm="minres",rotate = "varimax")
      write.table(loadings9$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-9factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings9 <- psych::fa(cor(sent),nfactors = 9,fm="minres",rotate = "varimax")
      write.table(loadings9$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-9factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings9)})
    tryCatch({
      loadings10 <- NULL
      loadings10 <- psych::fa(cor(sent),nfactors = 10,fm="minres",rotate = "varimax")
      write.table(loadings10$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-10factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings10 <- psych::fa(cor(sent),nfactors = 10,fm="minres",rotate = "varimax")
      write.table(loadings10$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-10factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings10)})  
    tryCatch({
      loadings11 <- NULL
      loadings11 <- psych::fa(cor(sent),nfactors = 11,fm="minres",rotate = "varimax")
      write.table(loadings11$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-11factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings11 <- psych::fa(cor(sent),nfactors = 11,fm="minres",rotate = "varimax")
      write.table(loadings11$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-11factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings11)})  
    tryCatch({
      loadings12 <- NULL
      loadings12 <- psych::fa(cor(sent),nfactors = 12,fm="minres",rotate = "varimax")
      write.table(loadings12$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-12factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings12 <- psych::fa(cor(sent),nfactors = 12,fm="minres",rotate = "varimax")
      write.table(loadings12$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-12factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings12)})
    
    
    #loadings <- psych::fa.parallel(cor(cooc[[2]]),nfactors = 10, fm = "minres")
    
    
    
    #withkmeans
    fit_cluster_kmeans <- fpc::kmeansruns(test$Y,krange=2:(nrow(sent)/2),critout=F,runs=5,criterion="ch")
    write.csv2(data.frame(terms=rownames(sent),clusters=fit_cluster_kmeans$cluster),paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-TSNE-cluster.csv"), row.names = F, col.names = F, sep = ";")
    #with dbscan
    #ds <- dbscan::dbscan(scale(test$Y), 20)
    #cc <- randomcoloR::distinctColorPalette(max(ds$cluster))
    #sub_title <- "dbscan"
    #plot(test$Y[,1], test$Y[,2], col=cc[ds$cluster], cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0("DBSCAN"))
    #text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=rownames(sent))
    
    # trait categories from pda500 coded file
    curTerms <- as.data.frame(cbind(rownames(sent),fit_cluster_kmeans$cluster),stringsAsFactors = F)
    
    wordTrait <- read.table("../../resources/pda500-trait-matches.csv",sep=",",header = T)
    wordTrait$Term<-tolower(wordTrait$Term)
    
    #distribution of traits when max value is used without threshold
    #plyr::count(wordTrait$maxValTrait)
    
    matched <- wordTrait[which(wordTrait$Term %in% curTerms$V1),c(1,3)]
    
    curTerms$trait <- ""
    curTerms[which(curTerms$V1 %in% matched$Term),3] <- as.character(matched[which(matched$Term %in% curTerms$V1),2])
    
    colnames(curTerms)<-c("term","cluster","trait")
    
    curTerms$trait[which(curTerms$trait=="")] <- "N.N."
    
    
    
    ### cur term with loading from 1710 mapping
    
    curTerms2 <- as.data.frame(cbind(rownames(sent),fit_cluster_kmeans$cluster),stringsAsFactors = F)
    
    wordTrait2 <- read.table("../../resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
    wordTrait2$Word<-tolower(wordTrait2$Word)
    
    #add the max trait variable without loading threshold
    wordTrait2$maxValTrait <- ""
    wordTrait2$thresholdValTrait <- ""
    for(i in 1:nrow(wordTrait2)){
      wordTrait2$maxValTrait[i] <- names(which.max(abs(wordTrait2[i,5:9])))
      wordTrait2$thresholdValTrait[i] <- ifelse(length(which(abs(wordTrait2[i,5:9])>.25))>0,names(which.max(abs(wordTrait2[i,5:9]))),"")
    }
    
    #distribution of traits when max value is used without threshold
    #plyr::count(wordTrait2$maxValTrait)
    
    matched <- wordTrait2[which(wordTrait2$Word %in% curTerms2$V1),]
    
    curTerms2$trait <- ""
    curTerms2[which(curTerms2$V1 %in% matched$Word),3] <- as.character(matched[which(matched$Word %in% curTerms2$V1),11])
    
    colnames(curTerms2)<-c("term","cluster","trait")
    
    curTerms2$trait[which(curTerms2$trait=="")] <- "N.N."
    
    
    # value categories 
    curTerms3 <- as.data.frame(cbind(rownames(sent),fit_cluster_kmeans$cluster),stringsAsFactors = F)
    wordVal <- read.table("../../resources/values.txt",sep=";")
    wordVal$V1 <- as.character(wordVal$V1)
    wordValCount<-plyr::count(wordVal$V1)
    if(length(which(wordValCount$freq>1))>0){
      redTerms <- as.character(wordValCount[which(wordValCount$freq>1),1])
      wordVal <- wordVal[-which(wordVal$V1 %in% as.character(wordValCount[which(wordValCount$freq>1),1])),]
      if(length(which(curTerms3$V1 %in% redTerms)) > 0){
        curTerms3 <- curTerms3[-which(curTerms3$V1 %in% redTerms),]
      }
    }
    
    
    matched <- wordVal[which(wordVal$V1 %in% curTerms3$V1),]
    curTerms3$trait <- ""
    curTerms3[which(curTerms3$V1 %in% matched$V1),3] <- as.character(matched[which(matched$V1 %in% curTerms3$V1),2])
    colnames(curTerms2)<-c("term","cluster","value")
    
    
    #plot kmeans
    cc <- randomcoloR::distinctColorPalette(max(fit_cluster_kmeans$cluster))
    png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_terms_2d.png"))
    plot(test$Y[,1], test$Y[,2], col=cc[fit_cluster_kmeans$cluster], cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0("DBSCAN"))
    points(fit_cluster_kmeans$centers, col = cc, pch = 8, cex = 2)
    #text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=rownames(sent))
    text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=paste0(rownames(sent), " - ",curTerms3[which(curTerms3$term==rownames(sent)),3]))
    text(x=fit_cluster_kmeans$centers[,1], y=fit_cluster_kmeans$centers[,2], cex=0.8, pos=2, labels=c(1:fit_cluster_kmeans$bestk))
    dev.off()
    
    
    #barplot
    plo <- ggplot(plyr::count(curTerms,vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
      geom_bar(stat="identity", colour="white") +
      geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) +
      ggtitle(gsub("\\_links\\.csv","",alllinks[[index]])) + 
      coord_flip()
    ggplot2::ggsave(paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_traits.jpg"),plot=plo,device="jpeg")
    
    
    # without NN labels
    plo <- ggplot(plyr::count(curTerms[which(curTerms$trait!="N.N."),],vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
      geom_bar(stat="identity", colour="white") +
      geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) +
      ggtitle(gsub("\\_links\\.csv","",alllinks[[index]])) + 
      coord_flip()
    ggplot2::ggsave(paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_traits_noNN.jpg"),plot=plo,device="jpeg")
    
    # without unmatched loadings and without NN
    plo <- ggplot(plyr::count(curTerms[which(curTerms$trait!="N.N." & curTerms$trait!="-"),],vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
      geom_bar(stat="identity", colour="white") +
      geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) +
      ggtitle(gsub("\\_links\\.csv","",alllinks[[index]])) + 
      coord_flip()
    ggplot2::ggsave(paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_traits_nounmatched.jpg"),plot=plo,device="jpeg")
    
    
    #traits per node
    tA<-c(0)
    tC<-c(0)
    tE<-c(0)
    tN<-c(0)
    tO<-c(0)
    tNN<-c(0)
    
    for(i in 1:nrow(node[[index]])){
      nodeTraits <- as.data.frame(unlist(str_split(node[[index]][i,2],", ")),stringsAsFactors = F)
      
      if(node[[index]][i,2] != ""){
        colnames(nodeTraits)<-c("words")
        nodeTraits$trait <- ""
        nodeTraits[which(nodeTraits$words %in% wordTrait$Term),2] <- as.character(wordTrait[which(wordTrait$Term %in% nodeTraits$words),3])
        colnames(nodeTraits)<-c("word","trait")
        nodeTraits$trait[which(nodeTraits$trait=="")] <- "N.N."
      }
      
      cnt_tbl <- plyr::count(nodeTraits$trait)
      
      tA <- c(tA,ifelse(length(cnt_tbl[which(cnt_tbl$x=="A"),2]>0), tA[length(tA)] + cnt_tbl[which(cnt_tbl$x=="A"),2], tA[length(tA)]))
      tC <- c(tC,ifelse(length(cnt_tbl[which(cnt_tbl$x=="C"),2]>0), tC[length(tC)] + cnt_tbl[which(cnt_tbl$x=="C"),2], tC[length(tC)]))
      tE <- c(tE,ifelse(length(cnt_tbl[which(cnt_tbl$x=="E"),2]>0), tE[length(tE)] + cnt_tbl[which(cnt_tbl$x=="E"),2], tE[length(tE)]))
      tN <- c(tN,ifelse(length(cnt_tbl[which(cnt_tbl$x=="N"),2]>0), tN[length(tN)] + cnt_tbl[which(cnt_tbl$x=="N"),2], tN[length(tN)]))
      tO <- c(tO,ifelse(length(cnt_tbl[which(cnt_tbl$x=="O"),2]>0), tO[length(tO)] + cnt_tbl[which(cnt_tbl$x=="O"),2], tO[length(tO)]))
      tNN <- c(tNN,ifelse(length(cnt_tbl[which(cnt_tbl$x=="N.N."),2]>0), tNN[length(tNN)] + cnt_tbl[which(cnt_tbl$x=="N.N."),2], tNN[length(tNN)]))
      
    }
    tA <- as.data.frame(tA)
    tC <- as.data.frame(tC)
    tE <- as.data.frame(tE)
    tN <- as.data.frame(tN)
    tO <- as.data.frame(tO)
    tNN <- as.data.frame(tNN)
    
    colnames(tA) <- c("vals")
    tA$rws <- rownames(tA)
    colnames(tC) <- c("vals")
    tC$rws <- rownames(tC)
    colnames(tE) <- c("vals")
    tE$rws <- rownames(tE)
    colnames(tN) <- c("vals")
    tN$rws <- rownames(tN)
    colnames(tO) <- c("vals")
    tO$rws <- rownames(tO)
    colnames(tNN) <- c("vals")
    tNN$rws <- rownames(tNN)
    
    out <- list()
    out["tA"] <- tA
    out["tC"] <- tC
    out["tE"] <- tE
    out["tN"] <- tN
    out["tO"] <- tO
    #out["tNN"] <- tNN
    
    dat <- lapply(out, function(x) cbind(x = seq_along(x), y = x))
    
    list.names <- names(dat)
    lns <- sapply(dat, nrow)
    dat <- as.data.frame(do.call("rbind", dat))
    dat$group <- rep(list.names, lns)
    
    plo <- ggplot(dat, aes(x = x, y = y, colour = group)) +
      theme_bw() +
      geom_line(linetype = "dotted") +
      ggtitle(gsub("\\_links\\.csv","",alllinks[[index]]))
    ggplot2::ggsave(paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_node_traits.jpg"),plot=plo,device="jpeg")
    
    
    
    
    #traits per node
    tA<-c(0)
    tC<-c(0)
    tE<-c(0)
    tN<-c(0)
    tO<-c(0)
    tNN<-c(0)
    
    for(i in 1:nrow(node[[index]])){
      nodeTraits <- as.data.frame(unlist(str_split(node[[index]][i,2],", ")),stringsAsFactors = F)
      
      if(node[[index]][i,2] != ""){
        colnames(nodeTraits)<-c("words")
        nodeTraits$trait <- ""
        nodeTraits[which(nodeTraits$words %in% wordTrait2$Word),2] <- as.character(wordTrait2[which(wordTrait2$Word %in% nodeTraits$words),3])
        colnames(nodeTraits)<-c("word","trait")
        nodeTraits$trait[which(nodeTraits$trait=="")] <- "N.N."
      }
      
      cnt_tbl <- plyr::count(nodeTraits$trait)
      
      tA <- c(tA,ifelse(length(cnt_tbl[which(cnt_tbl$x=="A"),2]>0), tA[length(tA)] + cnt_tbl[which(cnt_tbl$x=="A"),2], tA[length(tA)]))
      tC <- c(tC,ifelse(length(cnt_tbl[which(cnt_tbl$x=="C"),2]>0), tC[length(tC)] + cnt_tbl[which(cnt_tbl$x=="C"),2], tC[length(tC)]))
      tE <- c(tE,ifelse(length(cnt_tbl[which(cnt_tbl$x=="E"),2]>0), tE[length(tE)] + cnt_tbl[which(cnt_tbl$x=="E"),2], tE[length(tE)]))
      tN <- c(tN,ifelse(length(cnt_tbl[which(cnt_tbl$x=="N"),2]>0), tN[length(tN)] + cnt_tbl[which(cnt_tbl$x=="N"),2], tN[length(tN)]))
      tO <- c(tO,ifelse(length(cnt_tbl[which(cnt_tbl$x=="O"),2]>0), tO[length(tO)] + cnt_tbl[which(cnt_tbl$x=="O"),2], tO[length(tO)]))
      tNN <- c(tNN,ifelse(length(cnt_tbl[which(cnt_tbl$x=="N.N."),2]>0), tNN[length(tNN)] + cnt_tbl[which(cnt_tbl$x=="N.N."),2], tNN[length(tNN)]))
      
    }
    tA <- as.data.frame(tA)
    tC <- as.data.frame(tC)
    tE <- as.data.frame(tE)
    tN <- as.data.frame(tN)
    tO <- as.data.frame(tO)
    tNN <- as.data.frame(tNN)
    
    colnames(tA) <- c("vals")
    tA$rws <- rownames(tA)
    colnames(tC) <- c("vals")
    tC$rws <- rownames(tC)
    colnames(tE) <- c("vals")
    tE$rws <- rownames(tE)
    colnames(tN) <- c("vals")
    tN$rws <- rownames(tN)
    colnames(tO) <- c("vals")
    tO$rws <- rownames(tO)
    colnames(tNN) <- c("vals")
    tNN$rws <- rownames(tNN)
    
    out <- list()
    out["tA"] <- tA
    out["tC"] <- tC
    out["tE"] <- tE
    out["tN"] <- tN
    out["tO"] <- tO
    #out["tNN"] <- tNN
    
    dat <- lapply(out, function(x) cbind(x = seq_along(x), y = x))
    
    list.names <- names(dat)
    lns <- sapply(dat, nrow)
    dat <- as.data.frame(do.call("rbind", dat))
    dat$group <- rep(list.names, lns)
    
    plo <- ggplot(dat, aes(x = x, y = y, colour = group)) +
      theme_bw() +
      geom_line(linetype = "dotted") +
      ggtitle(gsub("\\_links\\.csv","",alllinks[[index]]))
    ggplot2::ggsave(paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_node_traits_1710.jpg"),plot=plo,device="jpeg")
    
    
    #traits per node
    tAC<-c(0)
    tBE<-c(0)
    tCO<-c(0)
    tHE<-c(0)
    tPO<-c(0)
    tSE<-c(0)
    tSD<-c(0)
    tST<-c(0)
    tTR<-c(0)
    tUN<-c(0)
    
    for(i in 1:nrow(node[[index]])){
      nodeTraits <- as.data.frame(unlist(str_split(node[[index]][i,2],", ")),stringsAsFactors = F)
      
      if(node[[index]][i,2] != ""){
        colnames(nodeTraits)<-c("words")
        nodeTraits$value <- ""
        nodeTraits[which(nodeTraits$words %in% wordVal$V1),2] <- as.character(wordVal[which(wordVal$V1 %in% nodeTraits$words),2])
        colnames(nodeTraits)<-c("word","value")
      }
      
      cnt_tbl <- plyr::count(nodeTraits$value)
      
      tAC <- c(tAC,ifelse(length(cnt_tbl[which(cnt_tbl$x=="AC"),2]>0), tAC[length(tAC)] + cnt_tbl[which(cnt_tbl$x=="AC"),2], tAC[length(tAC)]))
      tBE <- c(tBE,ifelse(length(cnt_tbl[which(cnt_tbl$x=="BE"),2]>0), tBE[length(tBE)] + cnt_tbl[which(cnt_tbl$x=="BE"),2], tBE[length(tBE)]))
      tCO <- c(tCO,ifelse(length(cnt_tbl[which(cnt_tbl$x=="CO"),2]>0), tCO[length(tCO)] + cnt_tbl[which(cnt_tbl$x=="CO"),2], tCO[length(tCO)]))
      tHE <- c(tHE,ifelse(length(cnt_tbl[which(cnt_tbl$x=="HE"),2]>0), tHE[length(tHE)] + cnt_tbl[which(cnt_tbl$x=="HE"),2], tHE[length(tHE)]))
      tPO <- c(tPO,ifelse(length(cnt_tbl[which(cnt_tbl$x=="PO"),2]>0), tPO[length(tPO)] + cnt_tbl[which(cnt_tbl$x=="PO"),2], tPO[length(tPO)]))
      tSE <- c(tSE,ifelse(length(cnt_tbl[which(cnt_tbl$x=="SE"),2]>0), tSE[length(tSE)] + cnt_tbl[which(cnt_tbl$x=="SE"),2], tSE[length(tSE)]))
      tSD <- c(tSD,ifelse(length(cnt_tbl[which(cnt_tbl$x=="SD"),2]>0), tSD[length(tSD)] + cnt_tbl[which(cnt_tbl$x=="SD"),2], tSD[length(tSD)]))
      tST <- c(tST,ifelse(length(cnt_tbl[which(cnt_tbl$x=="ST"),2]>0), tST[length(tST)] + cnt_tbl[which(cnt_tbl$x=="ST"),2], tST[length(tST)]))
      tTR <- c(tTR,ifelse(length(cnt_tbl[which(cnt_tbl$x=="TR"),2]>0), tTR[length(tTR)] + cnt_tbl[which(cnt_tbl$x=="TR"),2], tTR[length(tTR)]))
      tUN <- c(tUN,ifelse(length(cnt_tbl[which(cnt_tbl$x=="UN"),2]>0), tUN[length(tUN)] + cnt_tbl[which(cnt_tbl$x=="UN"),2], tUN[length(tUN)]))
    }
    tAC <- as.data.frame(tAC)
    tBE <- as.data.frame(tBE)
    tCO <- as.data.frame(tCO)
    tHE <- as.data.frame(tHE)
    tPO <- as.data.frame(tPO)
    tSE <- as.data.frame(tSE)
    tSD <- as.data.frame(tSD)
    tST <- as.data.frame(tST)
    tTR <- as.data.frame(tTR)
    tUN <- as.data.frame(tUN)
    
    colnames(tAC) <- c("vals")
    tAC$rws <- rownames(tAC)
    colnames(tBE) <- c("vals")
    tBE$rws <- rownames(tBE)
    colnames(tCO) <- c("vals")
    tCO$rws <- rownames(tCO)
    colnames(tHE) <- c("vals")
    tHE$rws <- rownames(tHE)
    colnames(tPO) <- c("vals")
    tPO$rws <- rownames(tPO)
    colnames(tSE) <- c("vals")
    tSE$rws <- rownames(tSE)
    colnames(tSD) <- c("vals")
    tSD$rws <- rownames(tSD)
    colnames(tST) <- c("vals")
    tST$rws <- rownames(tST)
    colnames(tTR) <- c("vals")
    tTR$rws <- rownames(tTR)
    colnames(tUN) <- c("vals")
    tUN$rws <- rownames(tUN)
    
    out <- list()
    out["tAC"] <- tAC
    out["tBE"] <- tBE
    out["tCO"] <- tCO
    out["tHE"] <- tHE
    out["tPO"] <- tPO
    out["tSE"] <- tSE
    out["tSD"] <- tSD
    out["tST"] <- tST
    out["tTR"] <- tTR
    out["tUN"] <- tUN
    
    dat <- lapply(out, function(x) cbind(x = seq_along(x), y = x))
    
    list.names <- names(dat)
    lns <- sapply(dat, nrow)
    dat <- as.data.frame(do.call("rbind", dat))
    dat$group <- rep(list.names, lns)
    
    plo <- ggplot(dat, aes(x = x, y = y, colour = group)) +
      theme_bw() +
      geom_line(linetype = "dotted") +
      ggtitle(gsub("\\_links\\.csv","",alllinks[[index]]))
    ggplot2::ggsave(paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_node_values.jpg"),plot=plo,device="jpeg")
    
    ###MDS on values
    if(thresh < 0.1){
      sent[lower.tri(sent)] <- 0
      
      wordVal <- read.table("../../resources/values.txt",sep=";")
      wordVal$V1 <- as.character(wordVal$V1)
      wordVal$V2 <- as.character(wordVal$V2)
      wordValCount<-plyr::count(wordVal$V1)
      if(length(which(wordValCount$freq>1))>0){
        redTerms <- as.character(wordValCount[which(wordValCount$freq>1),1])
        wordVal <- wordVal[-which(wordVal$V1 %in% as.character(wordValCount[which(wordValCount$freq>1),1])),]
      }
      
      traitCo <- matrix(nrow=10,ncol=10,data = 0)
      rownames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      colnames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      
      for(i in 1:nrow(sent)){
        for(j in 1:nrow(sent)){
          rowTerm <- as.character(wordVal$V2[which(wordVal$V1==rownames(sent)[i])][1])
          colTerm <- as.character(wordVal$V2[which(wordVal$V1==colnames(sent)[j])][1])
          crossVal <- sent[i,j]
          if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
            traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
            traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
          }
        }
      }
      
      Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_rank_shepard.png"))
      plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      dev.off()
      MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      MDSmod_10values_r$stress
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_rank.png"))
      plot(MDSmod_10values_r, main = "1710 // Rank", xlab = "Dimension 1", ylab = "Dimension 2")
      dev.off()
      
      Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_corr_shepard.png"))
      plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      dev.off()
      MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      MDSmod_10values_c$stress
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_corr.png"))
      plot(MDSmod_10values_c, main = "1710 // Correlation", xlab = "Dimension 1", ylab = "Dimension 2")
      dev.off()
      
      
      
      
      ###MDS on traits randomized
      traitCo <- matrix(nrow=10,ncol=10,data = 0)
      rownames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      colnames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      baseTraits<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      traitCounts <- plyr::count(wordVal$V2)
      #traitCounts <- traitCounts[-which(traitCounts$x=="-"),]
      traitProbs <- traitCounts$freq/sum(traitCounts$freq)
      
      for(i in 1:nrow(sent)){
        for(j in 1:nrow(sent)){
          rowTerm <- sample(baseTraits, 1)
          colTerm <- sample(baseTraits, 1)
          crossVal <- sent[i,j]
          if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
            traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
            traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
          }
        }
      }
      
      Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_rank_shepard_randomized.png"))
      plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      dev.off()
      MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      MDSmod_10values_r$stress
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_rank_randomized.png"))
      plot(MDSmod_10values_r, main = "1710 // Rank", xlab = "Dimension 1", ylab = "Dimension 2")
      dev.off()
      
      Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_corr_shepard_randomized.png"))
      plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      dev.off()
      MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      MDSmod_10values_c$stress
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_corr_randomized.png"))
      plot(MDSmod_10values_c, main = "1710 // Correlation", xlab = "Dimension 1", ylab = "Dimension 2")
      dev.off()
      
      
      for(i in 1:nrow(sent)){
        for(j in 1:nrow(sent)){
          rowTerm <- sample(baseTraits, 1,prob = traitProbs)
          colTerm <- sample(baseTraits, 1,prob = traitProbs)
          crossVal <- sent[i,j]
          if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
            traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
            traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
          }
        }
      }
      
      Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_rank_shepard_randomized_probability.png"))
      plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      dev.off()
      MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      MDSmod_10values_r$stress
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_rank_randomized_probability.png"))
      plot(MDSmod_10values_r, main = "1710 // Rank", xlab = "Dimension 1", ylab = "Dimension 2")
      dev.off()
      
      Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_corr_shepard_randomized_probability.png"))
      plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      dev.off()
      MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      MDSmod_10values_c$stress
      png(filename=paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_MDS_values_corr_randomized_probability.png"))
      plot(MDSmod_10values_c, main = "1710 // Correlation", xlab = "Dimension 1", ylab = "Dimension 2")
      dev.off()
    }
  }
}



