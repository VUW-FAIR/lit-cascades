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



index <- 0
for (sent in cooc) {
  index <- index + 1
  ## cluster text
  test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                       pca=TRUE, perplexity = max(1,floor(nrow(sent)/3)-1), 
                       theta=0, dims=2)
  #withkmeans
  fit_cluster_kmeans <- fpc::kmeansruns(test$Y,krange=2:(nrow(sent)/2),critout=F,runs=5,criterion="ch")
  write.csv2(data.frame(terms=rownames(sent),clusters=fit_cluster_kmeans$cluster),paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"-TSNE-cluster.csv"), row.names = F, col.names = F, sep = ";")
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
  
  
  #plot kmeans
  cc <- randomcoloR::distinctColorPalette(max(fit_cluster_kmeans$cluster))
  png(filename=paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_terms_2d.png"))
  plot(test$Y[,1], test$Y[,2], col=cc[fit_cluster_kmeans$cluster], cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0("DBSCAN"))
  points(fit_cluster_kmeans$centers, col = cc, pch = 8, cex = 2)
  #text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=rownames(sent))
  text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=paste0(rownames(sent), " - ",curTerms[which(curTerms$term==rownames(sent)),3]))
  text(x=fit_cluster_kmeans$centers[,1], y=fit_cluster_kmeans$centers[,2], cex=0.8, pos=2, labels=c(1:fit_cluster_kmeans$bestk))
  dev.off()
  
  
  #barplot
  plo <- ggplot(plyr::count(curTerms,vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
    geom_bar(stat="identity", colour="white") +
    geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) +
    ggtitle(gsub("\\_links\\.csv","",alllinks[[index]])) + 
    coord_flip()
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_traits.jpg"),plot=plo,device="jpeg")
  
  
  # without NN labels
  plo <- ggplot(plyr::count(curTerms[which(curTerms$trait!="N.N."),],vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
    geom_bar(stat="identity", colour="white") +
    geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) +
    ggtitle(gsub("\\_links\\.csv","",alllinks[[index]])) + 
    coord_flip()
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_traits_noNN.jpg"),plot=plo,device="jpeg")
  
  # without unmatched loadings and without NN
  plo <- ggplot(plyr::count(curTerms[which(curTerms$trait!="N.N." & curTerms$trait!="-"),],vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
    geom_bar(stat="identity", colour="white") +
    geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) +
    ggtitle(gsub("\\_links\\.csv","",alllinks[[index]])) + 
    coord_flip()
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_traits_nounmatched.jpg"),plot=plo,device="jpeg")
  
  
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
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_node_traits.jpg"),plot=plo,device="jpeg")
  
  
  
  
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
      nodeTraits[which(nodeTraits$words %in% wordTrait2$Term),2] <- as.character(wordTrait2[which(wordTrait2$Term %in% nodeTraits$words),3])
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
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_node_traits_1710.jpg"),plot=plo,device="jpeg")
  
  
}

### FOR 1710


setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/pda1710-1000words-advs-lemma-book-centric/")

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






index <- 0
for (sent in cooc) {
  index <- index + 1
  ## cluster text
  test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                       pca=TRUE, perplexity = max(1,floor(nrow(sent)/3)-1), 
                       theta=0, dims=2)
  #withkmeans
  fit_cluster_kmeans <- fpc::kmeansruns(test$Y,krange=2:(nrow(sent)/2),critout=F,runs=5,criterion="ch")
  
  write.csv2(data.frame(terms=rownames(sent),clusters=fit_cluster_kmeans$cluster),paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"-TSNE-cluster.csv"), row.names = F, col.names = F, sep = ";")
  #with dbscan
  #ds <- dbscan::dbscan(scale(test$Y), 20)
  #cc <- randomcoloR::distinctColorPalette(max(ds$cluster))
  #sub_title <- "dbscan"
  #plot(test$Y[,1], test$Y[,2], col=cc[ds$cluster], cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0("DBSCAN"))
  #text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=rownames(sent))
  
  # trait categories from 1710 coded file
  curTerms <- as.data.frame(cbind(rownames(sent),fit_cluster_kmeans$cluster),stringsAsFactors = F)
  
  wordTrait <- read.table("../../resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
  wordTrait$Word<-tolower(wordTrait$Word)
  
  #add the max trait variable without loading threshold
  wordTrait$maxValTrait <- ""
  wordTrait$thresholdValTrait <- ""
  for(i in 1:nrow(wordTrait)){
    wordTrait$maxValTrait[i] <- names(which.max(abs(wordTrait[i,5:9])))
    wordTrait$thresholdValTrait[i] <- ifelse(length(which(abs(wordTrait[i,5:9])>.25))>0,names(which.max(abs(wordTrait[i,5:9]))),"")
  }
  
  #distribution of traits when max value is used without threshold
  #plyr::count(wordTrait$maxValTrait)
  
  matched <- wordTrait[which(wordTrait$Word %in% curTerms$V1),]
  
  #for above threshold .3 loading as coded
  #curTerms$trait <- ""
  #curTerms[which(curTerms$V1 %in% matched$Word),3] <- as.character(matched[which(matched$Word %in% curTerms$V1),2])
  
  #for max value loading without threshold
  #curTerms$trait <- ""
  #curTerms[which(curTerms$V1 %in% matched$Word),3] <- as.character(matched[which(matched$Word %in% curTerms$V1),10])
  
  #for max value loading with threshold
  curTerms$trait <- ""
  curTerms[which(curTerms$V1 %in% matched$Word),3] <- as.character(matched[which(matched$Word %in% curTerms$V1),11])
  
  colnames(curTerms)<-c("term","cluster","trait")
  
  curTerms$trait[which(curTerms$trait=="")] <- "N.N."
  
  ### for pda500 loading file
  curTerms2 <- as.data.frame(cbind(rownames(sent),fit_cluster_kmeans$cluster),stringsAsFactors = F)
  
  wordTrait2 <- read.table("../../resources/pda500-trait-matches.csv",sep=",",header = T)
  wordTrait2$Term<-tolower(wordTrait2$Term)
  
  #distribution of traits when max value is used without threshold
  #plyr::count(wordTrait2$maxValTrait)
  
  matched <- wordTrait2[which(wordTrait2$Term %in% curTerms2$V1),c(1,3)]
  
  curTerms2$trait <- ""
  curTerms2[which(curTerms2$V1 %in% matched$Term),3] <- as.character(matched[which(matched$Term %in% curTerms2$V1),2])
  
  colnames(curTerms2)<-c("term","cluster","trait")
  
  curTerms2$trait[which(curTerms2$trait=="")] <- "N.N."
  
  
  #plot kmeans
  cc <- randomcoloR::distinctColorPalette(max(fit_cluster_kmeans$cluster))
  png(filename=paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_terms_2d.png"))
  plot(test$Y[,1], test$Y[,2], col=cc[fit_cluster_kmeans$cluster], cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0("DBSCAN"))
  points(fit_cluster_kmeans$centers, col = cc, pch = 8, cex = 2)
  #text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=rownames(sent))
  text(x=test$Y[,1], y=test$Y[,2], cex=0.8, pos=4, labels=paste0(rownames(sent), " - ",curTerms[which(curTerms$term==rownames(sent)),3]))
  text(x=fit_cluster_kmeans$centers[,1], y=fit_cluster_kmeans$centers[,2], cex=0.8, pos=2, labels=c(1:fit_cluster_kmeans$bestk))
  dev.off()
  
  
  #barplot
  plo <- ggplot(plyr::count(curTerms,vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
    geom_bar(stat="identity", colour="white") +
    geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) +
    ggtitle(gsub("\\_links\\.csv","",alllinks[[index]])) + 
    coord_flip()
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_traits.jpg"),plot=plo,device="jpeg")
  
  
  # without NN labels
  plo <- ggplot(plyr::count(curTerms[which(curTerms$trait!="N.N."),],vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
    geom_bar(stat="identity", colour="white") +
    geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) +
    ggtitle(gsub("\\_links\\.csv","",alllinks[[index]])) + 
    coord_flip()
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_cluster_traits_noNN.jpg"),plot=plo,device="jpeg")
  
  
  #traits per node
  tA<-c(0)
  tC<-c(0)
  tE<-c(0)
  tN<-c(0)
  tO<-c(0)
  tNN<-c(0)
  
  nod <- node[[index]]
  nod$A <- 0
  nod$C <- 0
  nod$E <- 0
  nod$N <- 0
  nod$O <- 0
  nod$NN <- 0
  
  for(i in 1:nrow(node[[index]])){
    nodeTraits <- as.data.frame(unlist(str_split(node[[index]][i,2],", ")),stringsAsFactors = F)
    colnames(nodeTraits)<-c("words")
    nodeTraits$trait <- ""
    nodeTraits[which(nodeTraits$words %in% wordTrait$Word),2] <- as.character(wordTrait[which(wordTrait$Word %in% nodeTraits$words),11])
    colnames(nodeTraits)<-c("word","trait")
    nodeTraits$trait[which(nodeTraits$trait=="")] <- "N.N."
    
    cnt_tbl <- plyr::count(nodeTraits$trait)
    
    tA <- c(tA,ifelse(length(cnt_tbl[which(cnt_tbl$x=="A"),2]>0), tA[length(tA)] + cnt_tbl[which(cnt_tbl$x=="A"),2], tA[length(tA)]))
    nod$A[i] <- tA[i+1]
    tC <- c(tC,ifelse(length(cnt_tbl[which(cnt_tbl$x=="C"),2]>0), tC[length(tC)] + cnt_tbl[which(cnt_tbl$x=="C"),2], tC[length(tC)]))
    nod$C[i] <- tC[i+1]
    tE <- c(tE,ifelse(length(cnt_tbl[which(cnt_tbl$x=="E"),2]>0), tE[length(tE)] + cnt_tbl[which(cnt_tbl$x=="E"),2], tE[length(tE)]))
    nod$E[i] <- tE[i+1]
    tN <- c(tN,ifelse(length(cnt_tbl[which(cnt_tbl$x=="N"),2]>0), tN[length(tN)] + cnt_tbl[which(cnt_tbl$x=="N"),2], tN[length(tN)]))
    nod$N[i] <- tN[i+1]
    tO <- c(tO,ifelse(length(cnt_tbl[which(cnt_tbl$x=="O"),2]>0), tO[length(tO)] + cnt_tbl[which(cnt_tbl$x=="O"),2], tO[length(tO)]))
    nod$O[i] <- tO[i+1]
    tNN <- c(tNN,ifelse(length(cnt_tbl[which(cnt_tbl$x=="N.N."),2]>0), tNN[length(tNN)] + cnt_tbl[which(cnt_tbl$x=="N.N."),2], tNN[length(tNN)]))
    nod$NN[i] <- tNN[i+1]
    
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
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_node_traits.jpg"),plot=plo,device="jpeg")
  
  
  
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
      nodeTraits[which(nodeTraits$words %in% wordTrait2$Term),2] <- as.character(wordTrait2[which(wordTrait2$Term %in% nodeTraits$words),3])
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
  ggplot2::ggsave(paste0(gsub("\\_links\\.csv","",alllinks[[index]]),"_node_traits_500.jpg"),plot=plo,device="jpeg")
  
  ## time series analysis of traits
  #ts1 <- zoo::zoo(nod$A)
  #ts1_rate <- log(ts1 / stats::lag(ts1))
  #ts1_rate <- diff(ts1)
  #ts2 <- zoo::zoo(nod$C)
  #ts2_rate <- log(ts2 / stats::lag(ts2))
  #ts2_rate <- diff(ts2)
  #lmtest::grangertest(ts1_rate ~ ts2_rate, order = 1)
}





library(igraph)
library(threejs)
g <- igraph::graph.edgelist(as.matrix(data.frame(from=as.character(link[[1]][,1]),to=as.character(link[[1]][,2]))), directed=F)
g <- g - E(g)
graph_list <- list()

labels <- c()

for(j in 1:nrow(link[[1]])){
  foo <- g
  labels<-c(labels,link[[1]][j,1],link[[1]][j,2])
  for(k in (j-1):0){
    foo <- foo + edge(as.character(link[[1]][j-k,1]),as.character(link[[1]][j-k,2]))
  }
  graph_list[[j]] <- foo
}
labels <- unique(labels)
V(g)$label <- paste0(labels, " - ",node[[1]]$tags[labels])
threejs::graphjs(graph_list, vertex.label=paste0(labels, " - ",node[[1]]$tags[labels]),
                 fpl=50, repulsion = 0.3, curvature = 0.4, bg="black", fg="white")



### recurrence
lnk <- link[[23]]
nod <- node[[23]]
wordTrait <- read.table("../../resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
wordTrait$Word<-tolower(wordTrait$Word)

#add the max trait variable without loading threshold
wordTrait$maxValTrait <- ""
wordTrait$thresholdValTrait <- ""
for(i in 1:nrow(wordTrait)){
  wordTrait$maxValTrait[i] <- names(which.max(abs(wordTrait[i,5:9])))
  wordTrait$thresholdValTrait[i] <- ifelse(length(which(abs(wordTrait[i,5:9])>.25))>0,names(which.max(abs(wordTrait[i,5:9]))),"")
}

