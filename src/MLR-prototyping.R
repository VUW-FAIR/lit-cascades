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
  #plot(test$Y[,1], test$Y[,2], col=cc[fit_cluster_kmeans$cluster], cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0("DBSCAN"))
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
  
  #cluster trait profile
  #print(plyr::count(curTerms,vars=cluster~trait))
  
  #barplot
  plo <- ggplot(plyr::count(curTerms,vars=cluster~trait), aes(x=cluster, y=freq, fill=trait)) +
    geom_bar(stat="identity", colour="white") +
    geom_text(aes(label=paste0(trait,"-",freq)),position=position_stack(vjust=0.5), colour="white",size = 3) + coord_flip()
  ggplot2::ggsave(paste0(index,"_cluster_traits.jpg"),plot=plo,device="jpeg")
  
  
  
  
  #traits per node
  tA<-c(0)
  tC<-c(0)
  tE<-c(0)
  tN<-c(0)
  tO<-c(0)
  tNN<-c(0)
  
  for(i in 1:nrow(node[[index]])){
    nodeTraits <- as.data.frame(unlist(str_split(node[[1]][i,2],", ")),stringsAsFactors = F)
    colnames(nodeTraits)<-c("words")
    nodeTraits$trait <- ""
    nodeTraits[which(nodeTraits$words %in% wordTrait$Word),2] <- as.character(wordTrait[which(wordTrait$Word %in% nodeTraits$words),11])
    colnames(nodeTraits)<-c("word","trait")
    nodeTraits$trait[which(nodeTraits$trait=="")] <- "N.N."
    
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
    geom_line(linetype = "dotted")
  ggplot2::ggsave(paste0(index,"_node_traits.jpg"),plot=plo,device="jpeg")
  
  
  
}




