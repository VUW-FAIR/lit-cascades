

###Original Circular Graphs

#PREP THE MATRICES
setwd("/home/STAFF/cabauagr/projects/tic-personality-words/")
D<-"pda500-1000words-advs-lemma-book-centric"
getwd()
setwd("..")

## Creating a list with all co-ocurrence matrices for all outputs
allmatrices <- list.files(paste0("outputs save/",D),
pattern = "(.*)_network_matrix.csv",
full.names = T)

cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))

for(bb in 1:length(cooc)){
    rownames(cooc[[bb]]) <- cooc[[bb]][,1]
    cooc[[bb]][,1] <- NULL
    
     # # rare co-occurring terms
     # rareT <- which(rowSums(cooc[[bb]]) <= ceiling(max(rowSums(cooc[[bb]])) * .15))
     # # frequent co-occ terms
     # freqT <- which(rowSums(cooc[[bb]]) > ceiling(max(rowSums(cooc[[bb]])) * .15))
     # # rare terms only co-occurring with rare terms
     # finalRare <- which(colSums(cooc[[bb]][freqT,]) == 0)
     # 
     # cooc[[bb]] <- cooc[[bb]][-finalRare,-finalRare]
     # 
     # if(length(which(rowSums(cooc[[bb]])==0)) > 0){
     #   cooc[[bb]] <- cooc[[bb]][-(which(rowSums(cooc[[bb]])==0)),-(which(rowSums(cooc[[bb]])==0))]
     # }
}     


##
#mat<-cooc[[1]]
#word frequency in matrix
count_cooc<-function(mat){
    v<-c(1:length(mat[,1])) #vector
    v1<-c(1:length(mat[,1])) #vector
    v2<-c(1:length(mat[,1])) #vector
    
    ##FREQUENCY
    #rows
    for(b in 1:length(mat[,1])){ #row
        count<-0 #count
        v1[b]<-0
        for(c in 1:length(mat[,1])){ #col
            #c<-2
            count<-count+mat[b,c]
        }
        v1[b]<-count
        #print(paste(b,count))
    }
    
    #cols
    for(b in 1:length(mat[,1])){ #row
        count<-0 #count
        v2[b]<-0
        for(c in 1:length(mat[,1])){ #col
            #c<-2
            count<-count+mat[c,b]
        }
        v2[b]<-count
        #print(paste(b,count))
    }
    
    for(x in 1:length(mat[,1])){
        v[x]<- v1[x]+v2[x]
    }
    
    return(v)
}


#CIRCULAR GRAPHS
install.packages("circlize")
install.packages("RColorBrewer")
library(circlize)
library(RColorBrewer)

#Setting directory
setwd("outputs save")
DIRECTORY<-"pda500-1000words-advs-lemma-book-centric/0.1" #** <--change 0.1 or 0.15
setwd(DIRECTORY)
getwd()
setwd("..") #if you would like to go back one directory

#all the -TSNE-cluster.csv files in the directory
clusterscsv <- list(dir(pattern = "*-TSNE-cluster.csv"))

#matrixCount<-count_cooc(mat)


##do a loop here
k<-1

for(k in 1:length(cooc)){
    
    print(paste("mat: ",k))
    
    mat<-as.matrix(cooc[[k]])
    
    mat[lower.tri(mat)] <- 0
    par(mar = c(0, 0, 0, 0))
    
    #frequency count
    matrixCount<-count_cooc(mat)
    
    cfile<-clusterscsv[[1]][k]
    
    #get the title and position k
    clusters<-read.csv(file=cfile,header=T,sep="")
    ##
    colnames(clusters)[2]<-"count"
    clustersVector<-as.data.frame(c(1:length(clusters[,1])))
    colnames(clustersVector)[1]<-"clusters"
    
    for(i in 1:length(clusters[,1])){
        n<-clusters[i,2]
        n<-sub(";","",n)
        clustersVector[i,1]<-n
    }
    
    ##
    n <- nrow(unique(clusters))
    
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    colcodes <- sample(col_vector, n, replace = T)
    
    clusters<-clustersVector
    
    colframe <- data.frame(terms=rownames(mat),clusters=clusters)
    
    colframe$clusters<-as.numeric(colframe$clusters)
    colframe$colcode <- colcodes[colframe$clusters]
    
    grid.col <- setNames(colframe$colcode, colframe$terms)
    
    Title<-gsub("\\_", " ",sub(pattern = "-TSNE-cluster(.*)\\..*$", replacement = "\\1", basename(cfile)))
    
    #circular graph
    #title(main=Title,col.main="gray",adj=0.15,line=-2)
    chordDiagram(mat, directional = FALSE, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col)
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.index = get.cell.meta.data("sector.numeric.index")
        f<-matrixCount[sector.index]
        sector.name = paste(get.cell.meta.data("sector.index"),f)
        circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    }, bg.border = NA)
    
    #save the graph in the PostProc folder of the directory
    BookTitle<-sub(pattern = "-TSNE-cluster(.*)\\..*$", replacement = "\\1", basename(cfile))
    dev.copy2pdf(file = paste0(paste0("../",DIRECTORY,"/","PostProc","/"),BookTitle,k,"_","clusterCircPlot.pdf"), height=20, width=20,out.type = "pdf",onefile=FALSE)
    dev.off()
    circos.clear()
    
}


#vector size too big: 2,3,5,6,12,14,16,19,21
###-------------------------------------

##EXTRA:
#prep the matrices
begPath<-"/Users/GraceC/TIC-VUW" #<- example
setwd(paste0(begPath,"/Users/GraceC/TIC-VUW/tic-personality-words"))
getwd()
setwd("..") #if you would like to go back one directory

cooc[[3]]
clusterscsv[3]
clusterscsv
