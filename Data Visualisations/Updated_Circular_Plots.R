
###Updated Circular Graphs


#set directory
DIRECTORY<-"pda1710-sentence-advs-lemma-book-centric" 


#prep the matrices
setwd("/home/STAFF/cabautgr/projects/tic-personality-words/")

getwd()
setwd("..")
## Creating a list with all co-ocurrence matrices for all outputs
## Creating a list with all co-ocurrence matrices for all outputs
allmatrices <- list.files(paste0("outputs save/",DIRECTORY),
                          pattern = "(.*)_network_matrix.csv",
                          full.names = T)

cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))

for(bb in 1:length(cooc)){
  rownames(cooc[[bb]]) <- cooc[[bb]][,1]
  cooc[[bb]][,1] <- NULL
  
  # rare co-occurring terms
  rareT <- which(rowSums(cooc[[bb]]) <= ceiling(max(rowSums(cooc[[bb]])) * .15))
  # frequent co-occ terms
  freqT <- which(rowSums(cooc[[bb]]) > ceiling(max(rowSums(cooc[[bb]])) * .15))
  # rare terms only co-occurring with rare terms
  finalRare <- which(colSums(cooc[[bb]][freqT,]) == 0)
  
  cooc[[bb]] <- cooc[[bb]][-finalRare,-finalRare]
  
  if(length(which(rowSums(cooc[[bb]])==0)) > 0){
    cooc[[bb]] <- cooc[[bb]][-(which(rowSums(cooc[[bb]])==0)),-(which(rowSums(cooc[[bb]])==0))]
  }
  
}


#duplicate of circ plot examples.r
#creating circular graphs:

library(circlize)

#Setting directory
setwd("outputs save")
setwd(DIRECTORY)
getwd()
setwd("..") #if you would like to go back one directory

#all the -TSNE-cluster.csv files in the directory
clusterscsv <- list(dir(pattern = "*-TSNE-cluster.csv")) 

##do a loop here
#for(k in 3:length(cooc)){
k<-3
mat<-as.matrix(cooc[[k]])
mat[lower.tri(mat)] <- 0
par(mar = c(0, 0, 0, 0))


cfile<-clusterscsv[[1]][k] 

#get the title and position k
clusters<-read.csv(file=cfile,header=T,sep=";")

library(RColorBrewer)
n <- nrow(unique(clusters))

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]

col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colcodes <- sample(col_vector, n, replace = T)

colframe <- data.frame(terms=rownames(mat),clusters=clusters)
colframe$colcode <- colcodes[colframe$clusters]

grid.col <- setNames(colframe$colcode, colframe$terms)

Title<-gsub("\\_", " ",sub(pattern = "-TSNE-cluster(.*)\\..*$", replacement = "\\1", basename(cfile)))

#title(main=Title,col.main="gray",adj=0.15,line=-2)
makeCircos<-function(){
  circos.par(gap.degree = 0)
  chordDiagram(mat, directional = FALSE, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col)
  #plot.new()
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    #circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
  }, bg.border = NA)
}

makeCircos()

#save the graph in the PostProc folder of the directory
BookTitle<-sub(pattern = "-TSNE-cluster(.*)\\..*$", replacement = "\\1", basename(cfile))
dev.copy2pdf(file = paste0(paste0("../",DIRECTORY,"/","PostProc","/"),BookTitle,k,"_","clusterCircPlot.pdf"), height=10, width=10,out.type = "pdf",onefile=FALSE)
dev.off()
circos.clear()
#gc()
#}

# rm(mat)
# rm(cfile)
# rm(clusters)
# rm(n)
# rm(qual_col_pals)
# rm(col_vector)
# rm(colcodes)
# rm(colframe)
# rm(colframe$colcode)
# rm(grid.col)
# gc()
# rm(allmatrices)
# gc()

###-------------------------------------

##EXTRA:
#prep the matrices
begPath<-"/Users/GraceC/TIC-VUW" #<- example
setwd(paste0(begPath,"/Users/GraceC/TIC-VUW/tic-personality-words"))
getwd()
setwd("..") #if you would like to go back one directory

