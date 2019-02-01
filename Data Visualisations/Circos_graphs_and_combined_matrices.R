
install.packages('RCircos')
### Circular graph for one book only

library(circlize)
library(circos)
# mat<-as.matrix(m3) #combined matA and matB
mat<-as.matrix(cooc[[1]])
mat[lower.tri(mat)] <- 0
par(mar = c(0, 0, 0, 0))

clusters <- read.csv2("resources/output/post/A_Tale_of_Two_Cities-TSNE-cluster.csv",sep=";",header = T)


### Assigning unique colours for clustering
library(RColorBrewer)
n <- nrow(unique(clusters))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colcodes <- sample(col_vector, n)

colframe <- data.frame(terms=rownames(mat),clusters=clusters)
colframe$colcode <- colcodes[colframe$clusters]

grid.col <- setNames(colframe$colcode, colframe$terms)


### ChordDiagram to create the circular graph 
chordDiagram(mat, directional = FALSE, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  #circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

### For recurrence all_features information
tmp <- read.csv2("resources/output/post/The_unfortunate_traveler-allFeatures-output.csv",sep=";") #changed book
tmp[which(is.nan(tmp[,7])),7] <- 0   

df_tmp <- tmp[which(tmp$terms %in% rownames(mat)),]

#circos.track(factors = rownames(mat), x = df_tmp$entropy, y = df_tmp$coocs,
#             panel.fun = function(x, y) {
#               print(x)
#               circos.points(x, y)
#             })
circos.clear()

####Combining matrices -----------------------

m1<-as.matrix(cooc[[1]])
m2<-as.matrix(cooc[[2]])

m1
length(m1[1,]) #62
m2
length(m2[1,]) #216

#select unique row names and colnames
row_names_unique<- unique(c(rownames(m1),rownames(m2))) 
row_names_unique #242
col_names_unique<- unique(c(colnames(m1),colnames(m2)))
col_names_unique #242

#creates an EMPTY set that combines matA and matB with unique rownames and unique colnames
#(still needs to be initialized with the combined data of matA and matB)

emptyCombined <- matrix(data = 0,
                        nrow=(length(row_names_unique)),
                        ncol=(length(col_names_unique)),
                        dimnames = list(c(row_names_unique),c(col_names_unique)))
emptyCombined
length(emptyCombined[,1]) #242

#initializing empty matrix, combinedMat - combining matrices B and C
#using m3 as the final matrix:
m3<-emptyCombined
m3
#puts matrix B values in the matched places in m3:
m3[rownames(m3)%in%rownames(m1),colnames(m3)%in%colnames(m1)]=m1 
m3
length(m3[1,])
#puts and combines matrix c into the final m3 matrix
m3[rownames(m3)%in%rownames(m2),colnames(m3)%in%colnames(m2)]<-m3[rownames(m3)%in%rownames(m2),colnames(m3)%in%colnames(m2)]+m2 
m3

#compare results:
m1
m2
m3 #FINAL combined matrix

m3[lower.tri(m3)] <- 0 #<---are these two lines needed before making the circ graph?
par(mar = c(0, 0, 0, 0))

clusters <- read.csv2("resources/output/post/A_Tale_of_Two_Cities-TSNE-cluster.csv",sep=";",header = T) ##<--


### Assigning unique colours for clustering
library(RColorBrewer)
n <- nrow(unique(clusters))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colcodes <- sample(col_vector, n)
colframe <- data.frame(terms=rownames(m3),clusters=clusters) #<---- need to combine clustering the corresponding books before proceeding
clusters
colframe$colcode <- colcodes[colframe$clusters]

#length(m3[1,])
#length(rownames(m3))

grid.col <- setNames(colframe$colcode, colframe$terms)

#let's try chordDiagram to visualize combined matrix of cooc1 and cooc2:
chordDiagram(m3, directional = FALSE, annotationTrack = "grid", preAllocateTracks = 1, )#grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  #circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

circos.clear()



####----------------------------------


mat<-as.matrix(cooc[[1]])
mat[lower.tri(mat)] <- 0

matA<-as.matrix(cooc[[1]])
matA[lower.tri(mat)] <- 0
matB<-as.matrix(cooc[[2]])
matB[lower.tri(mat)] <- 0

combinedMat<- merge(mat1,mat2)
combinedMat
length(combinedMat)
length(mat1)
length(mat2)
mat

t(combinedMat)



### 
### 1) Construct empty matrix with all terms as row names and col first

# combinedMat <- matrix(nrow=length(unique(c(rownames(mat1),rownames(mat2)))),
#                       ncol=length(unique(c(rownames(mat1),rownames(mat2)))), 
#                       rownames=unique(c(rownames(mat1),rownames(mat2))), 
#                       colnames=unique(c(rownames(mat1),rownames(mat2))))

#combining two matrices (two books)

matA<-as.matrix(cooc[[1]])
matA[lower.tri(mat)] <- 0
matB<-as.matrix(cooc[[2]])
matB[lower.tri(mat)] <- 0

# #list with duplicated words from matA and matB
# row_names_all<- c(rownames(matA),rownames(matB)) #length=278
# col_names_all<- c(colnames(matA),colnames(matB)) #length278

#unique list of words from matA and matB
row_names_unique<- unique(c(rownames(matA),rownames(matB))) #length=242
col_names_unique<- unique(c(colnames(matA),colnames(matB)))#length=242

#creates an EMPTY set that combines matA and matB with unique rownames and unique colnames
#(still needs to be initialized with the combined data of matA and matB)
combinedMat <- matrix(data = 0,
                      nrow=(length(row_names_unique)),
                      ncol=(length(col_names_unique)),
                      dimnames = list(c(row_names_unique),c(col_names_unique)))
combinedMat
rownames(combinedMat)
colnames(combinedMat)

### 2) Then initialise with values for all pairs you find in the individual matrices.

#loop to insert values from matA and matB to the combinedMat

#for(word in rownames(matA){ ... }

for(word in rownames(matA)){
  
}



#try chord diagram with m3
chordDiagram(m3, directional = FALSE, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  #circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

# i<-0
# while(i<length(matA)){
#   
#   
#   while()
#   matA[,i]
# }






combinedMat                      
                      
length(combinedMat[,1])                    
                     # rownames=row_names_unique, 
                      #colnames=col_names_unique)









row_names
length(col_names_all)
length(row_names_all) #242
length(col_names_unique) #62 - wrong
rownames(mat1)
colnames(mat1)
a<- length(unique(c(rownames(mat1),rownames(mat2))))
a
length(mat1[,1])
length(mat1[1,])
length(mat1)

row_length_uniqueA <-length(unique(rownames(mat1)))
col_length_uniqueA <-length(unique(colnames(mat1)))

row_length_uniqueB <-length(unique(rownames(mat2)))
col_length_uniqueB <-length(unique(colnames(mat2)))




x<-length(unique(rownames(mat1)))+length(unique(rownames(mat2)))
x

x
rownames(mat1)
rownames(mat2)



mat0<-as.matrix(cooc[[25]])
length(mat0)
