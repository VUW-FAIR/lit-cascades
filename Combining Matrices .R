####COMBINING TWO MATRICES:

####Uses two different cooc matrices (still needs to be refined)

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
#(crashes here)
chordDiagram(m3, directional = FALSE, annotationTrack = "grid", preAllocateTracks = 1, )#grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  #circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

circos.clear()

##### Combining two small matrices together and shows final output of one combined matrix: -------------


#created matrix1
m1 <- matrix( 
  c(2, 4, 3, 1, 5, 7), 
  nrow=3, 
  ncol=2,
  dimnames = list(c('excited','sad','curious'),
                  c('happy','bored')))
m1

#add a column in m1
m1<-cbind(m1,hopeful = c(1,2,3))
m1

#add a row in m1
m1<-rbind(m1,grumpy = c(3,2,1))
m1

#created matrix m2
m2 <- matrix(
  c(1, 1, 1, 1, 1, 1), 
  nrow=3, 
  ncol=2,
  dimnames = list(c('hopeful','excited','creepy'),
                  c('dumb','happy')))
)
m2

#select unique row names and colnames
row_names_unique<- unique(c(rownames(m1),rownames(m2))) 
row_names_unique
col_names_unique<- unique(c(colnames(m1),colnames(m2)))
col_names_unique

#creates an EMPTY set that combines matA and matB with unique rownames and unique colnames
#(still needs to be initialized with the combined data of matA and matB)

emptyCombined <- matrix(data = 0,
                        nrow=(length(row_names_unique)),
                        ncol=(length(col_names_unique)),
                        dimnames = list(c(row_names_unique),c(col_names_unique)))
emptyCombined

#initializing empty matrix, combinedMat - combining matrices B and C
#using m3 as the final matrix:
m3<-emptyCombined
m3
#puts matrix B values in the matched places in m3:
m3[rownames(m3)%in%rownames(m1),colnames(m3)%in%colnames(m1)]=m1 
m3
m3[1,]
#puts and combines matrix c into the final m3 matrix
m3[rownames(m3)%in%rownames(m2),colnames(m3)%in%colnames(m2)]<-m3[rownames(m3)%in%rownames(m2),colnames(m3)%in%colnames(m2)]+m2 
m3

#compare results:
m1
m2
m3 #FINAL combined matrix


###----------------------
