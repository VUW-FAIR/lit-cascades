## Creating a list with all co-ocurrence matrices for all outputs
allmatrices <- list.files(paste0("resources/output/", "sentence", "/"),
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
  #cooc[[bb]] <- cooc[[bb]][-which(rowSums(cooc[[bb]]) < ceiling(max(rowSums(cooc[[bb]])) * .10)),
  #                         -which(colSums(cooc[[bb]]) < ceiling(max(colSums(cooc[[bb]])) * .10))]
  
}

getwd()
setwd("TIC-VUW/tic-personality-words")
getwd()








library(circlize)

mat<-as.matrix(cooc[[25]])
#mat<-cooc[[25]]
#grid.col <- setNames(rainbow(length(unlist(dimnames(mat)))), union(rownames(mat), colnames(mat)))
par(mar = c(1, 1, 1, 1))
#chordDiagram(mat, grid.col = grid.col, annotationTrack = "grid")
#chordDiagram(mat, grid.col = grid.col, annotationTrack = c("name", mat),
#            annotationTrackHeight = c(0.03, 0.01))
#chordDiagram(mat, grid.col = grid.col, annotationTrack = NULL)
#chordDiagram(mat, order = sort(union(from, to)), directional = TRUE) #original
#circos.clear()

 chordDiagram(mat,  grid.col = NULL, grid.border = NA, transparency = 0.5,
              col = NULL, row.col = NULL, column.col = NULL,
              order = NULL, directional = 0, xmax = NULL, annotationTrack = "grid",  
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(mat))))))

# we go back to the first track and customize sector labels

#circos.track(track.index = 1, panel.fun = function(x, y) {
#  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
#              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
#}, bg.border = NA) # here set bg.border to NA is important



# original image
#chordDiagram(mat, grid.col = grid.col) 




# now, the image with rotated labels
## chordDiagram(mat, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col) 

# circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  # xlim = get.cell.meta.data("xlim")
  # ylim = get.cell.meta.data("ylim")
  # sector.name = get.cell.meta.data("sector.index")
  # circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  # circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
# }, bg.border = NA)

#chordDiagram(mat, col = col[df$Customer.Sat], diffHeight = diffHeight[df$Customer.Sat], annotationTrack = "grid", preAllocateTracks = 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), col = "lightgray")
}, bg.border = NA)


circos.info()
circos.clear()


