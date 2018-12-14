library(circlize)
mat<-as.matrix(cooc[[1]])
mat[lower.tri(mat)] <- 0
par(mar = c(0, 0, 0, 0))

clusters <- read.csv2("resources/output/post/A_Tale_of_Two_Cities-TSNE-cluster.csv",sep=";",header = T)

library(RColorBrewer)
n <- nrow(unique(clusters))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colcodes <- sample(col_vector, n, replace = T)

colframe <- data.frame(terms=rownames(mat),clusters=clusters)
colframe$colcode <- colcodes[colframe$clusters]

grid.col <- setNames(colframe$colcode, colframe$terms)

chordDiagram(mat, directional = FALSE, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  #circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

tmp <- read.csv2("resources/output/post/A_Tale_of_Two_Cities-allFeatures-output.csv",sep=";")
tmp[which(is.nan(tmp[,7])),7] <- 0

df_tmp <- tmp[which(tmp$terms %in% rownames(mat)),]

#circos.track(factors = rownames(mat), x = df_tmp$entropy, y = df_tmp$coocs,
#             panel.fun = function(x, y) {
#               print(x)
#               circos.points(x, y)
#             })
circos.clear()



set.seed(999)
n = 1000
df = data.frame(factors = sample(letters[1:8], n, replace = TRUE),
                x = rnorm(n), y = runif(n))


circos.par("track.height" = 0.1)
circos.initialize(factors = df$factors, x = df$x)




circos.track(factors = df$factors, y = df$y,
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(5, "mm"), 
                           CELL_META$sector.index)
               circos.axis(labels.cex = 0.6)
             })
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(df$factors, df$x, df$y, col = col, pch = 16, cex = 0.5)
circos.text(-1, 0.5, "text", sector.index = "a", track.index = 1)


bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(df$factors, df$x, bin.size = 0.2, bg.col = bgcol, col = NA)


circos.track(factors = df$factors, x = df$x, y = df$y,
             panel.fun = function(x, y) {
               ind = sample(length(x), 10)
               x2 = x[ind]
               y2 = y[ind]
               od = order(x2)
               circos.lines(x2[od], y2[od])
             })



circos.update(sector.index = "d", track.index = 2, 
              bg.col = "#FF8080", bg.border = "black")
circos.points(x = -2:2, y = rep(0.5, 5), col = "white")
circos.text(CELL_META$xcenter, CELL_META$ycenter, "updated", col = "white")


circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  xlim = CELL_META$xlim
  ylim = CELL_META$ylim
  breaks = seq(xlim[1], xlim[2], by = 0.1)
  n_breaks = length(breaks)
  circos.rect(breaks[-n_breaks], rep(ylim[1], n_breaks - 1),
              breaks[-1], rep(ylim[2], n_breaks - 1),
              col = rand_color(n_breaks), border = NA)
})



circos.link("a", 0, "b", 0, h = 0.4)
circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red",
            border = "blue", h = 0.2)
circos.link("e", 0, "g", c(-1,1), col = "green", border = "black", lwd = 2, lty = 2)


circos.clear()