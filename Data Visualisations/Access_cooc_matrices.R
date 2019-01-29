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




