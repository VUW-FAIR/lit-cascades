library(tidyverse)
library(readr)
  
trait_words <- read_csv('../resources/random-500.txt', col_names = F) %>%
                .[[1]] %>% tolower(.) %>% unique(.)

## Creating empty data frame for storage
trait_df <- as.data.frame(matrix(nrow = length(trait_words),
                       ncol = length(trait_words),
                       data = 0,
                       dimnames = list(trait_words,
                                       trait_words)))

## Making rowname column into rownames to keep matrix numeric
trait_df <- dplyr::add_rownames(trait_df, "X")
  

## Creating a list with all co-ocurrence matrices for all outputs
allmatrices <- list.files(paste0("../outputs save/random wordlist/output/", "1000", "/"),
                            pattern = "(.*)_network_matrix.csv",
                            full.names = T)

alllinks <- list.files(paste0("../outputs save/random wordlist/output/", "1000", "/"),
                            pattern = "(.*)_links.csv",
                            full.names = T)
## Fetching the data of the co-ocurrence matrices
cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))
link <- lapply(alllinks, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))
for(bb in 1:length(cooc)){
  colnames(cooc[[bb]])[1] <- "X"
}


cor_list <- list()
for(sent in link){
  keyword_cors <- sent %>% 
  group_by(tag) %>%
  #filter(n() >= 50) %>%
  widyr::pairwise_cor(tag, target, sort = TRUE, upper = T)
  keyword_mat <- reshape2::acast(keyword_cors, item1 ~ item2,
                                 value.var = "correlation")
  keyword_mat[is.na(keyword_mat)] <- 1
  
  cor_list[[length(cor_list) + 1]] <- keyword_mat
}

## Fusing the individuals matrices into a single filled matrix
#cooc[[length(cooc) + 1]] <- trait_df
#binded <- plyr::rbind.fill(cooc)
#binded[is.na(binded)] <- 0
## Making rows unique
#result <- aggregate(binded[-1], by = list(binded$X), FUN = sum)

plotlist <- list()
plotnumber <- 1
for (mat in cooc){
lapply(cor_list, function(x){
  cor_dis <- smacof::sim2diss(x)
   
  #rownames(mat) <- mat$X
  #mat <- mat[-1]
  #mat <- mat[,order(names(mat))]
 
  
  MDS <- smacof::mds(cor_dis, type = "ordinal", ndim = 2,
                     itmax = 2000)
  coordinates <- as.data.frame(MDS$conf) %>%
    tibble::rownames_to_column()
  
  ggsave(filename = paste0(paste0("../outputs save/random wordlist/output/", "1000", "/"),
                           "plot",plotnumber,".svg"),
                            device = "svg",
         plot =   coordinates %>%
                  ggplot() +
                  aes(x = D1, y = D2) +
                  geom_point() +
                  geom_text(aes(label = rowname)) +
                  theme_classic())
plotnumber <- plotnumber + 1
  
})
  
  
  ## Removing 0 from matrix
  #cleaned_result <- result[-which(rowSums(result) <= 10),
  #                         -which(colSums(result) <= 10)]
  
  ### Converting similarity into a dissimilarity matrix 
  #cleaned_result <- max(cleaned_result) - cleaned_result
  #rownames(cleaned_result) <- colnames(cleaned_result)
}



### Stress plot 
#stress_values <- c()
#for(ii in 1:10){
#  distance_MDS <- smacof::mds(cleaned_result, ndim = ii,
#                              type =   "ordinal")
#  stress_values[ii] <- distance_MDS$stress
#}
### Ordinal PCA

##3 Ties breaking Berg Gronenen 2015

### Unique ? Common ?







## Probabilities?
#probs <- result / rowSums(result)
#is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
#probs[is.nan.data.frame(probs)] <- 0
#probs <- probs[-which(rowSums(probs) == 0 ),-which(rowSums(probs) == 0 )]
#psych::principal(probs, nfactors = 5, rotate = "varimax") %>%
#  psych::fa.sort()
