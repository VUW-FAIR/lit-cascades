library(tidyverse)
library(readr)
library(gridExtra)  

# Function ----------------------------------------------------------------
plot_cluster <- function(data, var_cluster, palette)  
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
  geom_point(size= 1) +
  geom_label(aes(label = rowname)) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "horizontal") + 
    scale_colour_brewer(palette = palette) 
}

  
trait_words <- read_csv('../resources/Personal Traits.txt', col_names = F) %>%
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
allmatrices <- list.files(paste0("../outputs save/Allport/", "1000", "/"),
                            pattern = "(.*)_network_matrix.csv",
                            full.names = T)

alllinks <- list.files(paste0("../outputs save/Allport/", "1000", "/"),
                            pattern = "(.*)_links.csv",
                            full.names = T)
## Fetching the data of the co-ocurrence matrices
cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))
link <- lapply(alllinks, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))
for(bb in 1:length(cooc)){
  rownames(cooc[[bb]]) <- cooc[[bb]][,1]
  cooc[[bb]][,1] <- NULL
}

tsn_list <- list()
for(sent in cooc){

  test <- Rtsne::Rtsne(test_cooc,check_duplicates=FALSE,
                     pca=TRUE, perplexity=5, theta=0.5, dims=2)
  
  tsn_list[[length(tsn_list) + 1]] <- test
}


for(number in 1:length(tsn_list)){
d_tsne_1 <- as.data.frame(test$Y) 
d_tsne_1 <- cbind(d_tsne_1, rowname = test_cooc$rowname) 
 
rng<-2:10 #K from 2 to 20
tries <-1000 #Run the K Means algorithm 100 times 
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points 
for(v in rng){ # For each value of the range variable
   v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
   for(i in 1:tries){
     k.temp <-kmeans(d_tsne_1[-3],centers=v) #Run kmeans
     v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
   }
   avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss 
   }

plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
      ylab="Average Total Within Sum of Squares",
      xlab="Value of K")

## keeping original data
d_tsne_1_original = d_tsne_1

## Creating k-means clustering model, and assigning the result to the data used to create the tsne
fit_cluster_kmeans = kmeans(scale(d_tsne_1[-3]), 6)  
d_tsne_1_original$cl_kmeans = factor(fit_cluster_kmeans$cluster)

## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne
fit_cluster_hierarchical=hclust(dist(scale(d_tsne_1[-3])))

## setting 3 clusters as output
d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical, k=6)) 

plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Accent")  
plot_h=plot_cluster(d_tsne_1_original, "cl_hierarchical", "Set1")

## and finally: putting the plots side by side with gridExtra lib...
grid.arrange(plot_k, plot_h,  ncol=2)

}

