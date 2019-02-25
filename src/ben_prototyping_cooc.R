# library(ggplot2)
# library(gridExtra)
library(FactoMineR)
library(sparsepca)
library(kohonen)
library(cluster)
library(fpc)

# Clear any previous plots
if(!is.null(dev.list())) dev.off()

tsne_plotting <- function(tsn_list, type = 0, percent = .05){
  
  test_results <- list()
  
  for(number in 1:length(tsn_list)){
    if (number > max_books) { break; }
    if (type != 2) {
      d_tsne_1 <- as.data.frame(tsn_list[[number]]$Y) 
      d_tsne_1 <- cbind(d_tsne_1, rowname = names_list[[number]])   
    } else if (type == 2) {
      d_tsne_1 <- as.data.frame(tsn_list[[number]]$tsne$Y)
      d_tsne_1 <- cbind(d_tsne_1, rowname = names_list[[number]])
    }
    
    
    #components_number <- getClusterNumber(d_tsne_1[-3],percent)
    
    ## keeping original data
    d_tsne_1_original <-  d_tsne_1
    
    ## Creating k-means clustering model, and assigning the result to the data used to create the tsne
    #print(components_number)
    #fit_cluster_kmeans <-  kmeans(scale(d_tsne_1[-3]), components_number)
    if (type != 2) {
      fit_cluster_kmeans <- fpc::kmeansruns(scale(d_tsne_1[-3]),krange=2:(nrow(d_tsne_1)/2),critout=F,runs=5,criterion="ch")
      # print(kmIC(fit_cluster_kmeans))
      # print(paste0("Clusters: ", fit_cluster_kmeans$bestk))
      
      test_results[[number]] <- evaluate_centers(d_tsne_1, fit_cluster_kmeans, names_list[[number]])
      
      colpal <- randomcoloR::distinctColorPalette(fit_cluster_kmeans$bestk)
      
      d_tsne_1_original$cl_kmeans <- factor(fit_cluster_kmeans$cluster)
    }
    
    ## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne
    # fit_cluster_hierarchical <- hclust(dist(scale(d_tsne_1[-3])))
    
    ## setting 3 clusters as output
    # d_tsne_1_original$cl_hierarchical <-  factor(cutree(fit_cluster_hierarchical, k = fit_cluster_kmeans$bestk)) 
    
    # plot_k <- plot_cluster(d_tsne_1_original, "cl_kmeans", "Paired")  
    # plot_h <- plot_cluster(d_tsne_1_original, "cl_hierarchical", "Paired")
    
    ## and finally: putting the plots side by side with gridExtra lib...
    # grid.arrange(plot_k, plot_h,  ncol = 2)
    
    if (type != 2) {
      if (type == 1) {
        sub_title <- "SPCA_TSNE"
      } else {
        sub_title <- "TSNE_KMeans"
      }
      plot_full_title <- paste0("Co-occurrence ",gsub("resources/output/sentence//|\\_links\\.csv","",alllinks[[number]]),
                                " k=",fit_cluster_kmeans$bestk,
                                " ch=",max(fit_cluster_kmeans$crit), "\n", sub_title)
      plot(d_tsne_1_original$V1, d_tsne_1_original$V2, col=colpal[fit_cluster_kmeans$cluster], cex = plot_cex, cex.lab = plot_cex_lab, cex.axis = plot_cex_axis, cex.main = plot_cex_main, pch=20,main = plot_full_title)
    } else {
      plot_full_title <- paste0("Co-occurrence ",gsub("resources/output/sentence//|\\_links\\.csv","",alllinks[[number]]),
                                " \nSOM - TSNE")
      sub_title <- "SOM_TSNE"
      plot(d_tsne_1_original$V1, d_tsne_1_original$V2, col=tsn_list[[number]]$col[tsn_list[[number]]$unit.classif],cex = plot_cex, cex.lab = plot_cex_lab, cex.axis = plot_cex_axis, cex.main = plot_cex_main,pch=20,main = plot_full_title)
    }
    if (type == 0) {
      text(x=d_tsne_1_original$V1, y=d_tsne_1_original$V2, cex=0.2, cex=plot_cex_txt, pos=4, labels=(d_tsne_1_original$rowname))
    } else if (type == 1) {
      text(x=d_tsne_1_original$V1, y=d_tsne_1_original$V2, cex=0.2, cex=plot_cex_txt, pos=4, labels=(names_list[[number]]))
    } else if (type == 2) {
      text(x=d_tsne_1$V1, y=d_tsne_1$V2, cex=0.2, pos=4, labels=(names_list[[number]]))
    }
    savePlot(sub_title, number)
    #legend("topright", inset=c(-0.2,0), legend = paste("Cluster", fit_cluster_kmeans$cluster), pch=20, col=colpal[fit_cluster_kmeans$cluster], box.lty=0)
    
    # save cluster membership for confusion matrix
    # write.csv2(data.frame(clusters=fit_cluster_kmeans$cluster),paste0(gsub("\\_links\\.csv","",alllinks[[number]]),"-TSNE-cluster.csv"), row.names = F, col.names = F, sep = ";")
  }
  # print(test_results)
}

evaluate_centers <- function(data, fit, names) {
  #fit$clusters (list of assigned clusters)
  #compare each word (x, y) with cluster centers:
  # 1st Method - Euclidean Distance
  center_eval_list <- distance_centers(data, fit)
  
  result <- list()
  for (eval_num in 1:length(center_eval_list)) {
    result[[1]] <- list(word = names[eval_num], best_cluster = fit$cluster[1], 
                        closest_cluster = which.min(center_eval_list[[eval_num]]),
                        cluster_distances = center_eval_list[[eval_num]])
  }
  return (result)
}

euclid_distance <- function(p1, p2) {
  return (sqrt(sum((p1 - p2) ^ 2)))
}

distance_centers <- function(data, fit) {
  result <- list()
  point_index <- 1
  while (point_index < length(data[,1])) {
     point <- data[point_index,-3]
     
     center_index <- 1
     center_values <- double()
     while (center_index < length(fit$centers[,1])) {
       center_values[center_index] <- euclid_distance(point, fit$centers[center_index,])
       center_index <- center_index + 1
     }
     result[[point_index]] <- center_values
     point_index <- point_index + 1
  }
  return (result)
}

plot_cluster <- function(data, var_cluster, palette) {
  ggplot(data, aes_string(x = "V1", y = "V2", color = var_cluster)) +
    geom_point(size = 1) +
    geom_label(aes(label = rowname)) +
    guides(colour=guide_legend(override.aes = list(size = 6))) +
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

#evaluate kmeans cluster quality
kmIC <-  function(fit){
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = -fit$tot.withinss/2
  return(data.frame(AIC = D + 2 * m * k,
                    BIC = D + log(n) * m * k))
}

savePlot <- function(sub_title, count) {
  if (all_plot_mode) {
    dev.copy(png, filename=paste0(image_path,plot_title_list[count], "_", sub_title,".png"), width=plot_width, height=plot_height, units = plot_units, res=plot_res, pointsize = plot_pointsize)
    dev.off()
  }
}

# ======== Non Function Code =======

max_books <- 25
max_dirs <- 1

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
plot_cex_lab = 0.3
plot_cex_axis = 0.3

dirs <- list(
            #"allport-personal-traits-sentence-advs-lemma-book-centric",
            #"pda500-sentence-advs-lemma-book-centric"
            "pda1710-sentence-advs-lemma-book-centric"
)
dir_count <- 1
for (dir in dirs) {
  if (dir_count > max_dirs) {
    break
  }
  ## Creating a list with all co-ocurrence matrices for all outputs
  allmatrices <- list.files(paste0("../outputs save/",dir),
                            pattern = "(.*)_network_matrix.csv",
                            full.names = T)
  alllinks <- list.files(paste0("../outputs save/",dir),
                            pattern = "(.*)_links.csv",
                            full.names = T)
  allnodes <- list.files(paste0("../outputs save/",dir),
                            pattern = "(.*)_nodes.csv",
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
  
  tsn_list <- list()
  tsn_list2 <- list()
  tsn_list3 <- list()
  names_list <- list()
  plot_title_list <- list()
  count <- 1
  
  #Whether to save all images or not (TRUE = YES)
  all_plot_mode <- TRUE
  image_path <- "../../Images/Cooc/"
  
  for(sent in cooc){
    if (count < 0) {
      count = count + 1
      next
    }
    
    plot_title <- gsub("resources/output/sentence//|\\_links\\.csv","",alllinks[[count]])
    plot_split <- strsplit(plot_title, .Platform$file.sep, fixed = FALSE)
    word_set <- strsplit(plot_split[[1]][length(plot_split[[1]])-1], "-")[[1]][1]
    plot_title_list[count] <- paste0(word_set, "_", plot_split[[1]][length(plot_split[[1]])])
    
    print(paste(count,plot_title_list[count]))
    
    # TSNE with default perplexity of 3 and theta of 0.5
    # test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
    #                      pca=TRUE, perplexity = 3, theta=0.5, dims=2)
    
    #TSNE with maximum perplexity + minimum theta
    test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                         pca=TRUE, perplexity = max(1,floor(nrow(sent)/3)-1), theta=0, dims=2)
    # 
    #Alternative clustering
    #======================
    #Multi Dimensional Scaling + PCA
    #test2 <- cmdscale(sent, k = 5, eig = FALSE, list. = TRUE)
    #test2 <- as.data.frame(PCA(sent, graph = FALSE)$ind$coord)
    #colnames(test2) <- c("V1","V2")
    
    #SPCA - sparsepca (k = 2 : x, y)
    #-------------------------------
    #robspca, rspca, spca
    #defaults: alpha 1e-04, beta 1e-04, gamma 100, tol 1e-05
    #test3 <- sparsepca::robspca(sent, k=2, scale = TRUE)
    #K unrestricted (= num of words)
    test3 <- sparsepca::robspca(sent, verbose = FALSE)
    test3$tsne <- Rtsne::Rtsne(test3$loadings, dims = 2, pca=FALSE, perplexity = nrow(test3$loadings)/3-1, theta = 0)
    
    #Self Ordering Map -> TSNE
    #-------------------------
    # Recommended size by Vesanto - but causes issues with sample.int
    #grid <- 5*sqrt(nrow(sent)))
    # grid <- sqrt(nrow(sent)-2)
    # 
    test4 <- som(scale(sent), grid = somgrid(floor(grid/2), ceiling(grid/2), "hexagonal"))
    test4$points <- t(as.data.frame(test4$codes))
    test4$col <- randomcoloR::distinctColorPalette(max(test4$unit.classif))
    test4$maxPerp = max(1, floor(nrow(test4$points) / 3) - 1)
    test4$tsne <- Rtsne::Rtsne(test4$points, check_duplicates=FALSE, dims = 2, pca=FALSE, perplexity = test4$maxPerp, theta = 0)
    
    #SOM -> PCA
    #----------
    test5 <- as.data.frame(PCA(test4$points, ncp = 2, graph = FALSE)$ind$coord)
    colnames(test5) <- c("V1", "V2")
    sub_title <- "SOM_PCA"
    plot(test5$V1, test5$V2, col = test4$col[test4$unit.classif], cex = plot_cex, cex.axis = plot_cex_axis, cex.lab = plot_cex_lab, cex.main = plot_cex_main, pch=20,main = paste0(plot_title, " \n", sub_title))
    text(x=test5$V1, y=test5$V2, cex=0.2, pos=4, labels=(rownames(test5)))
    savePlot(sub_title, count)
    
    #Clusplot and PAM - No good, usually just 1 cluster
    # test6 <- pamk(sent, krange=2:nrow(sent))-1)
    # sub_title <- "PAM"
    # clusplot(pam(sent, test6$nc), lines = 0, labels = 3, cex.txt = 0.6, main = paste0(plot_title, "\n", sub_title))
    # savePlot(sub_title, count)
    
    #Clusplot and PAM on SOM
    #-----------------------
    test4$somPam <- pamk(test4$points, krange=2:nrow(sent)-1)
    sub_title <- "SOM_PAM"
    clusplot(pam(test4$points, test4$somPam$nc), cex.axis = plot_cex_axis, cex.lab = plot_cex_lab, sub = "", cex = plot_cex_clus, cex.main = plot_cex_main, lines = 0, labels = 3, cex.txt = plot_cex_txt_clus, main = paste0(plot_title, "\n", sub_title))
    savePlot(sub_title, count)
    
    #Clusplot and PAM on SOM TSNE (k=2) - Causes an error when plotting for some books
    # test4$pamk <- pamk(test4$tsne$Y, krange=2:nrow(sent))-1)
    # sub_title <- "SOM_TSNE_PAM"
    # clusplot(pam(test4$tsne$Y, test4$pamk$nc), lines = 0, labels = 3, cex.txt = 0.6, main = paste0(plot_title, " \n", sub_title))
    # savePlot(sub_title, count)
  
    #Clusplot and PAM on SOM PCA (k=2)
    #test5$pamk <- pamk(test5, krange=2:nrow(sent))-1)
    #clusplot(pam(test5, test5$pamk$nc), lines = 0, labels = 3, cex.txt = 0.6, main = "SOM PCA PAM")
  
    #DBSCAN
    #------
    ds <- dbscan::dbscan(scale(test$Y), 4, 1)
    cc <- randomcoloR::distinctColorPalette(max(ds$cluster))
    sub_title <- "dbscan"
    plot(test$Y[,1], test$Y[,2], col=colpal, cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0(plot_title, "\n", sub_title))
    text(x=test$Y[,1], y=test$Y[,2], cex=0.2, pos=4, labels=rownames(sent))
    savePlot(sub_title, count)
    
    tsn_list[[length(tsn_list) + 1]] <- test
    tsn_list2[[length(tsn_list2) + 1]] <- test4
    tsn_list3[[length(tsn_list3) + 1]] <- test3$tsne
    names_list[[length(names_list) + 1]] <- rownames(sent)
    count = count + 1
    if (count > max_books) {
      break;
    }
  }
  
  tsne_plotting(tsn_list)
  tsne_plotting(tsn_list2, 2)
  tsne_plotting(tsn_list3, 1)
  dir_count = dir_count + 1
}


