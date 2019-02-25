# library(ggplot2)
# library(gridExtra)
library(FactoMineR)
library(sparsepca)
library(kohonen)
library(cluster)
library(fpc)
library(vegan)
library(psych)

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

combineBooks <- function(book_ids, cooc) {
  # Collapse cooc matrix for books belonging to same author into single matrix
  # - Read in files, put all words into a data structure
  # - Create new matrix based on number of unique words
  # - Add data into new matrix from each book's original matrix
  book_words <- NULL
  for (i in book_ids) {
    book_words <- c(book_words, rownames(cooc[[i]]))
  }
  book_words <- unique(book_words)
  
  # Merge cooc matrices
  combined_cooc <- matrix(0, nrow = length(book_words), ncol = length(book_words))
  for (i in book_ids) {
    word_rows <- rownames(cooc[[i]])
    for (iw in 1:length(word_rows)) {
      word <- word_rows[iw]
      wordLoc <- length(which(word == book_words))
      if (length(wordLoc) != 0) {
        j <- iw+1
        jmax <- nrow(cooc[[i]][iw])
        while (j < jmax) {
          if (cooc[[i]][iw,j] != 0) {
            word2 <- colnames(cooc[[i]])[j]
            word2Loc <- which(word2 == book_words)
            if (length(word2Loc != 0)) {
              combined_cooc[word2Loc,wordLoc] <- combined_cooc[word2Loc,wordLoc] + 1
              combined_cooc[wordLoc,word2Loc] <- combined_cooc[wordLoc,word2Loc] + 1
            }
          }
          j <- j + 1
        }
      }
    }
  }
  rownames(combined_cooc) <- book_words
  return (combined_cooc)
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
  book_names <- list()
  count <- 1
  alllinks_extended <- list()
  book_names_extended <- list()
  
  cooc_modified <- cooc
  
  combine_books <- TRUE
  
  if (combine_books) {
    austen_books <- c(7,13,17,20,22,23)
    dickens_books <- c(1,2,3,5,6,10,11,12,14,15,16,18,19,21,24)
    
    # Add to cooc data
    cooc_modified[[length(cooc_modified)+1]] <- combineBooks(austen_books, cooc)
    book_names_extended[[length(book_names_extended)+1]] <- alllinks_extended[[length(alllinks_extended)+1]] <- "All Jane Austen Books"
    
    cooc_modified[[length(cooc_modified)+1]] <- combineBooks(dickens_books, cooc)
    book_names_extended[[length(book_names_extended)+1]] <- alllinks_extended[[length(alllinks_extended)+1]] <- "All Charles Dickens Books"
  }
  
  spca_start_book <- 1
  spca_rotation <- TRUE
  book_count <- length(cooc_modified) - spca_start_book + 1
  
  if (spca_rotation) {
    #Load psych loadings
    #psych_data <- read.csv("../../Some Data/1710tdaStructures2to7factors.csv", header = FALSE)
    psych_data <- read.csv("../../Some Data/1710tdaStructures2to7factors2.csv", header = FALSE)
    proc_results_df <- data.frame()
    cong_length <- sum(2:7)+5
    cong_matrix <- matrix(NA,nrow = book_count, ncol = cong_length)
    cong_matrix2 <- matrix(NA,nrow = book_count, ncol = cong_length)
    
    count <- 1
    proc_results <- list()
    for (sent in cooc_modified) {
      if (count < spca_start_book) {
        count <- count + 1
        next
      }
      
      if (count > length(alllinks)) {
        plot_title_list[count] <- alllinks_extended[[count - length(alllinks)]]
      } else {
        plot_title <- gsub("resources/output/sentence//|\\_links\\.csv","",alllinks[[count]])
        plot_split <- strsplit(plot_title, .Platform$file.sep, fixed = FALSE)
        word_set <- strsplit(plot_split[[1]][length(plot_split[[1]])-1], "-")[[1]][1]
        book_names[count] <- plot_split[[1]][length(plot_split[[1]])]
        plot_title_list[count] <- paste0(word_set, "_", plot_split[[1]][length(plot_split[[1]])])
      }
      
      proc_results[[count]] <- list()
      #Produce spca for each book using 2 - 7 components   
      mi <- 1
      ms <- 1
      sent_rows <- rownames(sent)
      csv_path <- "../../CSV/"
      txt_path <- "../../TXT/"
      congru <- list()
      congru2 <- list()
      
      
      for (i in 2:7) {
        if (i > ncol(sent)) {
          break
        }
        
        loadings <- sparsepca::spca(sent, k=i, scale = TRUE)
        mx <- mi + (i-2) + 2
        
        #Get relevant psych_data
        psych_data_i <- psych_data[c(mi:mx)]
        
        #Create new data using words that exist in both data sets
        #Psych words are truncated to 8 chars
        matches <- array(0,c(nrow(sent),3))
        match_count <- 1
        for (sent_index in 1:nrow(sent)) {
          search_word <- stringr::str_trunc(sent_rows[sent_index],width = 8,ellipsis = "")
          psych_row <- psych_data_i[psych_data_i[,1] == search_word,]
          if (nrow(psych_row) == 1) {
            matches[match_count,] <- c(sent_rows[sent_index], sent_index, as.integer(rownames(psych_row)))
            match_count <- match_count + 1
          }
        }

        #Convert to data frame and sort by sent index
        matches_df <- data.frame(matches[,1],as.numeric(matches[,2]),as.numeric(matches[,3]))
        matches_df <- matches_df[matches_df[,1]!=0,]
        colnames(matches_df) = c("term", "sent_index", "psych_index")
        matches_df <- matches_df[order(matches_df[,2]),]
        rownames(matches_df) = c(1:nrow(matches_df))
        
        #Filter psych data by matched words
        psych_data_m <- psych_data_i[matches_df[,3],-1]
        filtered_loadings <- loadings$loadings[matches_df[,2],]
        
        #Perform rotation
        rotation <- protest(psych_data_m, filtered_loadings)
        #Produces same result
        rotation2 <- MCMCpack::procrustes(filtered_loadings, psych_data_m, translation = TRUE, dilation = TRUE)
        
        #Keep significance for plotting
        proc_results[[count]][[i-1]] <- rotation$signif
        
        #Output data to csv
        #SPCA Loadings
        rownames(loadings$loadings) <- sent_rows
        write.csv(loadings$loadings,paste0(csv_path,plot_title_list[count],"_k_",i,"_SPCA_Loadings",".csv"))
        #Rotated Matrix Vegan
        yrot <- rotation$Yrot
        rownames(yrot) <- matches_df[,1]
        write.csv(yrot,paste0(csv_path,plot_title_list[count],"_k_",i,"_vegan_rotated_loadings",".csv"))
        #Rotated Matrix MCMCpack
        yrot <- rotation2$X.new
        rownames(yrot) <- matches_df[,1]
        write.csv(yrot,paste0(csv_path,plot_title_list[count],"_k_",i,"_MCMCpack_rotated_loadings",".csv"))
        
        
        #Congruence table
        congru[[i-1]] <- diag(fa.congruence(rotation$Yrot, psych_data_m))
        congru2[[i-1]] <- diag(fa.congruence(rotation2$X.new, psych_data_m))
        #write.csv2(congruence, paste0(csv_path,plot_title_list[cout],"_k_"))
        
        #Update psych column position
        mi <- mi + 4 + ms
        ms <- ms + 1
      }
      
      adj_count <- count - spca_start_book + 1
      for (j in 1:length(congru)) {
        if (j == 1) {
          cong_row <- congru[[j]]
        } else {
          cong_row <- c(cong_row,"",congru[[j]])
        }
      }
      while (length(cong_row) < cong_length) {
        cong_row <- c(cong_row, "")
      }
      cong_matrix[adj_count,] <- t(matrix(cong_row))
      
      for (j in 1:length(congru2)) {
        if (j == 1) {
          cong_row2 <- congru2[[j]]
        } else {
          cong_row2 <- c(cong_row2,"",congru2[[j]])
        }
      }
      while (length(cong_row2) < cong_length) {
        cong_row2 <- c(cong_row2, "")
      }
      cong_matrix2[adj_count,] <- t(matrix(cong_row2))
      
      if (adj_count == 1) {
        proc_results_df <- as.data.frame(proc_results[[count]], col.names = c(2:min(ncol(sent),7)))
      } else {
        proc_results_df[adj_count,] <- as.data.frame(proc_results[[count]], col.names = c(2:min(ncol(sent),7)))
      }
      #Write significance for each K value
      # proc <- as.data.frame(proc_results_df[count,])
      # colnames(proc) <- paste0("K=",c(2:7))
      # a<- paste0(csv_path,plot_title_list[count],"_all_significance_values",".csv")
      # write.csv2(proc,a)
      #write.infile(congru,paste0(txt_path,plot_title_list[count],"_congruence_vegan",".txt"))
      #write.infile(congru2,paste0(txt_path,plot_title_list[count],"_congruence_MCMCpack",".txt"))
      
      count <- count + 1
    }
    rownames(cong_matrix) <- plot_title_list[spca_start_book:(spca_start_book + (book_count - 1))]
    rownames(cong_matrix2) <- plot_title_list[spca_start_book:(spca_start_book + (book_count - 1))]
    write.csv(cong_matrix,paste0(csv_path,"All_Books_Congruence_vegan.csv"))
    write.csv(cong_matrix2,paste0(csv_path,"All_Books_Congruence_MCMCpack.csv"))
    
    
    #1 Line plot showing each book (x = K(2:7), y = congruence(average for K)) for vegan + MCMCpack
    line_data <- matrix(NA, nrow=nrow(cong_matrix), ncol=6)
    for (i in 1:nrow(cong_matrix)) {
      mi <- 1
      ms <- 1
      for (j in 1:6) {
        line_data[i,j] <- mean((as.double(cong_matrix[i,mi:(mi+ms)])))
        
        mi <- mi + ms + 2
        ms <- ms + 1
      }
    }
    
    line_data2 <- matrix(NA, nrow=nrow(cong_matrix2), ncol=6)
    for (i in 1:nrow(cong_matrix2)) {
      mi <- 1
      ms <- 1
      for (j in 1:6) {
        line_data2[i,j] <- mean((as.double(cong_matrix2[i,mi:(mi+ms)])))
        
        mi <- mi + ms + 2
        ms <- ms + 1
      }
    }
    colnames(line_data) <- c(2:7)
    colnames(line_data2) <- c(2:7)
    rownames(line_data)
    pcol = randomcoloR::distinctColorPalette(nrow(cong_matrix))
    
    layout(matrix(c(1,2),nrow=1), width=c(3,1))
    par(mar=c(5,4,4,0))
    matplot(t(line_data), lwd=3.5, type="l", col = pcol, ylab = "Average Congruence", xlab = "Value of K", main = "Vegan Procrustes - Congruence")
    par(mar=c(5,0,4,2))
    plot(c(0,1),type="n", axes=F, xlab="", ylab="")
    legend("topright", legend=c(book_names,book_names_extended), cex=0.5, pcol, fill=pcol, y.intersp = 0.6)
    
    layout(matrix(c(1,2),nrow=1), width=c(3,1))
    par(mar=c(5,4,4,0))
    matplot(t(line_data2), lwd=3, type="l", col = pcol, ylab = "Average Congruence", xlab = "Value of K", main = "MCMCpack Procrustes - Congruence")
    par(mar=c(5,0,4,2))
    plot(c(0,1),type="n", axes=F, xlab="", ylab="")
    legend("topright", legend=c(book_names,book_names_extended), cex=0.5, pcol, fill=pcol, y.intersp = 0.6)
  }
  
  #Whether to save all images or not (TRUE = YES)
  all_plot_mode <- FALSE
  image_path <- "../../Images/Cooc/"
  
  # 0 disables a particular plot, 1 enables
  # Plots are:
  # 1. TSNE -> KMeans
  # 2. SPCA -> TSNE -> KMeans
  # 3. SOM -> TSNE
  # 4. SOM -> PCA
  # 5. SOM -> PAM (requires #3)
  # 6. DBScan
  plots <- c(0,0,0,0,0,0)
  
  if (sum(plots) != 0) {
    for(sent in cooc) {
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
      if (plots[1]) {
        test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                           pca=TRUE, perplexity = max(1,floor(nrow(sent)/3)-1), theta=0, dims=2)
      }
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
      if (plots[2]) {
        test3 <- sparsepca::robspca(sent, verbose = FALSE)
        test3$tsne <- Rtsne::Rtsne(test3$loadings, dims = 2, pca=FALSE, perplexity = nrow(test3$loadings)/3-1, theta = 0)
      }
      
      #Self Ordering Map -> TSNE
      #-------------------------
      # Recommended size by Vesanto - but causes issues with sample.int
      #grid <- 5*sqrt(nrow(sent)))
      # grid <- sqrt(nrow(sent)-2)
      # 
      if (plots[3]) {
        test4 <- som(scale(sent), grid = somgrid(floor(grid/2), ceiling(grid/2), "hexagonal"))
        test4$points <- t(as.data.frame(test4$codes))
        test4$col <- randomcoloR::distinctColorPalette(max(test4$unit.classif))
        test4$maxPerp = max(1, floor(nrow(test4$points) / 3) - 1)
        test4$tsne <- Rtsne::Rtsne(test4$points, check_duplicates=FALSE, dims = 2, pca=FALSE, perplexity = test4$maxPerp, theta = 0)
      }
      
      #SOM -> PCA
      #----------
      if (plots[4]) {
        test5 <- as.data.frame(PCA(test4$points, ncp = 2, graph = FALSE)$ind$coord)
        colnames(test5) <- c("V1", "V2")
        sub_title <- "SOM_PCA"
        plot(test5$V1, test5$V2, col = test4$col[test4$unit.classif], cex = plot_cex, cex.axis = plot_cex_axis, cex.lab = plot_cex_lab, cex.main = plot_cex_main, pch=20,main = paste0(plot_title, " \n", sub_title))
        text(x=test5$V1, y=test5$V2, cex=0.2, pos=4, labels=(rownames(test5)))
        savePlot(sub_title, count)
      }
      
      #Clusplot and PAM - No good, usually just 1 cluster
      # test6 <- pamk(sent, krange=2:nrow(sent))-1)
      # sub_title <- "PAM"
      # clusplot(pam(sent, test6$nc), lines = 0, labels = 3, cex.txt = 0.6, main = paste0(plot_title, "\n", sub_title))
      # savePlot(sub_title, count)
      
      #Clusplot and PAM on SOM
      #-----------------------
      if (plots[3] && plots[5]) {
        test4$somPam <- pamk(test4$points, krange=2:nrow(sent)-1)
        sub_title <- "SOM_PAM"
        clusplot(pam(test4$points, test4$somPam$nc), cex.axis = plot_cex_axis, cex.lab = plot_cex_lab, sub = "", cex = plot_cex_clus, cex.main = plot_cex_main, lines = 0, labels = 3, cex.txt = plot_cex_txt_clus, main = paste0(plot_title, "\n", sub_title))
        savePlot(sub_title, count)
      }
      
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
      if (plots[6]) {
        ds <- dbscan::dbscan(scale(test$Y), 4, 1)
        cc <- randomcoloR::distinctColorPalette(max(ds$cluster))
        sub_title <- "dbscan"
        plot(test$Y[,1], test$Y[,2], col=colpal, cex.axis = plot_cex_axis, cex=plot_cex, cex.lab=plot_cex_lab, cex.main = plot_cex_main, pch=20, main = paste0(plot_title, "\n", sub_title))
        text(x=test$Y[,1], y=test$Y[,2], cex=0.2, pos=4, labels=rownames(sent))
        savePlot(sub_title, count)
      }
      
      if (plots[1]) {
        tsn_list[[length(tsn_list) + 1]] <- test
      }
      if (plots[3]) { 
        tsn_list2[[length(tsn_list2) + 1]] <- test4
      }
      if (plots[2]) {
        tsn_list3[[length(tsn_list3) + 1]] <- test3$tsne
      }
      names_list[[length(names_list) + 1]] <- rownames(sent)
      count = count + 1
      if (count > max_books) {
        break;
      }
    }
  }
  
  if (plots[1]) {
    tsne_plotting(tsn_list)
  }
  if (plots[3]) {
    tsne_plotting(tsn_list2, 2)
  }
  if (plots[2]) {
    tsne_plotting(tsn_list3, 1)
  }
  dir_count = dir_count + 1
}


