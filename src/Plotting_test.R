cor_dis <- smacof::sim2diss(cor_list[[26]])
   
  #rownames(mat) <- mat$X
  #mat <- mat[-1]
  #mat <- mat[,order(names(mat))]
 
  
  MDS <- smacof::mds(cor_dis, type = "ordinal", ndim = 2,
                     itmax = 2000)
  coordinates <- as.data.frame(MDS$conf) %>%
    tibble::rownames_to_column()
  
  ggsave(filename = paste0(paste0("../outputs save/random wordlist/output/", "1000", "/"),
                           "plot","25",".svg"),
                            device = "svg",
         plot =   coordinates %>%
                  ggplot() +
                  aes(x = D1, y = D2) +
                  geom_point() +
                  geom_label(aes(label = rowname)) +
                  theme_classic())