library(readr)

trait_words <- read_csv('../resources/pda500.txt', col_names = F) %>%
                .[[1]] %>% tolower(.) %>% unique(.)

trait_df <- as.data.frame(matrix(nrow = length(trait_words),
                       ncol = length(trait_words),
                       data = 0,
                       dimnames = list(trait_words,
                                       trait_words)))

trait_df <- dplyr::add_rownames(trait_df, "X")
  


for(sliceSize in list(1000, "sentence")){
  allmatrices <- list.files(paste0("../resources/output/", sliceSize, "/"),
                             pattern = "(.*)_network_matrix.csv",
                            full.names = T)
}

test <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))

for(bb in 1:length(test)){
  colnames(test[[bb]])[1] <- "X"
}

test[[length(test) + 1]] <- trait_df


binded <- rbind.fill(test)

binded[is.na(binded)] <- 0
result <- aggregate(binded[-1], by = list(binded$X), FUN = sum)
rownames(result) <- result$Group.1
result <- result[-1]
result <- result[,order(names(result))]

probs <- result / rowSums(result)



is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

probs[is.nan.data.frame(probs)] <- 0

probs <- probs[-which(rowSums(probs) == 0 ),-which(rowSums(probs) == 0 )]

psych::principal(probs, nfactors = 5, rotate = "varimax") %>%
  psych::fa.sort()
