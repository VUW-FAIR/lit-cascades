test_cooc <- cooc[[1]] %>%
  rownames_to_column(.) %>%
  gather(.,key = "key",value = "value", -rowname) %>%
  .[.[3] >10,] %>%
  spread(., key = key, value = value)

test_cooc[is.na(test_cooc)] <- 0




hist(test[test$value > 5,]$value)

test[order(test$value, decreasing = T),][1:20,]
