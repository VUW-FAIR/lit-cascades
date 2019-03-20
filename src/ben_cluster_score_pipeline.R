library(ROCR)
library(ggplot2)

PRF <- function (tic, human) {
  # Calculate precision, recall, and f-measure between TIC and Human coded data.
  
  df <- data.frame(pred = tic, real = human)
  scores <- list()
  for (i in seq(min(human), max(human))) {
    tp <- nrow(df[df$pred == i & df$real == i,])
    fp <- nrow(df[df$pred == i & df$real != i,])
    fn <- nrow(df[df$pred != i & df$real == i,])
    f1 <- (2 * tp) / (2 * tp + fp + fn)
    scores[[i]] <- f1
  }
  return (list(prediction = tic, labels = human, scores = scores, f = mean(unlist(scores))))
}

draw_trait_plot <- function(codes, coder, title, fname) {
  frame1 <- as.data.frame(codes$N)
  frame1[2,] <- as.data.frame(codes$E)
  frame1[3,] <- as.data.frame(codes$A)
  frame1[4,] <- as.data.frame(codes$C)
  frame1[5,] <- as.data.frame(codes$O)
  if (sum(codes$N$total, codes$E$total, codes$A$total, codes$C$total, codes$O$total) != 0) {
  
    rownames(frame1) <- c("N", "E", "A", "C", "O")
    
    # Plot
    data <- t(frame1[-1])
    barplot(data, main=paste0(title, coder), col = c("orange", "grey", "darkblue"), xlab = "Trait Code",
            legend = rownames(data), ylim = range(pretty(c(0, max(2,max(frame1[1]))))))
    savePlot(fname)
  }
}

draw_value_plot <- function(codes, coder, title, fname) {
  frame1 <- as.data.frame(codes$SE)
  frame1[2,] <- as.data.frame(codes$ST)
  frame1[3,] <- as.data.frame(codes$OC)
  frame1[4,] <- as.data.frame(codes$CO)
  if (sum(codes$SE$total, codes$ST$total, codes$OC$total, codes$CO$total) != 0) {
    rownames(frame1) <- c("SE", "ST", "OC", "CO")
    
    # Plot
    data <- t(frame1[-1])
    barplot(data, main=paste0(title, coder), col = c("orange", "grey", "darkblue"), xlab = "Trait Code",
            legend = rownames(data), ylim = range(pretty(c(0, max(2,max(frame1[1]))))))
    savePlot(fname)
  }
}

savePlot <- function(fname) {
  if (save_plots) {
    dev.copy(png, filename=paste0(image_path,fname), width=297, height=222, units = "mm", res = 300)
    dev.off()
  }
}

get_codes <- function(hc, id) {
  codes_by_character <- list()
  codes_by_trait <- list()
  codes_by_value <- list()
  # Group codings by character
  for (i in 1:nrow(hc)) {
    code_line <- hc[i,1]
    if (code_line == "" || code_line == " ") {
      next
    }
    
    split <- stringi::stri_split_fixed(code_line, ",")
    for (char_code in split[[1]]) {
      char_split <- stringi::stri_split_fixed(char_code, ":")
      if (trimws(char_split) == "") { next }
      char <- trimws(toupper(char_split[[1]][1]))
      trait <- trimws(toupper(char_split[[1]][2]))
      value <- trimws(toupper(char_split[[1]][3]))
      
      # Add code for char if it doesn't exist
      if (!char %in% names(codes_by_character)) {
        if (length(which(codes[,2] == char)) == 0) {
          print(paste0(char, " not found in code list"))
        }
        codes_by_character[[char]] <- list()
        codes_by_character[[char]]$"trait"$N <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        codes_by_character[[char]]$"trait"$E <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        codes_by_character[[char]]$"trait"$A <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        codes_by_character[[char]]$"trait"$C <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        codes_by_character[[char]]$"trait"$O <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        codes_by_character[[char]]$"value"$SE <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        codes_by_character[[char]]$"value"$ST <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        codes_by_character[[char]]$"value"$OC <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        codes_by_character[[char]]$"value"$CO <- list(total = 0, positive = 0, neutral = 0, negative = 0)
      }
      
      # Handle trait code
      # print(paste0((i+1), " ", trait))
      if (trait != "X") {
        trait_code <- stringi::stri_sub(trait, 1, 1)
        
        if (!trait_code %in% names(codes_by_trait)) {
          codes_by_trait[[trait_code]] <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        }
        
        if (length(which(traits == trait_code)) == 0) {
          print(paste0(id, "|", (i+1), " unknown trait: ", trait_code))
        }
        
        if (nchar(trait) == 2) {
          trait_mod <- stringi::stri_sub(trait, 2)
          if (trait_mod == "+") {
            codes_by_character[[char]]$"trait"[[trait_code]]$positive <- codes_by_character[[char]]$"trait"[[trait_code]]$positive + 1
            codes_by_trait[[trait_code]]$positive <- codes_by_trait[[trait_code]]$positive + 1
          } else if (trait_mod == "-") {
            codes_by_character[[char]]$"trait"[[trait_code]]$negative <- codes_by_character[[char]]$"trait"[[trait_code]]$negative + 1
            codes_by_trait[[trait_code]]$negative <- codes_by_trait[[trait_code]]$negative + 1
          } else {
            print(paste0((i+1), " Unrecognised trait modifier: ", trait_mod))
          }
        } else if (nchar(trait) == 1) {
          codes_by_character[[char]]$"trait"[[trait_code]]$neutral <- codes_by_character[[char]]$"trait"[[trait_code]]$neutral + 1
          codes_by_trait[[trait_code]]$neutral <- codes_by_trait[[trait_code]]$neutral + 1
        } else {
          print(paste0((i+1), " error in trait: ", trait))
        }
        codes_by_character[[char]]$"trait"[[trait_code]]$total <- codes_by_character[[char]]$"trait"[[trait_code]]$total + 1
        codes_by_trait[[trait_code]]$total <- codes_by_trait[[trait_code]]$total + 1
      }
      
      
      # Handle value code
      if (is.na(value)) {
        print(paste0((i+1), " missing value"))
        next
      }
      if (value != "X") {
        value_code <- stringi::stri_sub(value, 1, 2)
        
        if (!value_code %in% names(codes_by_value)) {
          codes_by_value[[value_code]] <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        }
        
        if (length(which(values == value_code)) == 0) {
          print(paste0(id, "|", (i+1), " unknown value: ", value_code))
        }
        
        if (nchar(value) == 3) {
          value_mod <- stringi::stri_sub(value, 3)
          if (value_mod == "+") {
            codes_by_character[[char]]$"value"[[value_code]]$positive <- codes_by_character[[char]]$"value"[[value_code]]$positive + 1
            codes_by_value[[value_code]]$positive <- codes_by_value[[value_code]]$positive + 1
          } else if (value_mod == "-") {
            codes_by_character[[char]]$"value"[[value_code]]$negative <- codes_by_character[[char]]$"value"[[value_code]]$negative + 1
            codes_by_value[[value_code]]$negative <- codes_by_value[[value_code]]$negative + 1
          } else {
            print(paste0((i+1), " Unrecognised value modifier: ", value_mod))
          }
        } else if (nchar(value) == 2) {
          codes_by_character[[char]]$"value"[[value_code]]$neutral <- codes_by_character[[char]]$"value"[[value_code]]$neutral + 1
          codes_by_value[[value_code]]$neutral <- codes_by_value[[value_code]]$neutral + 1
        } else {
          print(paste0((i+1), " error in value: ", value))
        }
        codes_by_character[[char]]$"value"[[value_code]]$total <- codes_by_character[[char]]$"value"[[value_code]]$total + 1
        codes_by_value[[value_code]]$total <- codes_by_value[[value_code]]$total + 1
      }
    }
  }
  return (list(codes_by_character = codes_by_character, codes_by_trait = codes_by_trait, codes_by_value = codes_by_value))
}

get_stats <- function(data1, data2) {
  # Sentence Evaluation
  stats <- list(hc1 = list(empty_sentences = list(matched = 0, unmatched = 0),
                           trait_sentences = list(matched = 0, unmatched = 0),
                           value_sentences = list(matched = 0, unmatched = 0),
                           trait_value_sentences = list(matched = 0, unmatched = 0),
                           characters = list(matched = 0, unmatched = 0),
                           traits = list(matched = 0, unmatched = 0),
                           values = list(matched = 0, unmatched = 0)),
                hc2 = list(empty_sentences = list(matched = 0, unmatched = 0),
                           trait_sentences = list(matched = 0, unmatched = 0),
                           value_sentences = list(matched = 0, unmatched = 0),
                           trait_value_sentences = list(matched = 0, unmatched = 0),
                           characters = list(matched = 0, unmatched = 0),
                           traits = list(matched = 0, unmatched = 0),
                           values = list(matched = 0, unmatched = 0)))
  
  hc1_traits <- 0
  hc1_values <- 0
  
  for (i in 1:nrow(data1)) {
    code_line1 <- data1[i,1]
    code_line2 <- data2[i,1]
    a <- trimws(code_line1) == ""
    b <- trimws(code_line2) == ""
    
    # Empty sentence check
    if (a && b) {
      stats$hc1$empty_sentences$matched <- stats$hc1$empty_sentences$matched + 1
      stats$hc2$empty_sentences$matched <- stats$hc2$empty_sentences$matched + 1
      next
    } else if (a) {
      stats$hc1$empty_sentences$unmatched <- stats$hc1$empty_sentences$unmatched + 1
    } else if (b) {
      stats$hc2$empty_sentences$unmatched <- stats$hc2$empty_sentences$unmatched + 1
    }
    
    # Split sentences by code
    split1 <- stringi::stri_split_fixed(code_line1, ",")
    split2 <- stringi::stri_split_fixed(code_line2, ",")
    
    if ((a && !b) || (!a && b)) {
      if (a && !b) {
        split <- split2
        coder <- "hc2"
      } else {
        split <- split1
        coder <- "hc1"
      }
      
      has_trait <- FALSE
      has_value <- FALSE
      for (code in split[[1]]) {
        char_split <- stringi::stri_split_fixed(code, ":")
        if (trimws(char_split) == "") { next }
        char <- trimws(toupper(char_split[[1]][1]))
        trait <- trimws(toupper(char_split[[1]][2]))
        value <- trimws(toupper(char_split[[1]][3]))
        stats[[coder]]$characters$unmatched <- stats[[coder]]$characters$unmatched + 1
        if (toupper(trait) != "X") {
          stats[[coder]]$traits$unmatched <- stats[[coder]]$traits$unmatched + 1
          has_trait <- TRUE
        }
        if (toupper(value) != "X") {
          stats[[coder]]$traits$unmatched <- stats[[coder]]$traits$unmatched + 1
          has_value <- TRUE
        }
      }
      if (has_trait && has_value) {
        stats[[coder]]$trait_value_sentences$unmatched <- stats[[coder]]$trait_value_sentences$unmatched + 1
      } else if (has_trait) {
        stats[[coder]]$trait_sentences$unmatched <- stats[[coder]]$trait_sentences$unmatched + 1
      } else if (has_value) {
        stats[[coder]]$value_sentences$unmatched <- stats[[coder]]$value_sentences$unmatched + 1
      }
    
    } else {
      # !a && !b
      # Both have codes
      # Parse codes
      codes1 <- list()
      for (code in split1[[1]]) {
        char_split <- stringi::stri_split_fixed(code, ":")
        if (trimws(char_split) == "") { next }
        codes1[[length(codes1)+1]] <- list(char = trimws(toupper(char_split[[1]][1])),
                                           trait = trimws(toupper(char_split[[1]][2])),
                                           value1 = trimws(toupper(char_split[[1]][3])))
      }
      
      codes2 <- list()
      for (code in split2[[1]]) {
        char_split <- stringi::stri_split_fixed(code, ":")
        if (trimws(char_split) == "") { next }
        codes2[[length(codes2)+1]] <- list(char = trimws(toupper(char_split[[1]][1])),
                                           trait = trimws(toupper(char_split[[1]][2])),
                                           value1 = trimws(toupper(char_split[[1]][3])))
      }
     
      
      # Compare codes
      has_trait1 <- FALSE
      has_value1 <- FALSE
      for (parsed_code1 in codes1) {
        match <- 0
        has_trait <- FALSE
        has_value <- FALSE
        for (parsed_code2 in codes2) {
          if (match != 4)  {
            # Check character match
            char_match <- FALSE
            trait_match <- FALSE
            value_match <- FALSE
            if (parsed_code1$char == parsed_code2$char) {
              char_match <- TRUE
            } else {
              next
            }
            
            if (parsed_code1$trait != "X") { has_trait <- TRUE }
            if (parsed_code1$trait != "X" && parsed_code2$trait != "X") {
              has_trait <- TRUE
              trait_code1 <- stringi::stri_sub(parsed_code1$trait, 1, 1)
              trait_code2 <- stringi::stri_sub(parsed_code2$trait, 1, 1)
              trait_mod1 <- stringi::stri_sub(parsed_code1$trait, 2)
              trait_mod2 <- stringi::stri_sub(parsed_code2$trait, 2)
              if (trait_code1 == trait_code2 && ((trait_mod1 == "-" && trait_mod2 == "-") ||
                                                 ((trait_mod1 == "+" || trait_mod1 == "") && (trait_mod2 == "+" || trait_mod2 == "")))) {
                trait_match <- TRUE
              }
            }
            
            if (parsed_code1$value != "X") { has_value <- TRUE }
            if (parsed_code1$value != "X" && parsed_code2$value != "X") {
              has_value <- TRUE
              value_code1 <- stringi::stri_sub(parsed_code1$value, 1, 2)
              value_code2 <- stringi::stri_sub(parsed_code2$value, 1, 2)
              value_mod1 <- stringi::stri_sub(parsed_code1$value, 3)
              value_mod2 <- stringi::stri_sub(parsed_code2$value, 3)
              if (value_code1 == value_code2 && ((value_mod1 == "-" && value_mod2 == "-") ||
                                                 ((value_mod1 == "+" || value_mod1 == "") && (value_mod2 == "+" || value_mod2 == "")))) {
                value_match <- TRUE
              }
            }
            
            if (value_match && trait_match) {
              match = 4
            } else if (value_match) {
              match = 3
            } else if (trait_match) {
              match = 2
            } else {
              match = 1
            }
          }
        }
        
        if (match == 0) {
          stats$hc1$characters$unmatched <- stats$hc1$characters$unmatched + 1
          if (has_trait) {
            stats$hc1$traits$unmatched <- stats$hc1$traits$unmatched + 1
          }
          if (has_value) {
            stats$hc1$values$unmatched <- stats$hc1$values$unmatched + 1
          }
        } else {
          stats$hc1$characters$matched <- stats$hc1$characters$matched + 1
        }
        
        if (match == 2 || match == 4) {
          stats$hc1$traits$matched <- stats$hc1$traits$matched + 1
        }
        if (match == 3 || match == 4) {
          stats$hc1$values$matched <- stats$hc1$values$matched + 1
        }
        if (!has_trait1 && has_trait) {
          has_trait1 <- TRUE
        }
        if (!has_value1 && has_value) {
          has_value1 <- TRUE
        }
      }
     
      
      # TOOD fix copy pasta
      has_trait2 <- FALSE
      has_value2 <- FALSE
      for (parsed_code1 in codes2) {
        match <- 0
        has_trait <- FALSE
        has_value <- FALSE
        for (parsed_code2 in codes1) {
          if (match != 4)  {
            # Check character match
            char_match <- FALSE
            trait_match <- FALSE
            value_match <- FALSE
            if (parsed_code1$char == parsed_code2$char) {
              char_match <- TRUE
            } else {
              next
            }
            
            if (parsed_code1$trait != "X") { has_trait <- TRUE }
            if (parsed_code1$trait != "X" && parsed_code2$trait != "X") {
              has_trait <- TRUE
              trait_code1 <- stringi::stri_sub(parsed_code1$trait, 1, 1)
              trait_code2 <- stringi::stri_sub(parsed_code2$trait, 1, 1)
              trait_mod1 <- stringi::stri_sub(parsed_code1$trait, 2)
              trait_mod2 <- stringi::stri_sub(parsed_code2$trait, 2)
              if (trait_code1 == trait_code2 && ((trait_mod1 == "-" && trait_mod2 == "-") ||
                                                 ((trait_mod1 == "+" || trait_mod1 == "") && (trait_mod2 == "+" || trait_mod2 == "")))) {
                trait_match <- TRUE
              }
            }
            
            if (parsed_code1$value != "X") { has_value <- TRUE }
            if (parsed_code1$value != "X" && parsed_code2$value != "X") {
              has_value <- TRUE
              value_code1 <- stringi::stri_sub(parsed_code1$value, 1, 2)
              value_code2 <- stringi::stri_sub(parsed_code2$value, 1, 2)
              value_mod1 <- stringi::stri_sub(parsed_code1$value, 3)
              value_mod2 <- stringi::stri_sub(parsed_code2$value, 3)
              if (value_code1 == value_code2 && ((value_mod1 == "-" && value_mod2 == "-") ||
                                                 ((value_mod1 == "+" || value_mod1 == "") && (value_mod2 == "+" || value_mod2 == "")))) {
                value_match <- TRUE
              }
            }
            
            if (value_match && trait_match) {
              match = 4
            } else if (value_match) {
              match = 3
            } else if (trait_match) {
              match = 2
            } else {
              match = 1
            }
          }
        }
        
        if (match == 0) {
          stats$hc2$characters$unmatched <- stats$hc2$characters$unmatched + 1
          if (has_trait) {
            stats$hc2$traits$unmatched <- stats$hc2$traits$unmatched + 1
          }
          if (has_value) {
            stats$hc2$values$unmatched <- stats$hc2$values$unmatched + 1
          }
        } else {
          stats$hc2$characters$matched <- stats$hc2$characters$matched + 1
        }
        
        if (match == 2 || match == 4) {
          stats$hc2$traits$matched <- stats$hc2$traits$matched + 1
        }
        if (match == 3 || match == 4) {
          stats$hc2$values$matched <- stats$hc2$values$matched + 1
        }
        if (!has_trait2 && has_trait) {
          has_trait2 <- TRUE
        }
        if (!has_value2 && has_value) {
          has_value2 <- TRUE
        }
      }
      
      if (has_trait1 && has_trait2 && has_value1 && has_trait2) {
        #tv1 and tv2
        stats$hc1$trait_value_sentences$matched <- stats$hc1$trait_value_sentences$matched + 1
        stats$hc2$trait_value_sentences$matched <- stats$hc2$trait_value_sentences$matched + 1
      } else if (has_trait1 && has_value1) {
        #tv1 and t2 or v2
        stats$hc1$trait_value_sentences$unmatched <- stats$hc1$trait_value_sentences$unmatched + 1
        if (has_trait2) {
          stats$hc2$trait_sentences$unmatched <- stats$hc2$trait_sentences$unmatched + 1
        } else if (has_value2) {
          stats$hc2$value_sentences$unmatched <- stats$hc2$value_sentences$unmatched + 1
        }
      } else if (has_trait2 && has_value2) {
        #tv2 and t1 or v1
        stats$hc2$trait_value_sentences$unmatched <- stats$hc2$trait_value_sentences$unmatched + 1
        if (has_trait1) {
          stats$hc1$trait_sentences$unmatched <- stats$hc1$trait_sentences$unmatched + 1
        } else if (has_value1) {
          stats$hc1$value_sentences$unmatched <- stats$hc1$value_sentences$unmatched + 1
        }
      } else if (has_trait1) {
        #t1 and t2 or v2
        if (has_trait2) {
          stats$hc1$trait_sentences$matched <- stats$hc1$trait_sentences$matched + 1
          stats$hc2$trait_sentences$matched <- stats$hc2$trait_sentences$matched + 1
        } else {
          stats$hc1$trait_sentences$unmatched <- stats$hc1$trait_sentences$unmatched + 1
          stats$hc2$value_sentences$unmatched <- stats$hc2$value_sentences$unmatched + 1
        }
      } else if (has_value1) {
        if (has_value2) {
          stats$hc1$value_sentences$matched <- stats$hc1$value_sentences$matched + 1
          stats$hc2$value_sentences$matched <- stats$hc2$value_sentences$matched + 1
        } else {
          stats$hc2$trait_sentences$unmatched <- stats$hc2$trait_sentences$unmatched + 1
          stats$hc1$value_sentences$unmatched <- stats$hc1$value_sentences$unmatched + 1
        }
      }
    }
  }
  return (stats)
}

###### Code ######################

#Acc, Rec, and F1 test
#print((PRF(c(1,1,4,3,5),c(1,2,3,4,5))))

load_data <- FALSE
parse_data <- FALSE
draw_plots <- FALSE
save_plots <- FALSE
calc_stats <- TRUE
image_path <- "../../Images/HumanCoding/"

# Load Character Codes
codes <- read.csv("../../Some Data/CharCodes.csv", header = FALSE)
traits <- c("N", "E", "A", "C", "O")
values <- c("SE", "ST", "OC", "CO")

if (load_data) {
  # Load human coding files
  hc1 <- read.csv("../../Some Data/S_N_S_Kelsey.csv", blank.lines.skip = FALSE)
  hc2 <- read.csv("../../Some Data/S_N_S_Issey.csv", blank.lines.skip = FALSE)
  hc3 <- read.csv("../../Some Data/S_N_S_Anushka.csv", blank.lines.skip = FALSE)
}

if (parse_data) {
  res1 <- get_codes(hc1, 1)
  res2 <- get_codes(hc2, 2)
  res3 <- get_codes(hc3, 3)
}

if (draw_plots) {
  draw_trait_plot(res1$codes_by_trait, "HC1", "All Traits - ", "All_Traits_HC1.png")
  draw_trait_plot(res2$codes_by_trait, "HC2", "All Traits - ", "All_Traits_HC2.png")
  draw_value_plot(res1$codes_by_value, "HC1", "All Values - ", "All_Values_HC1.png")
  draw_value_plot(res2$codes_by_value, "HC2", "All Values - ", "All_Values_HC2.png")
  
  # Plots for each character
  for (name_code in codes[,2]) {
    char_name <- codes[codes[,2] == name_code,1]
    
    if (length(res1$codes_by_character[[name_code]]) != 0) {
      draw_trait_plot(res1$codes_by_character[[name_code]]$"trait", " - HC1", paste0("Traits - ", char_name), paste0("Traits_",name_code,"_HC1",".png"))
      draw_value_plot(res1$codes_by_character[[name_code]]$"value", " - HC1", paste0("Values - ", char_name), paste0("Values_",name_code,"_HC1",".png"))
    }
    if (length(res2$codes_by_character[[name_code]]) != 0) {
      draw_trait_plot(res2$codes_by_character[[name_code]]$"trait", " - HC2", paste0("Traits - ", char_name), paste0("Traits_",name_code,"_HC2",".png"))
      draw_value_plot(res2$codes_by_character[[name_code]]$"value", " - HC2", paste0("Values - ", char_name), paste0("Values_",name_code,"_HC2",".png"))
    }
  }
}

if (calc_stats) {
  stats <- get_stats(hc1, hc2)
}


