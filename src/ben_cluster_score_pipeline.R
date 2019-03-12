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
  rownames(frame1) <- c("N", "E", "A", "C", "O")
  
  # Plot
  data <- t(frame1[-1])
  barplot(data, main=paste0(title, coder), col = c("orange", "grey", "darkblue"), xlab = "Trait Code",
          legend = rownames(data), ylim = range(pretty(c(0, max(2,max(frame1[1]))))))
  savePlot(fname)
}

draw_value_plot <- function(codes, coder, title, fname) {
  frame1 <- as.data.frame(codes$SE)
  frame1[2,] <- as.data.frame(codes$ST)
  frame1[3,] <- as.data.frame(codes$OC)
  frame1[4,] <- as.data.frame(codes$CO)
  rownames(frame1) <- c("SE", "ST", "OC", "CO")
  
  # Plot
  data <- t(frame1[-1])
  barplot(data, main=paste0(title, coder), col = c("orange", "grey", "darkblue"), xlab = "Trait Code",
          legend = rownames(data), ylim = range(pretty(c(0, max(2,max(frame1[1]))))))
  savePlot(fname)
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
      }
      
      # Handle trait code
      # print(paste0((i+1), " ", trait))
      if (trait != "X") {
        trait_code <- stringi::stri_sub(trait, 1, 1)
        
        if (!trait_code %in% names(codes_by_character[[char]])) {
          codes_by_character[[char]]$N <- list(total = 0, positive = 0, neutral = 0, negative = 0)
          codes_by_character[[char]]$E <- list(total = 0, positive = 0, neutral = 0, negative = 0)
          codes_by_character[[char]]$A <- list(total = 0, positive = 0, neutral = 0, negative = 0)
          codes_by_character[[char]]$C <- list(total = 0, positive = 0, neutral = 0, negative = 0)
          codes_by_character[[char]]$O <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        }
        if (!trait_code %in% names(codes_by_trait)) {
          codes_by_trait[[trait_code]] <- list(total = 0, positive = 0, neutral = 0, negative = 0)
        }
        
        if (length(which(traits == trait_code)) == 0) {
          print(paste0(id, "|", (i+1), " unknown trait: ", trait_code))
        }
        
        if (nchar(trait) == 2) {
          trait_mod <- stringi::stri_sub(trait, 2)
          if (trait_mod == "+") {
            codes_by_character[[char]][[trait_code]]$positive <- codes_by_character[[char]][[trait_code]]$positive + 1
            codes_by_trait[[trait_code]]$positive <- codes_by_trait[[trait_code]]$positive + 1
          } else if (trait_mod == "-") {
            codes_by_character[[char]][[trait_code]]$negative <- codes_by_character[[char]][[trait_code]]$negative + 1
            codes_by_trait[[trait_code]]$negative <- codes_by_trait[[trait_code]]$negative + 1
          } else {
            print(paste0((i+1), " Unrecognised trait modifier: ", trait_mod))
          }
        } else if (nchar(trait) == 1) {
          codes_by_character[[char]][[trait_code]]$neutral <- codes_by_character[[char]][[trait_code]]$neutral + 1
          codes_by_trait[[trait_code]]$neutral <- codes_by_trait[[trait_code]]$neutral + 1
        } else {
          print(paste0((i+1), " error in trait: ", trait))
        }
        codes_by_character[[char]][[trait_code]]$total <- codes_by_character[[char]][[trait_code]]$total + 1
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
            codes_by_value[[value_code]]$positive <- codes_by_value[[value_code]]$positive + 1
          } else if (value_mod == "-") {
            codes_by_value[[value_code]]$negative <- codes_by_value[[value_code]]$negative + 1
          } else {
            print(paste0((i+1), " Unrecognised value modifier: ", value_mod))
          }
        } else if (nchar(value) == 2) {
          codes_by_value[[value_code]]$neutral <- codes_by_value[[value_code]]$neutral + 1
        } else {
          print(paste0((i+1), " error in value: ", value))
        }
        codes_by_value[[value_code]]$total <- codes_by_value[[value_code]]$total + 1
      }
    }
  }
  return (list(codes_by_character = codes_by_character, codes_by_trait = codes_by_trait, codes_by_value = codes_by_value))
}


#print((PRF(c(1,1,4,3,5),c(1,2,3,4,5))))

load_data <- FALSE
parse_data <- FALSE
save_plots <- TRUE
image_path <- "../../Images/HumanCoding/"

# Load Character Codes
codes <- read.csv("../../Some Data/CharCodes.csv", header = FALSE)
traits <- c("N", "E", "A", "C", "O")
values <- c("SE", "ST", "OC", "CO")

if (load_data) {
  # Load human coding files
  hc1 <- read.csv("../../Some Data/S_N_S_Kelsey.csv", blank.lines.skip = FALSE)
  hc2 <- read.csv("../../Some Data/S_N_S_Issey.csv", blank.lines.skip = FALSE)
}

if (parse_data) {
  res1 <- get_codes(hc1, 1)
  res2 <- get_codes(hc2, 2)
}

draw_trait_plot(res1$codes_by_trait, "hc1", "All Traits - ", "HC1_All_Traits.png")
draw_trait_plot(res2$codes_by_trait, "hc2", "All Traits - ", "HC2_All_Traits.png")
draw_value_plot(res1$codes_by_value, "hc1", "All Values - ", "HC1_All_Values.png")
draw_value_plot(res2$codes_by_value, "hc2", "All Values - ", "HC2_All_Values.png")

# Plots for each character
for (name_code in codes[,2]) {
  char_name <- codes[codes[,2] == name_code,1]
  if (length(res1$codes_by_character[[name_code]]) != 0) {
    draw_trait_plot(res1$codes_by_character[[name_code]], " - hc1", paste0("Traits - ", char_name), paste0("HC1_Traits_",name_code,".png"))
  }
  if (length(res2$codes_by_character[[name_code]]) != 0) {
    draw_trait_plot(res2$codes_by_character[[name_code]], " - hc2", paste0("Traits - ", char_name), paste0("HC2_Traits_",name_code,".png"))
  }
}
