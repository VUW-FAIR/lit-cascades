## WORDS THAT NEVER OCCUR List ------------------------------------------------------


#Setting directory
begPath<-"/Users/GraceC/projects/TIC-VUW" #<- example
setwd(paste0("/tic-personality-words/outputs save"))
setwd("outputs save")
DIRECTORY<-"pda500-1000words-advs-lemma-book-centric" #**
setwd(DIRECTORY)
getwd()
setwd("..")
setwd("TIC-VUW")
setwd(begPath)


#LIST OF WORDS THAT DON'T OCCUR

#all the nodes.csv files in the directory
nodescsv <- list(dir(pattern = "*nodes.csv"))

#Note: TXTFILE needs to be in the same directory as DIRECTORY
TXTFILE<-"pda500.txt" #**

#Prep wordlist(TXTFILE):
wordlist<-read.csv(file=TXTFILE, header=F, sep="\t")
wordlist<-tolower(as.vector(wordlist[,1]))
wordlist<-unique(wordlist)
wordlist<-sort(wordlist)
length(wordlist)



#Function that returns a list of the words not present:
FrequencyWordsNotPresent<-function(FileToRead,WordList){
    FileToRead<-nodescsv[[1]][1] #a tale of two cities
    BookData <-read.csv(file=FileToRead, header=T, sep="") #Read CSV into R as a dataframe
    words <- as.vector(BookData[,2]) #vector type
    allwords<-unlist(strsplit(words,split=", ",fixed = FALSE)) #splits the two word values
    allwords<-sort(allwords) #alphabetical order
    uniqueWords<-sort(unique(allwords)) #removes duplicates and shows in alphabetical order
    
    wordsNotPresent<-wordlist[which(!wordlist %in% uniqueWords)]
    
    return(wordsNotPresent)
    
}

#Loop that goes through all books and saves the list as csv:
for (k in 1:length(nodescsv[[1]])){
    file<- nodescsv[[1]][k]
    csvfile<-FrequencyWordsNotPresent(file,wordlist)
    write.csv(csvfile, file = paste0(paste0("../",DIRECTORY,"/","PostProc","/"),file,k,"__WordsNotPresent",".txt"))
}

##LIST and GRID PLOT that shows Words that are and are not present -----------------------------------------------------

#FUNCTIONS:

#A function that returns all unique words from a book (nodes.csv)
getwords<-function(fileToRead){
    list<-read.csv(file=fileToRead, header=T, sep="")
    allwords <- as.vector(list[,2]) #vector type
    allwords <- allwords[allwords != ""] #no null values - only words
    allwords<-unlist(strsplit(allwords,split=", ",fixed = FALSE)) #splits the two word values
    words<-unique(allwords)
    words<-sort(words)
    return(words)
}

#function to calculate frequency for each word
CreateCountColumn<-function(df){ #<--- check - function does not work.
    count<-c()
    for(r in 1:length(df[[1]])){
        row<-as.vector(df[r,3:length(df)])
        count.true<-length(row[which(row==1)]) ##<-- YAAAS
        count[r]<-count.true
    }
    return(count)
    # #put values in the column df$count.present
    # df$count.present<-c(count)
}

WordsPresentBinary<-function(WORDLIST){
    df<-as.data.frame(c(WORDLIST))
    count.present<-c(1:length(WORDLIST))
    df<-cbind(df,count.present)
    colnames(df)<-c("Words","count.present")
    
    #for BINARY (present/not present) result
    #loop for plot and show in the created dataframe 'df'
    for(b in 1:length(nodescsv[[1]])){#go through each book #length = 25
        file<- nodescsv[[1]][b]
        nodes<-getwords(file)
        #colnames(df)[b+1]<-books[[1]][b] #column will show book titles
        #colnames(df)[b+1]<-paste0("B",b) #+1
        
        for(count in 1:length(WORDLIST)){ #go through each word in wordlist and check if in the book
            word<-WORDLIST[count]
            #value is either 1 (present) or 0 (not present)
            if(is.element(word,nodes)){ value<-1 }
            else{ value<-0 }
            col<-b+2
            df[count,col]<-value
        }
    }
    #books - the titles
    books<-list(1:25)
    
    #put titles as the colnames
    for(i in 1:length(nodescsv[[1]])){
        t<-nodescsv[[1]][i]
        tedit<-gsub("_"," ",sub(pattern= "_nodes.*", replacement = "\\",basename(t)))
        books[[1]][i]<-tedit
        colnames(df)[i+2]<-tedit
    }
    
    #original alphabetical ordered wordlist
    df$count.present<-CreateCountColumn(df)
    
    return(df)
}

#get the list of all unique words from book
WordFrequencyList <-function(FileToRead){
    LowerBound<-1
    #LowerBound<-5
    BookData <-read.csv(file=FileToRead, header=T, sep="") #Read CSV into R as a dataframe
    words <- as.vector(BookData[,2]) #vector type
    words <- words[words != ""] #no null values - only words
    allwords<-unlist(strsplit(words,split=", ",fixed = FALSE)) #splits the two word values
    FrequencyAllWords<- as.data.frame(table(allwords)) #table of count of word values under 'tags'
    return(FrequencyAllWords)
}

#function to calculate frequency for each word in FREQUENCY grid
CreateCountFrequencyColumn<-function(df){ #<--- check - function does not work.
    freqVec<-c()
    for(r in 1:length(df[[1]])){
        row<-as.vector(df[r,3:length(df)])
        count<-0
        for(c in 1:length(row)){
            count<-count+row[c]
        }
        #count.true<-length(row[which(row==1)]) ##<-- YAAAS
        freqVec[r]<-count
    }
    return(freqVec)
    # #put values in the column df$count.present
    # df$count.present<-c(count)
}

WordsPresentFrequency<-function(WORDLIST){
    
    df<-as.data.frame(c(WORDLIST))
    count.present<-c(1:length(WORDLIST))
    df<-cbind(df,count.present)
    colnames(df)<-c("Words","count.present")
    
    ##FREQUENCY
    #need to make the data first
    #loop for plot and show in the created dataframe 'df'
    for(b in 1:length(nodescsv[[1]])){#go through each book #length = 25
        file<- nodescsv[[1]][b]
        nodes<-getwords(file)
        list<-WordFrequencyList(file)
        #colnames(df)[b+1]<-books[[1]][b] #column will show book titles
        #colnames(df)[b+1]<-paste0("B",b) #+1
        
        for(count in 1:length(WORDLIST)){ #go through each word in wordlist and check if in the book
            word<-WORDLIST[count]
            #value is either 1 (present) or 0 (not present)
            if(is.element(word,nodes)){
                f<-which(nodes == word)
                value<-list[f,2]
            }
            else{ value<-0 }
            col<-b+2
            df[count,col]<-value
            
        }
    }
    
    #books - the titles
    books<-list(1:25)
    
    #put titles as the colnames
    for(i in 1:length(nodescsv[[1]])){
        t<-nodescsv[[1]][i]
        tedit<-gsub("_"," ",sub(pattern= "_nodes.*", replacement = "\\",basename(t)))
        books[[1]][i]<-tedit
        colnames(df)[i+2]<-tedit
    }
    
    df$count.present<-CreateCountFrequencyColumn(df)
    
    return(df)
}




#MAIN:

library(dplyr)


#Prepping the data:

#Setting directory
setwd(paste0("tic-personality-words/outputs save"))
setwd("TIC-VUW/tic-personality-words/outputs save")
DIRECTORY<-"pda1710-1000words-advs-lemma-book-centric" #**
setwd(DIRECTORY)
getwd()
setwd("..")
setwd("outputs save")
setwd("TIC-VUW")

#all the nodes.csv files in the directory (the words present in the book)
nodescsv <- list(dir(pattern = "*nodes.csv"))

#PREP WORDLIST (that could be found in each book):
#Note: TXTFILE needs to be in the same directory as DIRECTORY
TXTFILE<-"pda1710_no_abbreviation.csv" #**
#Set word list to compare with:
wordlist<-read.csv(file=TXTFILE, header=F, sep="\t")
wordlist<-tolower(as.vector(wordlist[,1]))
wordlist<-unique(wordlist)
wordlist<-sort(wordlist)

length(wordlist)

# ## For pda1710:
# #prep wordlist
# wordlist <- wordlist[-(372)] ## need to remove "extraction method: principal component analysis." from wordlist
# length(wordlist)

WORDLIST<- wordlist#**

#BINARY

df<-WordsPresentBinary(WORDLIST)
View(df)
#frequency ORDERED wordlist
ordered<-dplyr::arrange(df,desc(count.present))
View(ordered)

#Export Frequency list (df) as excel
setwd("PostProc")
write.csv(df,"WordsPresent_binary_list_allBooks.csv")
write.csv(ordered,"WordsPresent_binaryOrdered_list_allBooks.csv")

write.csv(df,"WordsPresent_binary_list_allBooks.txt")
write.csv(ordered,"WordsPresent_binaryOrdered_list_allBooks.txt")

setwd("..")

#austen books
austen<-c(WORDLIST)
count.present<-c(1:length(WORDLIST))
austen<-cbind(austen,count.present)
colnames(austen)<-c("Words","count.present")
b<-select(df,c(9,15,19,22,24,25))
austen<-cbind(austen,b)
austen$count.present<-CreateCountColumn(austen)

setwd("PostProc")

write.csv(austen,"WordsPresent_binary_list_austen.csv")
write.csv(austen,"WordsPresent_binary_list_austen.txt")

ordered.austen<-dplyr::arrange(austen,desc(count.present))
write.csv(ordered.austen,"WordsPresent_binaryOrdered_list_austen.csv")
write.csv(ordered.austen,"WordsPresent_binaryOrdered_list_austen.txt")

setwd("..")

#dickens books
dickens<-c(WORDLIST)
count.present<-c(1:length(WORDLIST))
dickens<-cbind(dickens,count.present)
colnames(dickens)<-c("Words","count.present")
b<-select(df,c(3,4,5,7,8,12,13,14,16,17,18,20,21,23,26))
dickens<-cbind(dickens,b)
dickens$count.present<-CreateCountColumn(dickens)

setwd("PostProc")

write.csv(dickens,"WordsPresent_binary_list_dickens.csv")
write.csv(dickens,"WordsPresent_binary_list_dickens.txt")

ordered.dickens<-dplyr::arrange(dickens,desc(count.present))
write.csv(ordered.dickens,"WordsPresent_binaryOrdered_list_dickens.csv")
write.csv(ordered.dickens,"WordsPresent_binaryOrdered_list_dickens.txt")

setwd("..")

#austen_books <- c(7,13,17,20,22,23)
#dickens_books <- c(1,2,3,5,6,10,11,12,14,15,16,18,19,21,24)


#FREQUENCY with gradient

df<-WordsPresentFrequency(WORDLIST)
View(df)

df$count.present<-as.numeric(df$count.present)
ordered<-dplyr::arrange(df,desc(count.present))

View(ordered)

#Export Frequency list (df) as excel
setwd("PostProc")
write.csv(df,"WordsPresent_frequency_list_allBooks.csv")
write.csv(df,"WordsPresent_frequency_list_allBooks.txt")
write.csv(ordered,"WordsPresent_frequencyOrdered_list_allBooks.csv")
write.csv(ordered,"WordsPresent_frequencyOrdered_list_allBooks.txt")
setwd("..")

#austen books
austen<-c(WORDLIST)
count.present<-c(1:length(WORDLIST))
austen<-cbind(austen,count.present)
colnames(austen)<-c("Words","count.present")
b<-select(df,c(9,15,19,22,24,25))
austen<-cbind(austen,b)
austen$count.present<-CreateCountFrequencyColumn(austen)


setwd("PostProc")

austen$count.present<-as.numeric(austen$count.present)
write.csv(austen,"WordsPresent_frequency_list_austen.csv")
write.csv(austen,"WordsPresent_frequency_list_austen.txt")


ordered.austen<-dplyr::arrange(austen,desc(count.present)) 

write.csv(ordered.austen,"WordsPresent_frequencyOrdered_list_austen.csv")
write.csv(ordered.austen,"WordsPresent_frequencyOrdered_list_austen.txt")

setwd("..")

#dickens books
dickens<-c(WORDLIST)
count.present<-c(1:length(WORDLIST))
dickens<-cbind(dickens,count.present)
colnames(dickens)<-c("Words","count.present")
b<-select(df,c(3,4,5,7,8,12,13,14,16,17,18,20,21,23,26))
dickens<-cbind(dickens,b)
dickens$count.present<-CreateCountFrequencyColumn(dickens)

setwd("PostProc")

dickens$count.present<-as.numeric(dickens$count.present)
write.csv(dickens,"WordsPresent_frequency_list_dickens.csv")
write.csv(dickens,"WordsPresent_frequency_list_dickens.txt")

ordered.dickens<-dplyr::arrange(dickens,desc(count.present))
write.csv(ordered.dickens,"WordsPresent_frequencyOrdered_list_dickens.csv")
write.csv(ordered.dickens,"WordsPresent_frequencyOrdered_list_dickens.txt")

setwd("..")

#austen_books <- c(7,13,17,20,22,23)
#dickens_books <- c(1,2,3,5,6,10,11,12,14,15,16,18,19,21,24)




#####------------------------------------------------------

#Creating Gridplot:

#Creating gridplot for dataframe 'df'
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
install.packages("gridExtra")
library(gridExtra)

MakeGridplotNumerically<-function(df){
    df.o<-c()
    df.o<-dplyr::arrange(df,desc(count.present))
    bookOrder<- as.vector(df.o$Words)
    df.o$Words<-as.character(df.o$Words)
    df.o$Words<-factor(df.o$Words,levels = bookOrder)
    df.o<-df.o[,-2]
    df.o<- melt(df.o, id.vars = "Words")
    colnames(df.o)[2]<- "Books"
    return(df.o)
}

MakeGridplotAlphabetically<-function(df){
    df.o<-df
    df.o<-df.o[,-2]
    df.o<- melt(df.o, id.vars = "Words")
    colnames(df.o)[2]<- "Books"
    return(df.o)
}

#In Binary and Frequency:

#allbooks
#binary
#binaryOrdered
#frequency
#frequencyOrdered

#df
#ordered
#austen
#ordered.austen
#dickens
#ordered.dickens

#change TITLE for the  title of your plot
TITLE<-"WordsPresent_binary_gridplot_allbooks"

#find highest frequency from ordered dataframe
#ordered
#ordered.austen
#ordered.dickens

#**
#maximum<- max(ordered.dickens[1,3:length(ordered.dickens)]) #frequency **change to the correct 'ORDERED' file
maximum<-1 #binary

#change PLOT to the dataframe you want to plot#**
df.plot<-df

#comment out and use one for alphabetical or numerical ordering#**

df.plot<-MakeGridplotAlphabetically(df.plot) #**
#df.plot<-MakeGridplotNumerically(df.plot) #**

#plot graph
gridplot <- ggplot(df.plot,aes(x=Books,y=Words)) + geom_tile(aes(fill = value),colour = "lightgray") +
  scale_fill_gradient(low = c("white","gray","#b2d8d8","#66b2b2","#008080"), high = c("#006666","#004c4c","black"),limits = c(0,maximum)) +
  coord_flip() +  theme(axis.text.x=element_text(angle=30,hjust=1)) + theme(legend.position="left")

#save graph
ggsave(filename=paste0(paste0("../",DIRECTORY,"/","PostProc","/"),
                       TITLE,".pdf"),device = "pdf", plot = gridplot,
       scale = 1, width = 350, height = 20, units = "cm", dpi = 300, limitsize = FALSE)

#--



#FOR LONG WORDLISTS (Allport,1710) break list and create gridplot with code below:

##Plot gridplot HERE: <-----****

#In Binary and Frequency:
#df
#ordered
#austen
#ordered.austen
#dickens
#ordered.dickens


#change TITLE for the  title of your plot
TITLE<-"WordsPresent_frequencyOrdered_gridplot_dickens"

#change PLOT to the dataframe you want to plot
PLOT<-ordered.dickens

#find highest frequency from ordered dataframe
maximum<- max(ordered.dickens[1,3:length(ordered.dickens)]) #frequency **change to the correct 'ORDERED' file
#maximum<-1 #binary

#create plot - make changes** within for loop for the correct plot you want to make
stop<-as.integer(ceiling((length(wordlist))/500))
w<-1
for(s in 1:stop){
    df.plot<-PLOT[w:(w+499),]
    
    #comment out and use one for alphabetical or numerical ordering**
    
    #df.plot<-MakeGridplotAlphabetically(df.plot) #**
    df.plot<-MakeGridplotNumerically(df.plot) #**

    #plot graph
    gridplot <- ggplot(df.plot,aes(x=Books,y=Words)) + geom_tile(aes(fill = value),colour = "lightgray") +
    scale_fill_gradient(low = c("white","gray","#b2d8d8","#66b2b2","#008080"), high = c("#006666","#004c4c","black"),limits = c(0,maximum)) +
    coord_flip() +  theme(axis.text.x=element_text(angle=30,hjust=1)) + theme(legend.position="left")
   
    #save graph
    ggsave(filename=paste0(paste0("../",DIRECTORY,"/","PostProc","/"),
    TITLE,"_",s,".pdf"),device = "pdf", plot = gridplot,
    scale = 1, width = 350, height = 20, units = "cm", dpi = 300, limitsize = FALSE)
    w<-w+499
}

#--