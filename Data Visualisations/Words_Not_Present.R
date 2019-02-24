## WORDS THAT NEVER OCCUR List ------------------------------------------------------


#Setting directory
begPath<-"/Users/GraceC/TIC-VUW" #<- example
setwd(paste0("tic-personality-words/outputs save"))
setwd("outputs save")
DIRECTORY<-"pda1710-sentence-advs-lemma-book-centric" #**
setwd(DIRECTORY)
getwd()
setwd("..") #if you would like to go back one directory
setwd("PostProc")
setwd(begPath)


#LIST OF WORDS THAT DON'T OCCUR

#all the nodes.csv files in the directory
nodescsv <- list(dir(pattern = "*nodes.csv"))

#Note: TXTFILE needs to be in the same directory as DIRECTORY
TXTFILE<-"pda1710.txt" #**

# #Prep wordlist:
# wordlist<-read.csv(file=TXTFILE, header=T, sep="\t")
# wordlist<-tolower(as.vector(wordlist[,1])) #turns the list into vector and removes capital letters
# wordlist<-unique(wordlist) #remove duplicates

#Set word list to compare with:
wordlist<-read.csv(file=TXTFILE, header=T, sep="\t")
wordlist<-tolower(as.vector(wordlist[,1]))
wordlist<-unique(wordlist)
length(wordlist) #499 for pda500

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

#Loop that goes through all files:
for (k in 1:length(nodescsv[[1]])){
    file<- nodescsv[[1]][k]
    csvfile<-FrequencyWordsNotPresent(file,wordlist)
    write.csv(csvfile, file = paste0(paste0("../",DIRECTORY,"/","PostProc","/"),file,k,"__WordsNotPresent",".txt"))
}

##GRID PLOT that shows Words that are and are not present -----------------------------------------------------
#prep files and directories

#Setting directory
begPath<-"/Users/GraceC/TIC-VUW" #<- example
setwd(paste0("tic-personality-words/outputs save"))
setwd("TIC-VUW/tic-personality-words/outputs save")
DIRECTORY<-"allport-personal-traits-sentence-advs-lemma-book-centric" #**
setwd(DIRECTORY)
getwd()
setwd("..") #if you would like to go back one directory
setwd("outputs save")
setwd("TIC-VUW")

#all the nodes.csv files in the directory (the words present in the book)
nodescsv <- list(dir(pattern = "*nodes.csv"))


#PREP WORDLIST (that could be found in each book):
#Note: TXTFILE needs to be in the same directory as DIRECTORY
TXTFILE<-"Personal Traits.txt" #**
#Set word list to compare with:
wordlist<-read.csv(file=TXTFILE, header=T, sep="\t")
wordlist<-tolower(as.vector(wordlist[,1]))
wordlist<-unique(wordlist)
length(wordlist)


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

#books - the titles
books<-list(1:25)
for(i in 1:length(nodescsv[[1]])){
    t<-nodescsv[[1]][i]
    tedit<-gsub("_"," ",sub(pattern= "_nodes.*", replacement = "\\",basename(t)))
    books[[1]][i]<-tedit
}

#create dataframe
df<-as.data.frame(c(wordlist))


#loop for plot and shows in the created dataframe 'df'
for(b in 1:length(nodescsv[[1]])){#go through each book #length = 25
    file<- nodescsv[[1]][b]
    nodes<-getwords(file)
    #colnames(df)[b+1]<-books[[1]][b] #column will show book titles
    #colnames(df)[b+1]<-paste0("B",b) #+1
    
    for(count in 1:length(wordlist)){ #go through each word in wordlist and check if in the book
        word<-wordlist[count]
        #value is either 1 (present) or 0 (not present)
        if(is.element(word,nodes)){ value<-1 }
        else{ value<-0 }
        col<-b+1
        df[count,col]<-value
    }
}

colnames(df)<- c("Words","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13",
"B14","B15","B16","B17","B18","B19","B20","B21","B22","B23","B24","B25")

library(ggplot2)
library(reshape2)
# library(scales)
library(plyr)
library(dplyr)

#prep the dataframe for gridplotting
df.plot<-df
df.plot<- melt(df.plot, id.vars = "Words")
colnames(df.plot)[2]<- "Books"

gridplot <- ggplot(df.plot,aes(x=Books,y=Words,fill=value)) + geom_tile() +
scale_fill_gradient(low = "white", high = "steelblue") +
coord_flip() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

gridplot

ggsave(filename=paste0(paste0("../",DIRECTORY,"/","PostProc","/"),
"WordsPresent_gridplot.pdf"),device = "pdf", plot = gridplot,
scale = 1, width = 100, height = 50, units = "cm", dpi = 300, limitsize = TRUE)



