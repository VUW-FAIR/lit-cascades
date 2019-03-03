## WORDS THAT NEVER OCCUR List ------------------------------------------------------


#Setting directory
begPath<-"/Users/GraceC/TIC-VUW" #<- example
setwd(paste0("tic-personality-words/outputs save"))
setwd("outputs save")
DIRECTORY<-"pda500-sentence-advs-lemma-book-centric" #**
setwd(DIRECTORY)
getwd()
setwd("..")
setwd("TIC-VUW")
setwd(begPath)


#LIST OF WORDS THAT DON'T OCCUR

#all the nodes.csv files in the directory
nodescsv <- list(dir(pattern = "*nodes.csv"))

#Note: TXTFILE needs to be in the same directory as DIRECTORY
TXTFILE<-"pda1710.txt" #**

#Prep wordlist(TXTFILE):
wordlist<-read.csv(file=TXTFILE, header=T, sep="\t")
wordlist<-tolower(as.vector(wordlist[,1]))
wordlist<-unique(wordlist)
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

#Prepping the data:

#Setting directory
setwd(paste0("tic-personality-words/outputs save"))
setwd("TIC-VUW/tic-personality-words/outputs save")
DIRECTORY<-"allport-personal-traits-sentence-advs-lemma-book-centric" #**
setwd(DIRECTORY)
getwd()
setwd("..")
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

## For pda1710:
#prep wordlist
wordlist <- wordlist[-(372)] ## need to remove "extraction method: principal component analysis." from wordlist
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

WORDLIST<- wordlist #wordlista

#create dataframe
df<-as.data.frame(c(WORDLIST))
count.present<-c(1:length(WORDLIST))
df<-cbind(df,count.present)
colnames(df)<-c("Words","count.present")

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

# colnames(df)<- c("Words","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13",
#                  "B14","B15","B16","B17","B18","B19","B20","B21","B22","B23","B24","B25")

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

#original alphabetical ordered wordlist
df$count.present<-CreateCountColumn(df)

#frequency ordered wordlist
ordered<-dplyr::arrange(df,desc(count.present))
View(ordered)

#Export Frequency list (df) as excel ###------
setwd("PostProc")
write.csv(df,"WordsPresent_original_list_allBooks.csv")
write.csv(ordered,"WordsPresent_frequency_list_allBooks.csv")
setwd("..")


#austen books
austen<-c(WORDLIST)
count.present<-c(1:length(WORDLIST))
austen<-cbind(austen,count.present)
colnames(austen)<-c("Words","count.present")
b<-select(df,c(9,15,19,22,24,25))
austen<-cbind(austen,b)
austen$count.present<-CreateCountColumn(austen)

write.csv(austen,"WordsPresent_original_list_austen.csv")

ordered.austen<-dplyr::arrange(austen,desc(count.present))
write.csv(ordered,"WordsPresent_frequency_list_austen.csv")

setwd("PostProc")
setwd("..")

#dickens books
dickens<-c(WORDLIST)
count.present<-c(1:length(WORDLIST))
dickens<-cbind(dickens,count.present)
colnames(dickens)<-c("Words","count.present")
b<-select(df,c(3,4,5,7,8,12,13,14,16,17,18,20,21,23,26))
dickens<-cbind(dickens,b)
dickens$count.present<-CreateCountColumn(dickens)

write.csv(dickens,"WordsPresent_original_list_dickens.csv")

ordered.dickens<-dplyr::arrange(dickens,desc(count.present))
write.csv(ordered,"WordsPresent_frequency_list_dickens.csv")

getwd()
setwd("PostProc")
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

MakeFrequencyGridplot<-function(df){
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

MakeOriginalGridplot<-function(df){
    df.o<-df
    df.o<-df.o[,-2]
    df.o<- melt(df.o, id.vars = "Words")
    colnames(df.o)[2]<- "Books"
    return(df.o)
}


df.plot<-MakeFrequencyGridplot(austen)

df.plot<-MakeOriginalGridplot(austen)

#gridplot creation:
gridplot <- ggplot(df.plot,aes(x=Books,y=Words)) + geom_tile(aes(fill = value),colour = "gray") +
scale_fill_gradient(low = "white", high = "steelblue") +
coord_flip() +  theme(axis.text.x=element_text(angle=30,hjust=1))

gridplot

#save gridplot
TITLE<-"WordsPresent_frequency_gridplot_austen.pdf"
TITLE<-"WordsPresent_original_gridplot_allbooks.pdf"
ggsave(filename=paste0(paste0("../",DIRECTORY,"/","PostProc","/"),
TITLE),device = "pdf", plot = gridplot,
scale = 1, width = 350, height = 20, units = "cm", dpi = 300, limitsize = FALSE)


library(gridExtra)
library(ggplot2)

##Plot ORIGINAL alphabetical ordered gridplot HERE:

title<-"WordsPresent_ORIGINAL_gridplot_allbooks"
stop<-as.integer(ceiling((length(wordlist))/500))
w<-1
for(s in 1:stop){
    df.plot<-df[w:(w+499),]
    df.plot<-MakeOriginalGridplot(df.plot)
    gridplot <- ggplot(df.plot,aes(x=Books,y=Words)) + geom_tile(aes(fill = value),colour = "gray") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    coord_flip() +  theme(axis.text.x=element_text(angle=30,hjust=1))
    ggsave(filename=paste0(paste0("../",DIRECTORY,"/","PostProc","/"),
    title,"_",s,".pdf"),device = "pdf", plot = gridplot,
    scale = 1, width = 350, height = 20, units = "cm", dpi = 300, limitsize = FALSE)
    w<-w+500
}
setwd("PostProc")
setwd("..")

##Plot FREQUENCY gridplot HERE:

#PLOT<-ordered
#PLOT<-ordered.dickens
PLOT<-ordered.austen

#title<-"WordsPresent_FREQUENCY_gridplot_allbooks"
#title<-"WordsPresent_DICKENS_gridplot_allbooks"
title<-"WordsPresent_AUSTEN_gridplot_allbooks"

stop<-as.integer(ceiling((length(wordlist))/500))
w<-1
for(s in 1:stop){
    df.plot<-PLOT[w:(w+499),]
    df.plot<-MakeFrequencyGridplot(df.plot)
    gridplot <- ggplot(df.plot,aes(x=Books,y=Words)) + geom_tile(aes(fill = value),colour = "gray") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    coord_flip() +  theme(axis.text.x=element_text(angle=30,hjust=1))
    ggsave(filename=paste0(paste0("../",DIRECTORY,"/","PostProc","/"),
    title,"_",s,".pdf"),device = "pdf", plot = gridplot,
    scale = 1, width = 350, height = 20, units = "cm", dpi = 300, limitsize = FALSE)
    w<-w+500
}




#
#
# #------------------------------------------------------------------------------
#
# #PREPPING WORDLISTS for corresponding wordlists:
#
# ## For allport-personal-traits-sentence-advs-lemma-book-centric:
#
# #have to make THREE graphs because there are 4000+ words in the wordlist
# wordlist<-sort(wordlist)
# wordlista<-wordlist[1:1486]
# wordlistb<-wordlist[1487:2972]
# wordlistc<-wordlist[2973:4457]
#
#
#
#
#
# ######-----------------------------------------------


# #Current List of book-centric:

# allport-personal-traits-sentence-advs-lemma-book-centric Done
# pda500-sentence-advs-lemma-book-centric Done
# pda1710-1000words-advs-lemma-book-centric - to do
# pda1710-sentence-advs-lemma-book-centric Done

