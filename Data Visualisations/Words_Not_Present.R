## WORDS THAT NEVER OCCUR


#Setting directory
begPath<-"/Users/GraceC/TIC-VUW" #<- example
setwd(paste0("tic-personality-words/outputs save"))
setwd("TIC-VUW/tic-personality-words/outputs save")
DIRECTORY<-"pda1710-sentence-advs-lemma-book-centric" #**
setwd(DIRECTORY)
getwd()
setwd("..") #if you would like to go back one directory
setwd("PostProc")


#all the nodes.csv files in the directory
nodescsv <- list(dir(pattern = "*nodes.csv"))

#Note: TXTFILE needs to be in the same directory as DIRECTORY
TXTFILE<-"pda1710.txt" #**

#Set word list to compare with:
wordlist<-read.csv(file=TXTFILE, header=T, sep="\t")
wordlist<-tolower(as.vector(wordlist[,1]))
wordlist<-unique(wordlist)

#Function that returns a list of the words not present:
FrequencyWordsNotPresent<-function(FileToRead,WordList){
    
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


######-----------------------------------------------

