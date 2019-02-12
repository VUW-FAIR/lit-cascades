## WORD FREQUENCY

# install.packages("tidyverse")
#library("tidyverse")
# install.packages("dplyr")

library("dplyr")
library("ggplot2")

#Setting directory
setwd("TIC-VUW/tic-personality-words/outputs save")
DIRECTORY<-"random-general-sentence-all-words-book-centric" 
setwd(DIRECTORY)
getwd()
setwd("..") #if you would like to go back one directory

#all the nodes.csv files in the directory
nodescsv <- list(dir(pattern = "*nodes.csv")) 
nodescsv

##FOR DIRECTORY: ...-book-centric. Code is below.##
##Graph shows frequency of more than 5.--------------------------------------------------##

#Function that will create the graph (ascending order) with corresponding file
WordFrequency <-function(FileToRead){
  LowerBound<-5
  BookData <-read.csv(file=FileToRead, header=T, sep="") #Read CSV into R as a dataframe
  words <- as.vector(BookData[,2]) #vector type
  words <- words[words != ""] #no null values - only words
  allwords<-unlist(strsplit(words,split=", ",fixed = FALSE)) #splits the two word values
  FrequencyAllWords<- as.data.frame(table(allwords)) #table of count of word values under 'tags'
  
  BookTitle<-gsub("\\_", " ",sub(pattern = "_nodes(.*)\\..*$", replacement = "\\1", basename(FileToRead)))
  #Frequencies greater than n
  GreaterThan<-filter(FrequencyAllWords,Freq >LowerBound)
  g<-ggplot(data=GreaterThan, aes(x=reorder(allwords,-Freq), y=Freq,fill=Freq)) +
    geom_bar(stat="identity",width = 0.75) + coord_flip() +  theme(legend.position="none") +
    labs(title=paste("Word Frequencies (greater than five) in",BookTitle), x="Words", y = "Frequency") 
  return(g)
}
 
#loops through the corresponding directory and creates graphs from those files
#and saves it into the POSTPROC folder
for (k in 1:length(nodescsv[[1]])){
   file<- nodescsv[[1]][k]
   graph<-WordFrequency(file)
  
  #change the filename to the folder you want to save the graph into
  ggsave(filename=paste0(paste0("../",DIRECTORY,"/","PostProc","/"),
          file,k,"__FMoreThanFiveGraph",".pdf"),device = "pdf", plot = graph, 
         scale = 1, width = 25, height = 20, units = "cm",
         dpi = 300, limitsize = TRUE)
}
 



#For testing purposes:
FileToRead<-"A_Tale_of_Two_Cities_nodes.csv"
WordFrequency(FileToRead)
#
