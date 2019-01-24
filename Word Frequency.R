## Statistics of Frequent Words 

# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("svglite") ## must also have cairo installed in system
# install.packages("Rcpp")
library("dplyr")
library("tidyverse")
library("ggplot2")

#Setting directory
setwd("TIC-VUW/tic-personality-words/outputs save")
directory<-"allport-personal-traits-sentence-advs-lemma-person-centric" 
setwd(directory)
getwd()

setwd("..") #if you would like to go back one directory

nodescsv <- list(dir(pattern = "*nodes.csv")) #all the nodes.csv files in the directory
nodescsv

#For testing:
FileToRead<-"A_Tale_of_Two_Cities_nodes.csv"
WordFrequency(FileToRead)
#


##FOR DIRECTORY: allport-personal-traits-sentence-advs-lemma-book-centric. Code is below.##
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
# for (k in 1:length(csv[[1]])){
#   file<- csv[[1]][k]
#   graph<-WordFrequency(file)
  
  #change the filename to the folder you want to save the graph into
  ggsave(filename=paste0(paste0("../",directory,"/","PostProc","/"),
                         file,k,"__FMoreThanFiveGraph",".pdf"),device = "pdf", plot = graph, 
         scale = 1, width = 25, height = 20, units = "cm",
         dpi = 300, limitsize = TRUE)
  #}
 

##FOR DIRECTORY: allport-personal-traits-sentence-advs-lemma-person-centric. Code is below.##
##Graph shows frequency of more than 5.----------------------------------------------------##
getwd()  

  #WordFrequency <-function(FileToRead){
    LowerBound<-5
    
    BookData <-read.csv(file=FileToRead, header=T, sep="") #Read CSV into R as a dataframe
    names(BookData)
    BookData[7:30,1:3]
    words <- as.vector(BookData[,2]) #vector type
    words
    words <- words[words != ""] #no null values - only words
    words
    length(words)
    allwords<-unlist(strsplit(words,split=", ",fixed = FALSE)) #splits the two word values
    allwords
    length(allwords)
    
   ## a) Get all the words first (disregard the characters and their connections with the words)
    w<-allwords
    wordsonly<-w #reset
    
    wordsonly<-gsub(".*::","",w) #ommitted characters
  
    
    
    
    
    FrequencyAllWords<- as.data.frame(table(allwords)) #table of count of word values under 'tags'
    
    
    
    
    
    
    BookTitle<-gsub("\\_", " ",sub(pattern = "_nodes(.*)\\..*$", replacement = "\\1", basename(FileToRead)))
    #Frequencies greater than n
    GreaterThan<-filter(FrequencyAllWords,Freq >LowerBound)
    g<-ggplot(data=GreaterThan, aes(x=reorder(allwords,-Freq), y=Freq,fill=Freq)) +
      geom_bar(stat="identity",width = 0.75) + coord_flip() +  theme(legend.position="none") +
      labs(title=paste("Word Frequencies (greater than five) in",BookTitle), x="Words", y = "Frequency") 
    return(g)
  #}

for (k in 1:length(csv[[1]])){
  file<- csv[[1]][k]
  graph<-WordFrequency(file)
  
  #change the filename to the folder you want to save the graph into
  ggsave(filename=paste0(paste0("../allport-personal-traits-sentence-advs-lemma-person-centric/","PostProc","/"),
                         file,k,"__FMoreThanFiveGraph",".pdf"),device = "pdf", plot = graph, 
         scale = 1, width = 25, height = 20, units = "cm",
         dpi = 300, limitsize = TRUE)
}





##---------------------
FileToRead <-"Sense_and_Sensibility_nodes.csv"

BookData <-read.csv(file=FileToRead, header=T, sep="") #Read CSV into R as a dataframe
words <- as.vector(BookData[,2]) #vector type
words <- words[words != ""] #no null values - only words
allwords<-unlist(strsplit(words,split=",",fixed = FALSE)) #splits the two word values
FrequencyAllWords<- as.data.frame(table(allwords)) #table of count of word values under 'tags'
FrequencyAllWords

#allwords w/o Freq = 1
OneFreqAllWords<-filter(FrequencyAllWords,Freq !=1)
OneFreqAllWords
#shows bargraph w/o Freq=1
ggplot(data=OneFreqAllWords, aes(x=allwords, y=Freq,fill=Freq)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() + coord_fixed(ratio = 2) + theme(legend.position="none")

#top 10 of all words
TopTenWords<-top_n(FrequencyAllWords, n=10, Freq) %>%
  ggplot(., aes(x=allwords, y=Freq,fill=Freq))+
  geom_bar(stat='identity')
TopTenWords

#Frequencies more than 5
MoreThanTen<-filter(FrequencyAllWords,Freq >5)
MoreThanTen
#shows bargraph Freq>10
ggplot(data=MoreThanTen, aes(x=allwords, y=Freq,fill=Freq)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() + coord_fixed(ratio =.25) + theme(legend.position="none")


# Visualise in graphs
library("ggplot2")
names(FrequencyAllWords)
# Very basic bar graph
ggplot(data=FrequencyAllWords, aes(x=allwords, y=Freq)) +
  geom_bar(stat="identity")
# Map the time of day to different fill colors
ggplot(data=FrequencyAllWords, aes(x=allwords, y=Freq,fill=Freq)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() + coord_fixed(ratio = 5) + theme(legend.position="none")


#other frequencies
OneFrequency<-filter(FrequencyAllWords,Freq ==1)
Top10<-FrequencyAllWords%>% 
  arrange(desc(Freq)) %>%
  slice(1:10)
LowerBound<-as.integer(Top10[10,2])
EverythingElse<-filter(FrequencyAllWords, Freq<LowerBound & Freq !=1)



#code for labels:
# + labs(title="Plot of length  per dose", 
#        x="Dose (mg)", y = "Length")+
#   scale_fill_manual(values=c('black','lightgray'))+
#   theme_classic()

newT<-gsub("\\_", " ",t) 
newT

