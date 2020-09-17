library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(tm)
useSejongDic()
mergeUserDic(data.frame("xxx","ncn"))
setwd('c:/Rtest/txt')
#상담내역
#===============================================================================
data1 <- readLines("consulting.txt",encoding = "UTF-8")
head(data1,10)

#extract Noun
data2 <- sapply(data1, extractNoun, USE.NAMES = F)
head(unlist(data2),10)

data3 <- unlist(data2)

#gsub
data3 <- gsub("\\d+","",data3)
data3 <- gsub(" ","",data3)
data3 <- gsub("-","",data3)

#save file, load as table
data3 <- Filter(function(x){nchar(x)>=2} , data3)

write(unlist(data3),"consulting2.txt")
data4 <- read.table("consulting2.txt")
nrow(data4)
wordcount <- table(data4)
head(sort(wordcount, decreasing = T))
#wordcloud
palete <- brewer.pal(9,"Set3")
wordcloud(names(wordcount),
          freq = wordcount,
          scale=c(5,1),
          rot.per = 0.25,
          min.freq = 50,
          random.order = F,
          random.color = T,
          colors = palete)
library(wordcloud2)
pal <- brewer.pal(12,"Set3")
freq <- sort(wordcount, decreasing = T)
wf <- data.frame(word=names(freq), freq=freq)
wf <- wf[,-1]
names(wf) <- c('word','freq')
wf_pick <- subset(wf,freq>50)
wordcloud2(wf,
           shape='circle',
           size=1,
           color=pal,
           backgroundColor = "white")
library(ggplot2)
library(dplyr)
freq <- sort(wordcount, decreasing = T)
wf <- data.frame(word=names(freq), freq=freq)
wf <- wf[order(wf$freq.Freq,decreasing = T),]
head(wf)
# Plot Histogram
subset(wf, freq.Freq>150)    %>%
  ggplot(aes(freq.data4, freq.Freq)) +
  geom_bar(stat="identity", fill="darkred", colour="darkgreen") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

