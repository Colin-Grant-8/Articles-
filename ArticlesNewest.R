setwd("~/Downloads")
library(stringr)
library(tm)
library(stopwords)
library(dplyr)
library(tidyr)
articles1<-read.csv('articles1.csv')
articles2<-read.csv('articles2.csv')
articles3<-read.csv('articles3.csv')
articles<-rbind(articles1,articles2,articles3)
stopWords<-stopwords(language = 'en')
stopWords<-as.vector(toupper(stopWords))

'articles<-sample_n(articles,100)'
articles$content<-gsub('-','',articles$content)
articles$content<-str_replace_all((articles$content),"[^[:alpha:]]"," ")
articles$content<-toupper(stemDocument(articles$content))
count=0
allwords<-NULL
words<-NULL



for (i in (unique(articles$publication))){
  if (count!=0) {allwords<-rbind(allwords,words)}
  count=count+1
  looprandos<-filter(articles,articles$publication==i)
  words<-as.data.frame(table(unlist(strsplit(looprandos$content,' '))))
  colnames(words)<-c('word','f')
  words<-words[which(!grepl("[^\x01-\x7F]+",words$word)),]
  words$word = removeWords(as.character(words$word), stopWords)
  words<-filter(words,str_length(words$word)>2)
  words<-words %>% 
    group_by(word) %>%
    summarise(Frequency=sum(f))
  words[,'group']=i
  assign(paste0(i,' Words'),words)
  if (count==length(unique(articles$publication))){
    wordsmatrix<-allwords %>%
      pivot_wider(values_from = Frequency,names_from = group)
    wordsmatrix<-as.data.frame(wordsmatrix)
    rownames(wordsmatrix)<-wordsmatrix$word
    wordsmatrix<-wordsmatrix[,-1]
    wordsmatrix<-t(wordsmatrix)
    wordsmatrix<-as.data.frame(wordsmatrix)
    wordsmatrix[is.na(wordsmatrix)] <- 0
  }
}