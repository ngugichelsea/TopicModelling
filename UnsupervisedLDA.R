#Loading Required Libraries
#Load Libraries
suppressPackageStartupMessages(c(library(tm), library(stm), library(tidytext), library(tidyverse), library(ggplot2),library(mongolite)))
                               
#Loading the data
newsdata<- readr::read_csv(file.choose())

#Create a Corpus
newscorpus<- Corpus(VectorSource(newsdata$entry.title))

#Data pre-processing/ Cleaning the Corpus
newscorpus<- tm_map(newscorpus, tolower)
newscorpus <- tm_map(newscorpus, removePunctuation) 
newscorpus <- tm_map(newscorpus, removeNumbers)
removeURL <- function(x) gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", x)
newscorpus<- tm_map(newscorpus, removeURL)
newscorpus<- tm_map(newscorpus, removeWords, stopwords('english'))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
newscorpus <- tm_map(newscorpus, toSpace, "â") 
newscorpus <- tm_map(newscorpus, toSpace, "/") 
newscorpus <- tm_map(newscorpus, toSpace, "@")
newscorpus <- tm_map(newscorpus, toSpace, "<")
newscorpus <- tm_map(newscorpus, toSpace, "~")
newscorpus <- tm_map(newscorpus, toSpace, "#")
newscorpus <- tm_map(newscorpus, toSpace, "Ÿ")
newscorpus <- tm_map(newscorpus, toSpace, "ð")
newscorpus <- tm_map(newscorpus, toSpace, "®")
newscorpus <- tm_map(newscorpus, toSpace, "\\|")
newscorpus <- tm_map(newscorpus, toSpace, "€")
newscorpus <- tm_map(newscorpus, toSpace, "™")
newscorpus <- tm_map(newscorpus, toSpace, "_")
newscorpus <- tm_map(newscorpus, toSpace, "-")
newscorpus <- tm_map(newscorpus, stemDocument)

#UNSUPERVISED LDA MODEL
#Create a document term matrix
newsDTM <- DocumentTermMatrix(newscorpus)

#Remove any empty rows in the document term matrix 
unique_indexes <- unique(newsDTM$i)
newsDTM <- newsDTM[unique_indexes,]

#The LDA model
newsfeed.lda<- LDA(newsDTM, k= 3, control = list(seed= 300))

#Top n words related to each of the K topics
news.terms<- terms(newsfeed.lda, 10)
news.terms
news.topics<- tidy(newsfeed.lda, matrix = "beta")
news.topics
newsfeed.top.terms<- news.topics %>% group_by(topic)%>% top_n(10, beta)%>% ungroup()%>% arrange(topic, -beta)
newsfeed.top.terms

#Plotting top words associated with the k topics
newsfeed.top.terms %>% mutate(term= reorder(term, beta)) %>% 
  mutate(topic=paste("Topic #", topic))%>%
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic, scales = "free")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size= 18))+
  labs(title = " Top terms per topic- LDA", caption = "Top Terms by Topic(betas)")+
  ylab("")+
  xlab("")+
  coord_flip()

  
