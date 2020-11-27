#Loading Required Libraries
#Load Libraries
suppressPackageStartupMessages(c(library(tm), library(stm), library(tidytext), library(tidyverse), library(ggplot2),library(mongolite)))
                               
#Loading the data
newsdata<- readr::read_csv(file.choose())

#SRUCTURED TOPIC MODEL
processedNewsfeed<- textProcessor(newsdata$entry.title)
#Create Objects which the package will use to help us browse the data later
out<- prepDocuments(processedNewsfeed$documents, processedNewsfeed$vocab)
docs<- out$documents
vocab<- out$vocab

#The STM model
newsfeedSTM<- stm(documents = out$documents, vocab = out$vocab, K= 3, n= 10, max.em.its = 75, init.type = "Spectral", verbose = FALSE )
plot(newsfeedSTM)

  
