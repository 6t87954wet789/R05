##Assignment 5

setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 05 Data Files")
getwd()

## Part 1 -- DETECTING VANDALISM ON WIKIPEDIA

wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)	#text analytics ==> FALSE
str(wiki)

wiki$Vandal = as.factor(wiki$Vandal)
nrow(subset(wiki, wiki$Vandal==1))

library(tm)
library(SnowballC)

length(stopwords("english")) #174 ==> OK

corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)	
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
str(wordsAdded)


corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)	
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

##1.5

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
colnames(wikiWords) = make.names(colnames(wikiWords))
library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, split==TRUE)
test = subset(wikiWords, split==FALSE)

table(test$Vandal)
618/(618+545)

## 1.6
