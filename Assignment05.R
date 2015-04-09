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
library(rpart)
library(rpart.plot)

vanCART = rpart(Vandal ~ ., data=train, method="class")
prp(vanCART) # 

predCART = predict(vanCART, newdata=test, type="class")
t = table(test$Vandal, predCART)
t
(618+12)/(618+12+533)

## 2.1

wikiWords2 = wikiWords #According to assignment text, makes a copy (not a reference)

wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

nrow(subset(wikiWords2, wikiWords2$HTTP==TRUE))

(609+57)/sum(t)


wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

vanCART = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(vanCART) # 

predCART = predict(vanCART, newdata=wikiTest2, type="class")
t = table(wikiTest2$Vandal, predCART)
t
(514+248)/sum(t)

##3.1

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin


wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)

vanCART = rpart(Vandal ~ ., data=wikiTrain3, method="class")
prp(vanCART) # 

predCART = predict(vanCART, newdata=wikiTest3, type="class")
t = table(wikiTest3$Vandal, predCART)
t
(t[1,1]+t[2,2])/sum(t)

## Part 2 -- AUTOMATING REVIEWS IN MEDICINE


setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 05 Data Files")
getwd()

trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(trials)
summary(trials)

nchar(trials$abstract)
max(nchar(trials$abstract))

nrow(subset(trials, nchar(trials$abstract)==0))

subset(trials, nchar(trials$title)==min(nchar(trials$title)))$title

##2.1

library(tm)
library(SnowballC)

length(stopwords("english")) #174 ==> OK
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract

sparseTitle = removeSparseTerms(dtmTitle, 0.95)	
sparseTitle
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)	
sparseAbstract

dtmTitle = as.data.frame(as.matrix(sparseTitle))
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))

tapply(dtmAbstract, colSums)
max(colSums(dtmAbstract))

wCount = colSums(dtmAbstract)
sort(wCount)

##3.1 Building a model


library(rpart)
library(rpart.plot)

