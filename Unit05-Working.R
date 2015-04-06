### UNIT 5 Text Analytics

setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 05 Data Files")
getwd()

# Lecture Sequence 1 - TURNING TWEETS INTO KNOWLEDGE 

## VIDEO 5: PRE-PROCESSING IN R

tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)	# text analytics ==> must set stringsAsFactors = FALSE

str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

library(tm)			# "Text Mining"
#install.packages("SnowballC")
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus[[1]]
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]
stopwords("english")[1:20]

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

corpus = tm_map(corpus, stemDocument)
corpus[[1]]

## VIDEO 6: BAG OF WORDS IN R

frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])

findFreqTerms(frequencies, lowfreq=20)

sparse = removeSparseTerms(frequencies, 0.995)	#sparcity threshold ==> only keep terms appearing in >= 0.5% of tweets

tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))	# Important! Always do this - some terms might not be valid variable names in R (ie start with number).
tweetsSparse$Negative = tweets$Negative


library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

#qq
findFreqTerms(frequencies, lowfreq=100)

## VIDEO 7: PREDICTING SENTIMENT

#CART model based on text data

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART) # freak, hate, wtf ==> negative sentiment

predCART = predict(tweetCART, newdata=testSparse, type="class")
t = table(testSparse$Negative, predCART)
t
Accuracy = (294+18) / sum(t)
Accuracy 	# 0.8788732


table(testSparse$Negative)
BaselineAccuracy = 300 / 355 	#always predict non-negative 
BaselineAccuracy	# 0.8450704

#CART model does better.

#Try Random Forest Model.

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ . , data=trainSparse)
predictRF = predict(tweetRF, newdata=testSparse)
t = table(testSparse$Negative, predictRF)
Accuracy = (t[1,1] + t[2,2]) / sum(t)
Accuracy #0.8901408  -- a bit better, but way less interpretable than CART model.


# qq
#  try out logistic regression:

tweetLog = glm(Negative ~ . , data =trainSparse, family=binomial)
predictLog = predict(tweetLog, newdata=testSparse, type="response")
t = table(testSparse$Negative, predictLog > 0.5)
Accuracy = (t[1,1] + t[2,2]) / sum(t)
Accuracy #0.7859155  -- 

## End of lecture Sequence 1

## Begin lecture Sequence 2

##  Lecture Sequence 2 -- MAN VS. MACHINE

setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 05 Data Files")
getwd()

