#importing all necessary packages
library ('tm')
library ('plyr')
library ('class')
library ('stringr')
library(e1071)
library('lm')
library(randomForest)
library('klaR')
library('caret')
library('rpart')
library('rattle')
install.packages('pROC')
library(pROC)

setwd('C:/*********/')


set.seed(9850)
#impoering all files into one corresponding data set
neg_list <-list.files("C:/*****path****/neg/")
negtext <- lapply(neg_list, function(x)read.table(x, header = F, sep = "\t", stringsAsFactors = F))
pos_list <-list.files("C:/*****path****/pos/")
postext <- lapply(pos_list, function(x)read.table(x, header = F, sep = "\t", stringsAsFactors = F))


#removing unwated whitespace/tab in data
postext <- unlist(lapply(postext, function(x) { str_split(x, "\n") })) # pre-processing step
negtext <- unlist(lapply(negtext, function(x) { str_split(x, "\n") })) # pre-processing step



#loading up affinword list, words are rated from -5 to +5
afinn_list <- read.delim(file='C:/Users/AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <-c('word','score')
afinn_list$word <- tolower(afinn_list$word)



#feature selection step
#categorize words from very negative to very positive and add some more movie-specific words stuff

vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")


#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #unnecessary characters are removed and splliting up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)  # pre-processing step
    sentence <- gsub('[[:cntrl:]]', '', sentence)  ## pre-processing step
    sentence <- gsub('\\d+', '', sentence)    # pre-processing step
    sentence <- tolower(sentence)          # pre-processing step
    wordList <- str_split(sentence, '\\s+')  # pre-processing step
    words <- unlist(wordList)
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #Adding number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}    

#build tables of positive and negative sentences with scores
posResult <- as.data.frame(sentimentScore(postext, vNegTerms, negTerms, posTerms, vPosTerms))
negResult <- as.data.frame(sentimentScore(negtext, vNegTerms, negTerms, posTerms, vPosTerms))
posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
negResult <- cbind(negResult, 'negative')
colnames(negResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')    

#Final table
results <- rbind(posResult, negResult) # input data in table/matrix form

#but as data is organized that all positive is first then negative, i would convert into mixture of postitive and negative
grp <-runif(nrow(results))
result <- results[order(grp),]
result$sentence <- gsub('[[:punct:]]','',result$sentence)

# spplitting data into train n test data set
indexes = sample(1:nrow(results), size=0.35*nrow(results))

# Split data into train and test
test = results[indexes,]
dim(test)  

train = results[-indexes,]
dim(train) 
