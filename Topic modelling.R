# Comment analysis

##libraries----
library(tm)
library(tokenizers)
library(textstem)
library(SnowballC)
library(jsonlite)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(ggplot2)
library(LDAvis)
library(servr)


#data loading----

RawData <- fromJSON("https://query.data.world/s/4ria2tfww73wmhfzke5z2w4zlez2re")

#student number 201582023-----

set.seed(023)
TestSet <- sample_n(RawData,5000)


#data preparation-----

## dividing the positive and negative comments

stars <- substr(TestSet$review_rating,1,1)
TestSetStar <- cbind(TestSet,stars)
Posit <- subset(TestSetStar, stars == "4"| stars == "5")
Negat <- subset(TestSetStar, stars == "1"| stars == "2")


#positive comments----

##binding the title and text and preparing corpus


TestBindpo <- paste(Posit$review_title,Posit$review_text)
Posit2 <- cbind(TestBindpo,Posit)
reviewspo <- stringr::str_conv(Posit2$TestBind, "UTF-8")
corppo <- Corpus(VectorSource(reviewspo))
print(corppo[[150]]$content)

### Lemmatization and Tokenization positive

lemmapo <- tm_map(corppo, lemmatize_strings)
WordsTokenpo <- tokenize_words(lemmapo$content)
content
print(WordsTokenpo[10])

### dtm - TF

dtmpo <- DocumentTermMatrix(corppo, control = list(lemma = T, removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf)))
dtmpo2 <- DocumentTermMatrix(corppo, control = list(lemma = T, removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf), weighting = function(x) weightTfIdf(x, normalize = F)))
findFreqTerms(dtmpo,lowfreq = 200)
findFreqTerms(dtmpo2,lowfreq = 100)
RawSumpo <- apply(dtmpo,1,FUN=sum)
dtmpo <- dtmpo[RawSumpo!=0,]


###frequency and removing spares

findFreqTerms(dtmpo,lowfreq = 100)
dtmspo <- removeSparseTerms(dtmpo, 0.97)
RawSumspo <- apply(dtmspo,1,FUN=sum)
dtmspo <- dtmspo[RawSumspo!=0,]
findFreqTerms(dtmspo,lowfreq = 100)
outputpo <- as.matrix(dtmspo)

#### frequency table positive
dtmsNewpo <- as.matrix(dtmspo)
frequencypo <- colSums(dtmsNewpo)
frequencypo <- sort(frequencypo, decreasing = T)
DocLengthpo <- rowSums(dtmsNewpo)
words <- names(frequencypo)
wordcloud(words[1:100], frequency[1:100], rot.per=0.15, random.order =F, scale = c(5,0.5), random.color = F, colors=brewer.pal(8,"Dark2"))

#negative comments----

##binding the title and text and preparing corpus

TestBindne <- paste(Negat$review_title,Negat$review_text)
Negat2 <- cbind(TestBindne,Negat)
reviewsne <- stringr::str_conv(Negat2$TestBind, "UTF-8")
corpne <- Corpus(VectorSource(reviewsne))
print(corpne[[150]]$content)

### Lemmatization and Tokenizationpositive

lemmane <- tm_map(corpne, lemmatize_strings)
WordsTokenne <- tokenize_words(lemmane$content)
print(WordsTokenne[100])

### dtm - TF-IDF

dtmne <- DocumentTermMatrix(corpne, control = list(lemma = T, removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf)))
RawSumne <- apply(dtmne,1,FUN=sum)
dtmne <- dtmne[RawSumne!=0,]


###frequency and removing spares

findFreqTerms(dtmne,lowfreq = 100)
dtmsne <- removeSparseTerms(dtmne, 0.97)
RawSumsne <- apply(dtmsne,1,FUN=sum)
dtmsne <- dtmsne[RawSumsne!=0,]
findFreqTerms(dtmsne,lowfreq = 100)
outputne <- matrix(dtmsne)

#### frequency table positive
dtmsNewne <- as.matrix(dtmsne)
frequencyne <- colSums(dtmsNewne)
frequencyne <- sort(frequencyne, decreasing = T)
DocLengthne <- rowSums(dtmsNewne)
wordsne <- names(frequencyne)
wordcloud(wordsne[1:100], frequency[1:100], rot.per=0.15, random.order =F, scale = c(5,0.5), random.color = F, colors=brewer.pal(8,"Dark2"))


#Topic modeling - sparse removed----

## positive
### choosing K

iter <- 4000
coherencepo <- c()
for(i in (2:15)){
  ldaOutpo <- LDA(dtmspo,i, method="Gibbs", control = list(iter=iter,seed=023))
  phi <- posterior(ldaOutpo)$terms %>% as.matrix
  theta <- posterior(ldaOutpo)$topics %>% as.matrix
  coherence_one <- mean(textmineR::CalcProbCoherence(phi = phi,
                                                     dtm = dtmsNewpo))
  coherencepo <-append(coherencepo,coherence_one)
}

kpo <- c(2:15)[which.max(coherencepo)]
print(kpo)

coherence_matpo <- data.frame(k = c(2:15),
                            coherence = coherencepo,
                            stringsAsFactors = FALSE)
ggplot(coherence_matpo, aes(x = k, y = coherencepo)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Number of Topic by Coherence Score - Positive comments") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,31,1)) + ylab("Coherence")

### model interpretation

k=4
iter=4000
ldapo <- LDA(dtmspo,k, method="Gibbs", control=list(iter=iter, seed=023))
phipo <- posterior(ldapo)$terms %>% as.matrix
ldaOuttermspo <- as.matrix(terms(ldapo, 20))
thetapo <- posterior(ldapo)$topics %>% as.matrix
thetapocom <- cbind(thetapo[1:1000,], Posit$review_text[1:1000])
write.csv(thetapocom,file="thetapocom.csv")
ldapoTerms <- as.matrix(terms(ldapo,10))

### documents vs topics map

ldaOutTopicspo <- data.frame(topics(ldapo))
ldaOutTopicspo$index <- as.numeric(row.names(ldaOutTopicspo))
TestSet$index <- as.numeric(row.names(TestSet))
datawithtopicpo <- merge(TestSet, ldaOutTopicspo, by='index',all.x=TRUE)
datawithtopicpo <- datawithtopicpo[order(datawithtopicpo$index), ]

### documents in each topic

topicProbabilitiespo <- as.data.frame(ldapo@gamma)

### visualization

vocabpo <- colnames(phipo) 
#vocab list in DTM
# create the JSON object to feed the visualization in LDAvis:
json_lda <- createJSON(phi = phipo, theta = thetapo,
                       vocab = vocabpo, doc.length = DocLengthpo,
                       term.frequency = frequencypo)

serVis(json_lda, out.dir = 'vis', open.browser = TRUE)

## negative
### choosing K
iter <- 4000
coherencene <- c()
for(i in (2:15)){
  ldaOutne <- LDA(dtmsne,i, method="Gibbs", control = list(iter=iter,seed=023))
  phi <- posterior(ldaOutne)$terms %>% as.matrix
  theta <- posterior(ldaOutne)$topics %>% as.matrix
  coherence_one <- mean(textmineR::CalcProbCoherence(phi = phi,
                                                     dtm = dtmsNewne))
  coherencene <-append(coherencene,coherence_one)
}

kne <- c(2:15)[which.max(coherencene)]
print(kne)
coherence_matne <- data.frame(k = c(2:15),
                              coherence = coherencene,
                              stringsAsFactors = FALSE)
ggplot(coherence_matne, aes(x = k, y = coherencene)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Number of Topic by Coherence Score - Negative comments") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,31,1)) + ylab("Coherence")
### model interpretation

k=6
iter=4000
ldane <- LDA(dtmsne,k, method="Gibbs", control=list(iter=iter, seed=023))
phine <- posterior(ldane)$terms %>% as.matrix
ldaOuttermsne <- as.matrix(terms(ldane, 20))
thetane <- posterior(ldane)$topics %>% as.matrix
thetanecom <- cbind(thetane[1:500,], Negat$review_text[1:500])
write.csv(thetanecom,file="thetanecom.csv")
ldaneTerms <- as.matrix(terms(ldane,15))
### visualization

vocabne <- colnames(phine) 
#vocab list in DTM
# create the JSON object to feed the visualization in LDAvis:
json_ldane <- createJSON(phi = phine, theta = thetane,
                       vocab = vocabne, doc.length = DocLengthne,
                       term.frequency = frequencyne)

serVis(json_ldane, out.dir = 'vis', open.browser = TRUE)

