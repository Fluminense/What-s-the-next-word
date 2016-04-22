inicio<-proc.time()

library(stringr)
library(SnowballC)

# Set the directory
setwd("Capstone Project/en_US/")

# Set connections and reading lines
conNews<-file("en_US.news.txt", "rb")
conBlog<-file("en_US.blogs.txt","rb")
conTwitter<-file("en_US.twitter.txt","rb")

Lnews<-length(readLines(conNews))
Lblog<-length(readLines(conBlog))
Ltwitter<-length(readLines(conTwitter))

#
set.seed(1)

conNews<-file("en_US.news.txt", "rb",encoding="UTF-8")
flipnews<-rbinom(n = Lnews,size = 1,prob = 0.05)
fnews<-function(){x<-NULL; 
for (i in flipnews){
    if (i==1)x<-c(x,readLines(conNews,1))};return(x)}
news<-fnews()

conBlog<-file("en_US.blogs.txt","rb",encoding="UTF-8")
flipblog<-rbinom(n = Lblog,size = 1,prob = 0.05)
fblog<-function(){x<-NULL; 
for (i in flipblog){
    if (i==1)x<-c(x,readLines(conBlog,1))};return(x)}
blog<-fblog()

conTwitter<-file("en_US.twitter.txt","rb",encoding="UTF-8")
fliptwitter<-rbinom(n = Ltwitter,size = 1,prob = 0.05)
ftwitter<-function(){x<-NULL; 
for (i in fliptwitter){
    if (i==1)x<-c(x,readLines(conTwitter,1))};return(x)}
twitter<-ftwitter()

rm(flipblog,flipnews,fliptwitter)  #removes object to save size
close(conNews,conBlog,conTwitter)
rm(fliptwitter,flipblog,flipnews)
###
library(tm)
corpusNews <- Corpus(VectorSource(news))
corpusBlog <- Corpus(VectorSource(blog))
corpusTwitter <- Corpus(VectorSource(twitter))
####
#Cleaning the corpus from puctuation, cursing words, white spaces, etcetera
toQuotes<- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})
cursing<- readLines("badwords.txt", warn = F)
cursing <- tolower(cursing)
cursing<-removePunctuation(cursing)   

corpusNews <- tm_map(corpusNews, toQuotes, "â|€|™|œ|˜|“|”|…|\u009d")
corpusNews <- tm_map(corpusNews, removePunctuation)
corpusNews <- tm_map(corpusNews, stripWhitespace)
corpusNews <- tm_map(corpusNews, removeNumbers)
corpusNews <- tm_map(corpusNews, content_transformer(tolower))
corpusNews <- tm_map(corpusNews, removeWords, cursing)

corpusBlog <- tm_map(corpusBlog, toQuotes, "â|€|™|œ|˜|“|”|…|\u009d")
corpusBlog <- tm_map(corpusBlog, removePunctuation)
corpusBlog <- tm_map(corpusBlog, stripWhitespace)
corpusBlog <- tm_map(corpusBlog, removeNumbers)
corpusBlog <- tm_map(corpusBlog, content_transformer(tolower))
corpusBlog <- tm_map(corpusBlog, removeWords, cursing)

corpusTwitter <- tm_map(corpusTwitter, toQuotes, "â|€|™|œ|˜|“|”|…|\u009d")
corpusTwitter <- tm_map(corpusTwitter, removePunctuation)
corpusTwitter <- tm_map(corpusTwitter, stripWhitespace)
corpusTwitter <- tm_map(corpusTwitter, removeNumbers)
corpusTwitter <- tm_map(corpusTwitter, content_transformer(tolower))
corpusTwitter <- tm_map(corpusTwitter, removeWords, cursing)
####
# Creating the Document Term Matrix
dtmNews <- DocumentTermMatrix(corpusNews)
dtmNews <-removeSparseTerms(dtmNews , 0.99999)

dtmBlog <- DocumentTermMatrix(corpusBlog)
dtmBlog <-removeSparseTerms(dtmBlog , 0.99999)

dtmTwitter <- DocumentTermMatrix(corpusTwitter)
dtmTwitter <-removeSparseTerms(dtmTwitter , 0.99999)

# Merging the databases

dtm<-c(dtmBlog,dtmNews,dtmTwitter)
rm(dtmNews,dtmBlog,dtmTwitter)
# Computing frequencies of words
freq<-slam::col_sums(dtm, na.rm = T)
freq <- sort(freq, decreasing=TRUE)


# Bigrams

BigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

dtmBlogBigram <- DocumentTermMatrix(corpusBlog, control = list(tokenize = BigramTokenizer))
dtmBlogBigram <-removeSparseTerms(dtmBlogBigram , 0.99999)

dtmNewsBigram <- DocumentTermMatrix(corpusNews, control = list(tokenize = BigramTokenizer))
dtmNewsBigram <-removeSparseTerms(dtmNewsBigram , 0.99999)

dtmTwitterBigram <- DocumentTermMatrix(corpusTwitter, control = list(tokenize = BigramTokenizer))
dtmTwitterBigram <-removeSparseTerms(dtmTwitterBigram , 0.99999)

dtmBigram<-c(dtmBlogBigram,dtmNewsBigram,dtmTwitterBigram)
rm(dtmBlogBigram,dtmNewsBigram,dtmTwitterBigram)

freqBigram<-slam::col_sums(dtmBigram, na.rm = T)
freqBigram <- sort(freqBigram, decreasing=TRUE)

# Trigrams

TrigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

dtmBlogTrigram <- DocumentTermMatrix(corpusBlog, control = list(tokenize = TrigramTokenizer))
dtmBlogTrigram <-removeSparseTerms(dtmBlogTrigram , 0.99998)

dtmNewsTrigram <- DocumentTermMatrix(corpusNews, control = list(tokenize = TrigramTokenizer))
dtmNewsTrigram <-removeSparseTerms(dtmNewsTrigram , 0.99998)

dtmTwitterTrigram <- DocumentTermMatrix(corpusTwitter, control = list(tokenize = TrigramTokenizer))
dtmTwitterTrigram <-removeSparseTerms(dtmTwitterTrigram , 0.99998)

dtmTrigram<-c(dtmBlogTrigram,dtmNewsTrigram,dtmTwitterTrigram)
rm(dtmBlogTrigram,dtmNewsTrigram,dtmTwitterTrigram)

freqTrigram<-slam::col_sums(dtmTrigram, na.rm = T)
freqTrigram <- sort(freqTrigram, decreasing=TRUE)

#Quadrigrams

QuadrigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

dtmBlogQuadrigram <- DocumentTermMatrix(corpusBlog, control = list(tokenize = QuadrigramTokenizer))
dtmBlogQuadrigram <-removeSparseTerms(dtmBlogQuadrigram , 0.99998)

dtmNewsQuadrigram <- DocumentTermMatrix(corpusNews, control = list(tokenize = QuadrigramTokenizer))
dtmNewsQuadrigram <-removeSparseTerms(dtmNewsQuadrigram , 0.99998)

dtmTwitterQuadrigram <- DocumentTermMatrix(corpusTwitter, control = list(tokenize = QuadrigramTokenizer))
dtmTwitterQuadrigram <-removeSparseTerms(dtmTwitterQuadrigram , 0.99998)

dtmQuadrigram<-c(dtmBlogQuadrigram,dtmNewsQuadrigram,dtmTwitterQuadrigram)
rm(dtmBlogQuadrigram,dtmNewsQuadrigram,dtmTwitterQuadrigram)

freqQuadrigram<-slam::col_sums(dtmQuadrigram, na.rm = T)
freqQuadrigram <- sort(freqQuadrigram, decreasing=TRUE)


###


tokenize<-function(x){
    x <- tolower(x)                    #transform all the word to lowercase
    x <- gsub("[^a-z\n\']", " ", x)    #keep only the alphabetic characters
    x <- unlist(strsplit(x," "))       #separate string into words 
    x <- grep("\\S",x,value = T)       #remove any white spaces
    x <- removePunctuation(x)
    return(x)}


proc.time()-inicio
