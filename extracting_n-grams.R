library(tm) # framework for text mining
library(SnowballC) # provides wordStem() for stemming
library(RColorBrewer) # generate palette of colours for plots
library(ggplot2) # plot word frequencies
library(scales) # format axis scales for plots
#library(Rgraphviz) # provides term correlation plots
library(tidyr) # assists in cleaning & preparing data
library(dplyr) # assists in data manipulation, transformation, & summarization
library(RWeka)
library(stringr)

####################
# Process Sample 1 #
####################
blog1 <- read.csv("Sample1/blog1.csv", stringsAsFactors=FALSE)
news1 <- read.csv("Sample1/news1.csv", stringsAsFactors=FALSE)
twitter1 <- read.csv("Sample1/twitter1.csv", stringsAsFactors=FALSE)
sample.ls <- list(Blog = blog1, News = news1, Twitter = twitter1)

## Pre-processing the Corpus
# transform words to lower case
sample.ls <- lapply(sample.ls, function(x) apply(x, 1, function(x) tolower(x)))

# remove all punctuations
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[[:punct:]]", ""))

# remove all non-alphanumeric characters
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[^[:alpha:]]", " "))

# trim white space
sample.ls <- lapply(sample.ls, function(x) str_trim(x, side = "both"))

sample <- Corpus(VectorSource(sample.ls))

# profanity filter
profanity <- read.csv("List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/en", header=FALSE, stringsAsFactors=FALSE)
profanity <- profanity$V1
sample <- tm_map(sample, removeWords, profanity)

sample <- tm_map(sample, stripWhitespace)

rm(blog1, news1, twitter1, sample.ls)

# Next, let's look at n-grams
# Bi-gram
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions

sample <- tm_map(sample, PlainTextDocument)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(sample, control = list(tokenize = BigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

bi.gram <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

rm(tdm, matrix.tdm, df.tdm)

# Tri-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions

sample <- tm_map(sample, PlainTextDocument)

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm <- TermDocumentMatrix(sample, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

tri.gram <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

rm(tdm, matrix.tdm, df.tdm)

# Quad-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions

sample <- tm_map(sample, PlainTextDocument)

QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuadgramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quad.gram <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

rm(tdm, matrix.tdm, df.tdm)

# Quin-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions

sample <- tm_map(sample, PlainTextDocument)

QuingramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuingramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quin.gram <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

rm(tdm, matrix.tdm, df.tdm)

####################
# Process Sample 2 #
####################
blog <- read.csv("Sample2/blog2.csv", stringsAsFactors=FALSE)
news <- read.csv("Sample2/news2.csv", stringsAsFactors=FALSE)
twitter <- read.csv("Sample2/twitter2.csv", stringsAsFactors=FALSE)
sample.ls <- list(Blog = blog, News = news, Twitter = twitter)

## Pre-processing the Corpus
# transform words to lower case
sample.ls <- lapply(sample.ls, function(x) apply(x, 1, function(x) tolower(x)))

# remove all punctuations
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[[:punct:]]", ""))

# remove all non-alphanumeric characters
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[^[:alpha:]]", " "))

# trim white space
sample.ls <- lapply(sample.ls, function(x) str_trim(x, side = "both"))

# convert to corpus
sample <- Corpus(VectorSource(sample.ls))

# profanity filter
sample <- tm_map(sample, removeWords, profanity)

# trim whitespace again
sample <- tm_map(sample, stripWhitespace)

rm(blog, news, twitter, sample.ls)

# Next, let's look at n-grams
# Bi-gram
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = BigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

bi.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

bi.gram <- rbind(bi.gram, bi.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, bi.gram.alt)

# Tri-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

tri.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

tri.gram <- rbind(tri.gram, tri.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, tri.gram.alt)

# Quad-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuadgramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quad.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

quad.gram <- rbind(quad.gram, quad.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, quad.gram.alt)

# Quin-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuingramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quin.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

quin.gram <- rbind(quin.gram, quin.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, quin.gram.alt, sample)

####################
# Process Sample 3 #
####################
blog <- read.csv("Sample3/blog3.csv", stringsAsFactors=FALSE)
news <- read.csv("Sample3/news3.csv", stringsAsFactors=FALSE)
twitter <- read.csv("Sample3/twitter3.csv", stringsAsFactors=FALSE)
sample.ls <- list(Blog = blog, News = news, Twitter = twitter)

## Pre-processing the Corpus
# transform words to lower case
sample.ls <- lapply(sample.ls, function(x) apply(x, 1, function(x) tolower(x)))

# remove all punctuations
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[[:punct:]]", ""))

# remove all non-alphanumeric characters
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[^[:alpha:]]", " "))

# trim white space
sample.ls <- lapply(sample.ls, function(x) str_trim(x, side = "both"))

# convert to corpus
sample <- Corpus(VectorSource(sample.ls))

# profanity filter
sample <- tm_map(sample, removeWords, profanity)

# trim whitespace again
sample <- tm_map(sample, stripWhitespace)

rm(blog, news, twitter, sample.ls)

# Next, let's look at n-grams
# Bi-gram
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = BigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

bi.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

bi.gram <- rbind(bi.gram, bi.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, bi.gram.alt)

# Tri-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

tri.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

tri.gram <- rbind(tri.gram, tri.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, tri.gram.alt)

# Quad-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuadgramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quad.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

quad.gram <- rbind(quad.gram, quad.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, quad.gram.alt)

# Quin-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuingramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quin.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

quin.gram <- rbind(quin.gram, quin.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, quin.gram.alt, sample)

####################
# Process Sample 4 #
####################
blog <- read.csv("Sample4/blog4.csv", stringsAsFactors=FALSE)
news <- read.csv("Sample4/news4.csv", stringsAsFactors=FALSE)
twitter <- read.csv("Sample4/twitter4.csv", stringsAsFactors=FALSE)
sample.ls <- list(Blog = blog, News = news, Twitter = twitter)

## Pre-processing the Corpus
# transform words to lower case
sample.ls <- lapply(sample.ls, function(x) apply(x, 1, function(x) tolower(x)))

# remove all punctuations
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[[:punct:]]", ""))

# remove all non-alphanumeric characters
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[^[:alpha:]]", " "))

# trim white space
sample.ls <- lapply(sample.ls, function(x) str_trim(x, side = "both"))

# convert to corpus
sample <- Corpus(VectorSource(sample.ls))

# profanity filter
sample <- tm_map(sample, removeWords, profanity)

# trim whitespace again
sample <- tm_map(sample, stripWhitespace)

rm(blog, news, twitter, sample.ls)

# Next, let's look at n-grams
# Bi-gram
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = BigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

bi.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

bi.gram <- rbind(bi.gram, bi.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, bi.gram.alt)

# Tri-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

tri.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

tri.gram <- rbind(tri.gram, tri.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, tri.gram.alt)

# Quad-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuadgramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quad.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

quad.gram <- rbind(quad.gram, quad.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, quad.gram.alt)

# Quin-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuingramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quin.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

quin.gram <- rbind(quin.gram, quin.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, quin.gram.alt, sample)

####################
# Process Sample 5 #
####################
blog <- read.csv("Sample5/blog5.csv", stringsAsFactors=FALSE)
news <- read.csv("Sample5/news5.csv", stringsAsFactors=FALSE)
twitter <- read.csv("Sample5/twitter5.csv", stringsAsFactors=FALSE)
sample.ls <- list(Blog = blog, News = news, Twitter = twitter)

## Pre-processing the Corpus
# transform words to lower case
sample.ls <- lapply(sample.ls, function(x) apply(x, 1, function(x) tolower(x)))

# remove all punctuations
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[[:punct:]]", ""))

# remove all non-alphanumeric characters
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[^[:alpha:]]", " "))

# trim white space
sample.ls <- lapply(sample.ls, function(x) str_trim(x, side = "both"))

# convert to corpus
sample <- Corpus(VectorSource(sample.ls))

# profanity filter
sample <- tm_map(sample, removeWords, profanity)

# trim whitespace again
sample <- tm_map(sample, stripWhitespace)

rm(blog, news, twitter, sample.ls)

# Next, let's look at n-grams
# Bi-gram
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = BigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

bi.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

bi.gram <- rbind(bi.gram, bi.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, bi.gram.alt)

# Tri-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

tri.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

tri.gram <- rbind(tri.gram, tri.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, tri.gram.alt)

# Quad-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuadgramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quad.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

quad.gram <- rbind(quad.gram, quad.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, quad.gram.alt)

# Quin-grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuingramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quin.gram.alt <- df.tdm %>%
        mutate(Total = Blog + News + Twitter) %>%
        select(token, Total)

quin.gram <- rbind(quin.gram, quin.gram.alt) %>%
        group_by(token) %>%
        summarise(Total = sum(Total))

rm(tdm, matrix.tdm, df.tdm, quin.gram.alt, sample, profanity, BigramTokenizer,
   TrigramTokenizer, QuadgramTokenizer, QuingramTokenizer)


# Analyze n-grams to identify low probability n-grams for removal
bi.gram <- bi.gram %>%
        arrange(desc(Total)) %>%
        mutate(Percent = Total/sum(Total)) %>%
        mutate(Cummulative = cumsum(Percent))

tri.gram <- tri.gram %>%
        arrange(desc(Total)) %>%
        mutate(Percent = Total/sum(Total)) %>%
        mutate(Cummulative = cumsum(Percent))

quad.gram <- quad.gram %>%
        arrange(desc(Total)) %>%
        mutate(Percent = Total/sum(Total)) %>%
        mutate(Cummulative = cumsum(Percent))

quin.gram <- quin.gram %>%
        arrange(desc(Total)) %>%
        mutate(Percent = Total/sum(Total)) %>%
        mutate(Cummulative = cumsum(Percent))


######################
# Combine dataframes #
######################

bi.gram$count <- 2
tri.gram$count <- 3
quad.gram$count <- 4
quin.gram$count <- 5

bi.gram <- bi.gram %>%
        mutate(key = word(token, 1), predict = word(token, -1)) %>%
        select(key, predict, total = Total, count)

tri.gram <- tri.gram %>%
        mutate(key = word(token, 1, 2), predict = word(token, -1)) %>%
        select(key, predict, total = Total, count)

quad.gram <- quad.gram %>%
        mutate(key = word(token, 1, 3), predict = word(token, -1)) %>%
        select(key, predict, total = Total, count)

quin.gram <- quin.gram %>%
        mutate(key = word(token, 1, 4), predict = word(token, -1)) %>%
        select(key, predict, total = Total, count)

final.df <- rbind(bi.gram, tri.gram, quad.gram, quin.gram) #has 4M unique tokens

test <- final.df %>% filter(total > 2)


final.df <- read.csv("final.df.csv", stringsAsFactors=FALSE)

test <- final.df %>%
        group_by(count) %>%
        mutate(sum_total = sum(total)) %>%
        mutate(cum_sum = cumsum(total)) %>%
        mutate(cum_perc = cum_sum/sum_total) %>%
        filter(cum_perc < .75) %>%
        select(key, predict, total, count)

saveRDS(test, "final.rds")

write.csv(test, file = "final.df.csv", row.names = FALSE)

top.unigram <- final.df %>% 
        group_by(predict) %>% 
        summarise(total = sum(total)) %>% 
        arrange(desc(total)) %>%
        mutate(probability = total/sum(total)) %>%
        filter(row_number() <= 100)

saveRDS(top.unigram, "top.unigram.rds")

######################
# Algorithm Building #
######################

# user input
user.input <- " on an uprising that began more than a year"
user.input <- tolower(user.input)
user.input <- as.vector(strsplit(user.input," ")[[1]])
user.input <- str_replace_all(user.input, "[^[:alpha:]]", "")
user.input <- grep('.', user.input, value=TRUE)
user.input <- paste(user.input, sep=" ", collapse=" ")

# get length of user input
i <- length(strsplit(user.input," ")[[1]])

if (i > 4){
        sub.input <- seq(i-3, i)
        user.input <- as.vector(strsplit(user.input," ")[[1]])[sub.input]
        i <- length(user.input)
}

# input for search
full.input <- paste(user.input, sep=" ", collapse= " ")

n <- i+1

# subset based on length of user input
source.df <- final.df %>%
        filter(count == n) %>%
        filter(key == full.input)

# if search yields no results look for n-1 gram
if(nrow(source.df) == 0 & i > 1){
        l = n
        l = l-1
        full.input2 <- strsplit(full.input, " ")[[1]][2:i]
        full.input2 <- paste(full.input2, sep=" ", collapse=" ")
        source.df <- final.df %>%
                filter(count == l) %>%
                filter(key == full.input2)
}

if(nrow(source.df) == 0 & i > 2){
        l = n
        l = l-2
        full.input3 <- strsplit(full.input, " ")[[1]][3:i]
        full.input3 <- paste(full.input3, sep=" ", collapse=" ")
        source.df <- final.df %>%
                filter(count == l) %>%
                filter(key == full.input3)
}

if(nrow(source.df) == 0 & i > 3){
        l = n
        l = l-3
        full.input4 <- strsplit(full.input, " ")[[1]][4:i]
        full.input4 <- paste(full.input4, sep=" ", collapse=" ")
        source.df <- final.df %>%
                filter(count == l) %>%
                filter(key == full.input4)
}


###### MISSPELLINGS or APPROXIMATES########
# find approximate of initial input
if(nrow(source.df) == 0 & i > 3){
        
        sub.df <- final.df %>%
                filter(count == 5)
        full.input5 <- strsplit(full.input, " ")[[1]][c(1,3:i)]
        start <- paste("^",full.input5[1], sep="", collapse="")
        end <- paste(full.input5[3],"\\b", sep="", collapse="")
        middle <- paste("\\w+", full.input5[2], sep=" ", collapse="")
        term1 <- paste(start, middle, end, sep=" ", collapse="")
        
        full.input5 <- strsplit(full.input, " ")[[1]][c(1:2,i)]
        start <- paste("^",full.input5[1], sep="", collapse="")
        end <- paste(full.input5[3],"\\b", sep="", collapse="")
        middle <- paste(full.input5[2], "\\w+", sep=" ", collapse="")
        term2 <- paste(start, middle, end, sep=" ", collapse="")
        
        term <- paste(term1, "|", term2, sep="", collapse=" ")
        
        source.df <- sub.df[grep(term, sub.df$key),]
}

if(nrow(source.df) == 0 & i > 2){
        
        sub.df <- final.df %>%
                filter(count == 4)
        full.input5 <- strsplit(full.input, " ")[[1]][c(1,i)]
        start <- paste("^",full.input5[1], sep="", collapse="")
        end <- paste(full.input5[2],"\\b", sep="", collapse="")
        end <- paste("\\w+", end, sep=" ", collapse="")
        term <- paste(start, end, sep=" ", collapse="")
        
        source.df <- sub.df[grep(term, sub.df$key),]
}

if(nrow(source.df) == 0)
        c("the", "to", "and") else
                c(source.df$predict, "the", "to", "and")[1:3]