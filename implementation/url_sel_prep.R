setwd("~/Documents/Uni/MasterThesis/R/review_data/")
# Extend Java memory capacity in order to set up the TDF
#options(java.parameters = "- Xmx1024m")
options(java.parameters = "-Xmx4g" ) 
# Libraries & Packages ---------------------------
library("psych")
library("stargazer")
library("PerformanceAnalytics")
library("DescTools")
library("tabplotd3")
library("tabplot")
#----------------------------------------------------------------------------------------
# TEXT ANALYTICS
library("textcat")
library("devtools")
library("tm")
library("sentiment")
library('openNLP') #POS tagging
library('openNLPmodels.en') #EN Language model (TOK/StopWords)
library('NLP')
library('stringi')  # String operations
library('quanteda')  #Quantitative Text Analysis
library('plyr')  
# ---------------------------------------- 
require(devtools)
# -------------------------------------------
# Lookup citation() for the BibteX reference
# Lookup install_url() to install package from url 
# -------------------------------------------
#----------------------------------------------------------------------------------------
#           PART I:   URL PRE-PROCESSING
#----------------------------------------------------------------------------------------
reviews.raw <- read.csv("~/Documents/Uni/MasterThesis/R/review_data/reviews_all_raw_3.csv", 
                        header=FALSE,
                        stringsAsFactors = TRUE,
                        na.strings = c("NA","NaN", " "))  
# Or load csv files from source:
#files <- list.files(pattern = '\\.csv')
#tables <- lapply(files, read.csv, header = FALSE)
#reviews.raw <- do.call(rbind, tables); as.data.frame(reviews.raw)
# -------------------------------------------
# Identify and filter double entries
reviews.raw <- unique(reviews.raw[,c(1:10)])#or 1-9
# Annotate columns
colnames(reviews.raw) = c("c_provider","c_title", "c_instructor", 
                      "c_status", "c_description", "c_date", 
                      "u_name", "u_status", "u_rating", 
                      "u_review")
print(dim(reviews.raw))
print(variable.names(reviews.raw))
#-----------
# Separate categorical variables from review text:
# clean douplets, get rid of unnecesary variables
reviews.data <- reviews.raw[,c(1:9)]
#reviews.text <- reviews.raw[,c(10)]
#-----------
# clean up variables cache
#reviews.data$c_description <- NULL
reviews.data$c_status <- NULL
# remove(reviews.raw,
#        files,
#        tables)
#--------------------------------------------------------------------- S1.1 Data Normalization --------------------------------
reviews.data$c_provider <- tolower(reviews.data$c_provider)
reviews.data$c_title <- tolower(reviews.data$c_title)
reviews.data$c_title <- as.data.frame(reviews.data$c_title)
reviews.data$c_instructor <- as.factor(tolower(reviews.data$c_instructor))
#Merge university, university_rank, provider rank

#[C] V6 - Date of Review --------------------------- [Only relevant for C]
# coursetalk dates of format: YYYY - MM - WW - DD - HH ago where: YYYY <= 9999, MM <= 12, WW <= 4, DD <= 31 (or 30?)
# thus actual_date = scrapping_date - coursetalk_date # 1- ignore HH
#   c.date <- reviews.data$c_date; c.date <- as.matrix(unlist(c.date))
#   c.date <- gsub("ago", "", c.date); 
#   c.date <- gsub("s","", c.date)
#   c.date <- c.date
#   s.date <- strsplit(c.date, ", ")
# 
#   c.date <- gsub("hour","* 0", c.date); as.matrix(c.date)
#   c.date <- gsub("day"," * 1", c.date); as.matrix(c.date)
#   c.date <- gsub("week"," * 7", c.date); as.matrix(c.date)
#   c.date <- gsub("month"," * 30", c.date); as.matrix(c.date)
#   c.date <- gsub("year","* 365", c.date); as.matrix(c.date)
#   
#   c.date <- gsub(","," +", c.date); as.matrix(c.date)
#   c.date <- sub(" ", "", c.date); as.matrix(c.date)
#   n.days <- read.csv("~/Downloads/cdate - Sheet1.csv", sep="", header = FALSE); as.matrix.POSIXlt(n.days)
#   reviews.data$c_date <- n.days
#   c.date <- sub("^", "= ", c.date)
reviews.data$c_date <- as.Date(reviews.data$c_date)
# Annotate year and month (YYYY), (MM)
reviews.data$c_date_year <- as.numeric(format(reviews.data$c_date, "%Y"))
reviews.data$c_date_year <- as.data.frame(reviews.data$c_date_year)
reviews.data$c_date_month <- as.numeric(format(reviews.data$c_date, "%m"))
reviews.data$c_date_month <- as.data.frame(reviews.data$c_date_month)

# [A] V - Price of Course ---------------------------
library(psych)
# Merge prices from url data
# Lookup course.data$c_price
reviews.data$c_price <- NA
for(i in 1:nrow(reviews.data)){
  reviews.data$c_price[course.data$course_name[i] %in% reviews.data$c_title[i]] <- course.data$c_price[i]}
  reviews.data$c_price <- data.matrix(as.numeric(reviews.data$c_price))
  describe(reviews.data$c_price)

# [C] V7 - Name of Student --------------------------- #Either name or Student (anonymous)
reviews.data$u_name <- gsub("\n","",reviews.data$u_name)  
reviews.data$u_name <- tolower(reviews.data$u_name)
  
reviews.data$u_anonym <- NA
  reviews.data$u_anonym <- ifelse(reviews.data$u_name == "student", 1, 0)
  print(summary(reviews.data$u_anonym))
  
# V8 - Status of Student --------------------------- #
# c["Complete, Taking now, Dropped"] # 42% rated with 10 (n = 20), (tot n = 47)
reviews.data$u_status <- as.data.frame(as.factor(unlist(reviews.data$u_status)))
  # Recode variables  
reviews.data$u_status_drp <- ifelse(reviews.data$u_status == "dropped", 1, 0); data.matrix(reviews.data$u_status_drp)
  course.drp <- as.numeric(colSums(reviews.data$u_status_drp))
  print(course.drp/nrow(reviews.data)) # drop Out ratio
reviews.data$u_status_tkn <- ifelse(reviews.data$u_status == "taking now", 1, 0); data.matrix(reviews.data$u_status_tkn)
  course.tkn <- as.numeric(colSums(reviews.data$u_status_tkn))
  print(course.tkn/nrow(reviews.data)) # Taking Now ratio
reviews.data$u_status_cpm <- ifelse(reviews.data$u_status == "completed", 1, 0); data.matrix(reviews.data$u_status_cpm)
  course.cpm <- as.numeric(colSums(reviews.data$u_status_cpm))
  print(course.cpm/nrow(reviews.data)) #Completed ratio

# [A|B|C] - Rating of Student ---------------------------
#   reviews.data$u_rating <- str_extract_all(reviews$u_rating, "[0-9]+/[0-9]+")
#   reviews.data$u_rating <- gsub("/[0-9]+","", reviews$u_rating)
#   reviews.data$u_rating <- gsub("None","0", reviews$u_rating)
reviews.data$u_rating <- data.matrix(as.numeric(unlist(reviews.data$u_rating)))
  reviews.rating <- reviews.data$u_rating; data.matrix(reviews.rating)
  reviews.rating[is.na(reviews.rating)] <- 10 #replace NA with median 
  print(describe(reviews.rating))
  plot(density(reviews.rating, na.rm = T, bw = 0.5, from = 1, to = 10))
  # Hartigans (dip.test) for multimodality
  diptest::dip.test(as.numeric(reviews.data$u_rating), simulate=TRUE)
  # Variable rescale - linear interpolation
  # newvalue= (max'-min')/(max-min)*(value-min)+min'
  rating.range <- seq(1:5)
  rating.rescaled <- NA
  rating.rescale <- function(x){min(rating.range) + (((max(rating.range) - min(rating.range)) * (x - min(reviews.rating)))/(max(reviews.rating) - min(reviews.rating)))}
  rating.rescaled <- rating.rescale(na.omit(reviews.rating))
  reviews.data$u_rating <- rating.rescaled; data.matrix(reviews.data$u_rating)
  remove(rating.rescaled,
         rating.range)
  #--------------------------------------------------------------------------------------------------
library(mixtools)
# Annotate polarity according to the starring distribution ()
reviews.data$polarity <- NA
for (i in 1:nrow(reviews.data)){
    if (reviews.data$u_rating[i] >= 4){
      reviews.data$polarity[i] <- "1"};
    if (reviews.data$u_rating[i] <= 2){ # Mean = 
      reviews.data$polarity[i] <- "-1"}
  }
  reviews.data$polarity[is.na(reviews.data$polarity)] <- "0"
  reviews.data$polarity <- data.matrix(as.factor(unlist(reviews.data$polarity)))
  polarity.info <- summary(reviews.data$polarity) #starring method: polarity after rescale
  
  print(variable.names(reviews.data))
#   reviews.data.corr <- cor(reviews.data[,c(8,9)])
#   reviews.data.corr.test <- FindCorr(reviews.data.corr, cutoff = 0.35, verbose = FALSE)
#--------------------------------------------------------------------------------------------------
#                               PART III: TEXT NORMALZATION
#--------------------------------------------------------------------------------------------------
# Recall Review Text
reviews.text <- as.data.frame(reviews.raw[,c(10)]); colnames(reviews.text) <- c("text.raw")
reviews.text$text.raw <- as.character(unlist(reviews.text$text.raw))
  #Pre-Process (Yes, still is some pre-processing to do)
  url.pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  reviews.text$text.raw <- gsub(pattern = url.pattern, replacement = " ", x = reviews.text$text.raw)
# Import some features from cat
reviews.text$polarity <- data.matrix(as.numeric(reviews.data$polarity))
reviews.text$stat_dropp <- data.matrix(reviews.data$u_status_drp)
reviews.text$rating <- data.matrix(reviews.data$u_rating)
#------------------------------------------------------------------------------------------------------
  #   reviews.text <- gsub("\n"," ", reviews.text)
  #   reviews.text <- gsub("[ \t]{2,}", " ", reviews.text)
  #   reviews.text <- gsub("^\\s+|\\s+$", " ", reviews.text)
  #   reviews.text <- gsub("'ve", " have", reviews.text)
  #   reviews.text <- gsub("'re", " are", reviews.text)
  #   reviews.text <- gsub("'m", " am", reviews.text)
  #   reviews.text <- gsub("'s", " ", reviews.text)
  #   reviews.text <- gsub("n't", " not", reviews.text)
#------------------------------------------------------------------------------------------------------  
#   Quantitative Text Analysis
#------------------------------------------------------------------------------------------------------  
# Using some of the LIWC dimensions/indices will provide us some insight into the 
# student's emotional state.
# devtools::install_github("kbenoit/LIWCalike")
#------------------------------------------------------------------------------------------------------  
# require(LIWCalike)
  require(quanteda)
# require(quantedaData)

# Word Count
  reviews.text$wc <- NA
  reviews.text$wc <- sapply(reviews.text$text.raw, function(x) length(unlist(strsplit(as.character(x), "\\W+")))); reviews.text$wc <- data.matrix(reviews.text$wc)
# Sentence Count
  reviews.text$sentc <- NA
  reviews.text$sentc <- sapply(reviews.text$text.raw, function(x) length(gregexpr('[[:alnum:] ][.!?]', as.character(x))[[1]] > 0)); reviews.text$sentc <- data.matrix(reviews.text$sentc)
# Word/Sentence Ratio
  reviews.text$wps <- NA
  reviews.text$wps <- reviews.text$wc/reviews.text$sentc; reviews.text$wps <- data.matrix(reviews.text$wps)
# CapsLock Count
  reviews.text$case <- NA 
  reviews.text$case <- sapply(reviews.text$text.raw, function(x) length(gregexpr("\\b[A-Z]{2,}\\b", as.character(x))[[1]] > 0)); reviews.text$case <- data.matrix(reviews.text$case)
  reviews.text$text.raw <- tolower(reviews.text$text.raw)
# Negation Count
# See Liu2012 # <not, never, none, nobody, nowhere, neither and cannot> 
  reviews.text$neg <- str_count(as.character(reviews.text$text.raw), "(not|n't|none|nobody|nowhere|neither|cannot|no|never)"); reviews.text$neg <- data.matrix(as.numeric(reviews.text$neg))
# Negations/Sentence Ratio  
  reviews.text$neg.ratio <- reviews.text$neg/reviews.text$sentc; reviews.text$neg.ratio <- data.matrix(reviews.text$neg.ratio)
# Punctutaion Count (!)
  reviews.text$punct.excl <- NA
  reviews.text$punct.excl <- str_count(as.character(reviews.text$text.raw), "!"); reviews.text$punct.excl <- data.matrix(as.numeric(reviews.text$punct.excl))
# Punctuation Count  
  reviews.text$punct <- NA
  reviews.text$punct <- sapply(reviews.text$text.raw, function(x) length(gregexpr("[:punc:]", as.character(x))[[1]] >= 0)) 
  reviews.text$punct <- data.matrix(as.numeric(reviews.text$punct))
#------------------------------------------------------  
# Lexical Level  
# Count Positive and Negative Words (B.Liu)
# http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
  p.file <- file("positive-words.txt", "r")
  pos.lexicon <- readLines(p.file)
  reviews.text$pos.words <- vapply(reviews.text$text.raw, function(x) sum(stri_count_fixed(x, pos.lexicon)), 1L)
  reviews.text$pos.words <- data.matrix(reviews.text$pos.words)
  reviews.text$pos.words.ratio <- reviews.text$pos.words/reviews.text$wc
  
  n.file <- file("negative-words.txt", "r")
  neg.lexicon <- readLines(n.file)
  reviews.text$neg.words <- vapply(reviews.text$text.raw, function(x) sum(stri_count_fixed(x, neg.lexicon)), 1L)
  reviews.text$neg.words <- data.matrix(reviews.text$neg.words)
  reviews.text$neg.words.ratio <- reviews.text$neg.words/reviews.text$wc
  
  reviews.text$emotion.words <- data.matrix(reviews.text$pos.words + reviews.text$neg.words)
  reviews.text$emotion.words.ratio <- reviews.text$emotion.words/reviews.text$wc
  # Create a dictionary
  # opinion.dict <- dictionary(list(positive = pos.lexicon, negative = neg.lexicon))
#------------------------------------------------------  
  # Lookup for correlations between the linguistic features:  
  ling.corr <- cor(na.omit(reviews.text[,c(2:ncol(reviews.text))])) #ALL
  ling.corr <- cor(na.omit(reviews.text[,c(2,3,5,7,8,10,11,13)])) # Reduced matrix
  ling.corr.test <- FindCorr(ling.corr, cutoff = 0.99, verbose = FALSE)
  print(ling.corr.test)
  # Measuring the correlation's p-value(s)
  # if p_value <= sign_value then correlation != 0
  ling.corr.p <- corr.p(ling.corr, n = nrow(na.omit(reviews.text)), alpha = .05) 
  # Plotting correlation matrix
  library(corrplot)
  corrplot(ling.corr, method = "number")
#-------------------------------------------------------------------------------------------
  # Conversion to Corpus
  # reviews.text$text.raw <- as.String(reviews.text$text.raw)
  require('openNLP','tm')
  corpus.tmp <- as.data.frame(reviews.text$text.raw)
  corpus.tmp <- Corpus(DataframeSource(corpus.tmp))
  #-------------------------------
  require('RWeka') 
  # Corpus processing
  reviews.corpus <- tm_map(corpus.tmp, content_transformer(tolower))
  reviews.corpus <- tm_map(reviews.corpus, removeNumbers)
  reviews.corpus <- tm_map(reviews.corpus, removePunctuation)
  reviews.corpus <- tm_map(reviews.corpus, removeWords, stopwords("english"))
  #reviews.corpus <- tm_map(reviews.corpus, removeSparseTerms)
  #------------------------------
  # Corpus convertion to DTM
  reviews.dtm <- DocumentTermMatrix(reviews.corpus, 
                                    control = list(
                                    removePunctuation = TRUE,
                                    removeWords = TRUE,
                                    stopwords(kind = "english"),
                                    stripWhitespace = TRUE,
                                    #stemming = TRUE,
                                    removeSparseTerms = TRUE))    
  #The number of zero-valued elements divided by the total number of elements 
  #(e.g., m × n for an m × n matrix) is called the sparsity of the matrix 
  #(which is equal to 1 minus the density of the matrix).
  #------------------------------
  # reducing DTM dimensionality
  # reviews.dtm.xs <- removeSparseTerms(reviews.dtm, sparse = 0.75)
  # reviews.dtm.xs <- removeSparseTerms(reviews.dtm, sparse = 0.9) #83%, 10,32244,27,144007/726581
  reviews.dtm.xs <- removeSparseTerms(reviews.dtm, sparse = 0.999) # sparsity = 94%, max = 10, docs = 32244, terms = 233, Non-/sparse entries =  420788/7092064 (5.9)
  # reviews.dtm.xs <- removeSparseTerms(reviews.dtm, sparse = 0.99) # sparsity = 96%, max = 10, docs = 32244, terms = 445, Non-/sparse entries =  516245/13832335 (3.73)
  # reviews.dtm.xs <- removeSparseTerms(reviews.dtm, sparse = 0.999)# sparsity = 99%, max = 15, docs = 32244, terms = 1929, Non-/sparse entries = 668683/61529993 (1.08)
  print(head(inspect(reviews.dtm.xs)))
  print(dim(reviews.dtm.xs))
  #------------------------------
  # Measure (Overall) Term Frequency (TF)
  word.freq <- as.data.frame(colSums(as.matrix(reviews.dtm.xs))); colnames(word.freq) <- "freq"
  word.freq$word <- rownames(word.freq)
  
  word.freq$prob <- word.freq$freq/nrow(word.freq)
  
  #------------------------------
  # Tri/Bigram Analysis
  # Inspecting n-3 grams:
  reviews.corpus <- tm_map(reviews.corpus, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')), mc.cores=1)
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tdm.n2 <- TermDocumentMatrix(reviews.corpus, control = list(tokenize = BigramTokenizer))
# POS tagging (openNLP) ---------------------------
# Extracting Opinion Words/Phrases  
tagPOS <-  function(x, ...){
    s <- as.String(x)
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- Annotation(1L, "sentence", 1L, nchar(s))
    a2 <- annotate(s, word_token_annotator, a2)
    a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
    a3w <- a3[a3$type == "word"] #diff between sentence and word
    POStags <- unlist(lapply(a3w$features, '[[', "POS"))
    POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
#    list(POStagged=POStagged, POStags=POStags)
    list(POStagged=POStagged)
}
#   for (i in (1:32101)){
#     reviews.text$tagPOS[i] <- tagPOS(na.omit(reviews.text$text.raw[i]))
# }
#word.freq <- NA
for (i in (1:229)){word.freq$pos[i] <- tagPOS(word.freq$word[i])}

#Creation of a DTM
#reviews.dtm <- t(reviews.tdm)

#------------------------------------------------------------------------------------------------------  
#   Modelling
#------------------------------------------------------------------------------------------------------  
# Train [0.8]
# Also possible to 60 train -  20 test - 20 cross-validation

#train_data <- 
# Train Data: Data annotated as positive wit help of starring

# Validate / Test [0.2]
#test_data <- 

#------------------------------------------------------------------------------------------------------  
#   Plotting
#------------------------------------------------------------------------------------------------------

  
  
