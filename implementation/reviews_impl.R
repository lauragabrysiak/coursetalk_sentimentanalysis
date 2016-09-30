setwd("~/Documents/Uni/MasterThesis/R/review_data/")
  # Extend Java memory capacity in order to set up the TDF
  #options(java.parameters = "- Xmx1024m")
  options(java.parameters = "-Xmx4g" ) 
# 0-Libraries & Packages ---------------------------
  # STATISTICS
  library("psych")
  library("stargazer")
  library("DescTools")
  library("infotheo")
  library("corrplot")
  library("devtools")
  library('plyr') 
  library('cluster')
  #library("PerformanceAnalytics")
#----------------------------------------------------------------------------------------
  # NLP
  library("textcat")
  library("stringr")
  library('stringi')  # String operations
  library("tm")
  library("sentiment")
  # https://cran.r-project.org/src/contrib/Archive/sentiment/
  library('quanteda')  #Quantitative Text Analysis
#  library('StanfordCoreNLP') # NLP # http://datacube.wu.ac.at/src/contrib/StanfordCoreNLP_0.1-1.tar.gz
  library('openNLP') #POS tagging
  library('openNLPmodels.en') #EN Language model (TOK/StopWords)
  library('NLP')
  library('syuzhet') #https://cran.r-project.org/src/contrib/syuzhet_1.0.0.tar.gz
  #----------------------------------------------------------------------------------------
  # ML
  library('e1071')  
  library('caret')  # caret: https://cran.r-project.org/src/contrib/caret_6.0-71.tar.gz  
  library('ROCR')
  # DEPENDENCIES -> require(nloptr,pbkrtest,car)
  # nloptr : http://cran.r-project.org/src/contrib/Archive/nloptr/nloptr_1.0.0.tar.gz
  # car : https://cran.r-project.org/src/contrib/car_2.1-3.tar.gz
  # pbkrtest : https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz  (3.2.2)
#---------------------------------------- 
  require(devtools)
# -------------------------------------------
  # Lookup citation() for the BibteX reference
  # Lookup install_url() to install package from url 
# A- Useful Functions --------------------------
  corstars <- function(x){
    require(Hmisc)
    x <- as.matrix(x)
    R <- rcorr(x)$r
    p <- rcorr(x)$P
    mystars <- ifelse(p < .01, "**", ifelse(p < .05, "* ", "  "))
    R <- format(round(cbind(rep(-1.111, ncol(x)), R), 3))[,-1]
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), "  ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    Rnew <- as.data.frame(Rnew)
    return(Rnew)}
#----------------------------------------------------------------------------------------
# Part II: PROCESSING REVIEWS DATASET  
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#                       1: URL PRE-PROCESSING
#----------------------------------------------------------------------------------------
reviews.raw <- read.csv("~/Documents/Uni/MasterThesis/R/review_data/reviews_all_raw_4.csv", 
                        header=FALSE,
                        stringsAsFactors = TRUE,
                        na.strings = c("NA","NaN"," "))  
# Or load csv files from source:
  #files <- list.files(pattern = '\\.csv')
  #tables <- lapply(files, read.csv, header = FALSE)
  #reviews.raw <- do.call(rbind, tables); as.data.frame(reviews.raw)
# -------------------------------------------
# Load Data ---------------------------
  # Identify and filter double entries
  reviews.raw <- unique(reviews.raw[,c(1:10)])#or 1-9
  # Annotate columns
  colnames(reviews.raw) = c("c_provider","c_title", "c_instructor", 
                      "c_status", "c_description", "c_date", 
                      "u_name", "u_status", "u_rating", 
                      "u_review")
  print(dim(reviews.raw))
  print(variable.names(reviews.raw))
  
# II.1- Reviews Dataset ---------------------------
  # Separate categorical variables from review text:
  # clean douplets, get rid of unnecesary variables
  reviews.data <- reviews.raw[,c(1:9)]
  # clean up variables cache
  reviews.data$c_description <- NULL
  reviews.data$c_status <- NULL
  # remove(reviews.raw,files,ables)
#-----------------------------------------------------------------------------------------------------
# II.2 Data Preprocessing ---------------------------
#-----------------------------------------------------------------------------------------------------
  reviews.data$c_provider <- tolower(reviews.data$c_provider)
  reviews.data$c_title <- tolower(reviews.data$c_title)
  reviews.data$c_title <- as.data.frame(reviews.data$c_title)
  reviews.data$c_instructor <- as.factor(tolower(reviews.data$c_instructor))
  reviews.data$c_date <- as.Date(reviews.data$c_date)
  reviews.data$c_date_year <- as.numeric(format(reviews.data$c_date, "%Y"))
  reviews.data$c_date_year <- data.matrix(reviews.data$c_date_year)
    # Plot date_year
    hist(reviews.data$c_date_year, freq = F, breaks = 7,
         xlab = "Reviews Count per Year",
         main = " ",
         col= c("cadetblue4","cadetblue3","cadetblue"),
         border = "white")
    abline(v = median(na.omit(reviews.data$c_date_year)), col = "red", lwd = 2)
    reviews.data$c_date_month <- as.numeric(format(reviews.data$c_date, "%m"))
    reviews.data$c_date_month <- data.matrix(reviews.data$c_date_month)
# II.3 Price ---------------------------  
require('psych')
  # Merge prices from url data, Lookup course.data$c_price
  reviews.data$c_price <- NA
  for(i in 1:nrow(reviews.data)){
    reviews.data$c_price[course.data$course_name[i] %in% reviews.data$c_title[i]] <- course.data$c_price[i]}
    reviews.data$c_price <- data.matrix(as.numeric(reviews.data$c_price))
    describe(reviews.data$c_price)
# II.4 Student Charcateristics ---------------------------  
  reviews.data$u_name <- gsub("\n","",reviews.data$u_name)  
  reviews.data$u_name <- tolower(reviews.data$u_name)
  # Anonymity
  reviews.data$u_anonym <- NA
  reviews.data$u_anonym <- ifelse(reviews.data$u_name == "student", 1, 0)
  print(summary(reviews.data$u_anonym))
  reviews.data$u_anonym.y <- ifelse(reviews.data$u_anonym == 1, 1, 0)
  reviews.data$u_anonym.n <- ifelse(reviews.data$u_anonym == 0, 1, 0)
# II.5 Status of Student ---------------------------  
  # c["Complete, Taking now, Dropped"] # 42% rated with 10 (n = 20), (tot n = 47)
  reviews.data$u_status <- as.data.frame(as.factor(unlist(reviews.data$u_status)))
  # Recode variables into Dummy variables
  reviews.data$u_status_drp <- ifelse(reviews.data$u_status == "dropped", 1, 0); data.matrix(reviews.data$u_status_drp)
  course.drp <- as.numeric(colSums(reviews.data$u_status_drp))
  print(course.drp/nrow(reviews.data)*100) # drop Out ratio = 0.51 [n = 165]
  reviews.data$u_status_tkn <- ifelse(reviews.data$u_status == "taking now", 1, 0); data.matrix(reviews.data$u_status_tkn)
  course.tkn <- as.numeric(colSums(reviews.data$u_status_tkn))
  print(course.tkn/nrow(reviews.data)*100) # Taking Now ratio = 2.909 [n = 931]
  reviews.data$u_status_cpm <- ifelse(reviews.data$u_status == "completed", 1, 0); data.matrix(reviews.data$u_status_cpm)
  course.cpm <- as.numeric(colSums(reviews.data$u_status_cpm))
  print(course.cpm/nrow(reviews.data)*100) #Completed ratio = 96.57 = [931]
  #-----------------------------------
  status.ratio <- as.table(summary(as.factor(unlist(reviews.data$u_status))))
  # Plot Student Status (class imbalance!)
  barplot(status.ratio[c(2:4)],
          main= " ", 
          xlab= "Status of the Student when review was written",
          col= c("grey","brown4","brown"),
          border="white")
# II.6 Rating of Student ---------------------------
  reviews.data$u_rating <- data.matrix(as.numeric(unlist(reviews.data$u_rating)))
  reviews.data$u_rating[reviews.data$u_rating=='NA'] <- NA
  reviews.data$u_rating[reviews.data$u_rating==' '] <- NA
  reviews.data$u_rating[is.na(reviews.data$u_rating)] <- median(reviews.data$u_rating) #replace NA with median 
  reviews.rating <- reviews.data$u_rating; data.matrix(reviews.rating)
  print(summary(reviews.rating))
  # Starting either from 1 or 0
  plot(density(na.omit(reviews.rating), 
                from = 0, to = 10, bw = 0.5), col="cadetblue",
        main = " ", xlab = "Rating extracted with the review \n (n=32072)")
  # Hartigans (dip.test) for multimodality
  # diptest::dip.test(as.numeric(reviews.data$u_rating), simulate=TRUE)
  #--------------------------------------------------------------------------------------------------  
  # Rescaling according to Fang2015 and Liu2012a approach    
  # Variable rescale - linear interpolation
  # newvalue= (max'-min')/(max-min)*(value-min)+min'
  #--------------------------------------------------------------------------------------------------  
  # II.7 Rating ---------------
  rating.range <- seq(1:5)
  rating.rescaled <- NA
  reviews.rating[is.na(reviews.rating)] <- median(na.omit(reviews.rating))
  # reviews.rating <- data.matrix(na.omit(reviews.rating))
  rescaling <- function(x){min(rating.range) + (((max(rating.range) - min(rating.range)) * (x - min(reviews.rating)))/(max(reviews.rating) - min(reviews.rating)))}
  rating.rescaled <- rescaling(reviews.rating)
  print(summary(rating.rescaled))
  remove(rating.range)
  # Plot rating and rescaled rating:
  plot(density(na.omit(reviews.data$u_rating), 
                from = 0, to = 10, bw = 0.4), col="cadetblue",
                main = " ", xlab = "Rating extracted with the review \n (n=32072)")
  lines(density(rating.rescaled, 
                 from = 1, to = 5, bw = 0.33), col="brown3")
  abline(v = mean(rating.rescaled), col = "brown3", lwd = 2)
  abline(v = mean(na.omit(reviews.data$u_rating)), col = "cadetblue", lwd = 2)
#--------------------------------------------------------------------------------------------------    
# II.8 Ground Truth ----------------
# Annotate polarity according to the starring distribution 
reviews.data$polarity <- NA
  for (i in 1:(nrow(rating.rescaled))){
      if (rating.rescaled[i] >= 4){
        reviews.data$polarity[i] <- "1"};
      if (rating.rescaled[i] <= 2){ # Mean = 
        reviews.data$polarity[i] <- "-1"}}
  reviews.data$polarity[is.na(reviews.data$polarity)] <- "0"
  
  reviews.data$polarity <- data.matrix(as.factor(unlist(reviews.data$polarity)))
  polarity.info <- summary(reviews.data$polarity) #starring method: polarity after rescale
  print(polarity.info)
# Polarity according to starring  
  reviews.data$polarity.pos <- ifelse(reviews.data$polarity == 1, 1, 0); reviews.data$polarity.pos <- as.numeric(reviews.data$polarity.pos)
  reviews.data$polarity.neutr <- ifelse(reviews.data$polarity == 0, 1, 0); reviews.data$polarity.neutr <- as.factor(reviews.data$polarity.neutr)
  reviews.data$polarity.neg <- ifelse(reviews.data$polarity == -1, 1, 0); reviews.data$polarity.neg <- as.factor(reviews.data$polarity.neg)
#----------------------------------------------------------------------------------------  
  print(variable.names(reviews.data))
  cor.set1 <- data.matrix(na.omit(reviews.data[,c(13:20)]))
  # Measure correlations between variables:
  # reviews.data.corr <- cor(na.omit(cor.set1))
  reviews.data.corr <- corstars(cor.set1)
  print(reviews.data.corr)
  #corrplot(reviews.data.corr, method = "number")
  # reviews.data.corr.test <- FindCorr(reviews.data.corr, cutoff = 0.35, verbose = FALSE)
#----------------------------------------------------------------------------------------  
# II.9 Experiments ---------------------------  
# Experiments: Investigating relation between status and rating and polarity
# Using Liu's Opinion Lexicon  
  experiments <- reviews.data[,c("u_status","u_rating","polarity")]
#---------------------------------------------  
# Experiment 1: Corrleation between STATUS & RATING
#---------------------------------------------  
  exp2 <- subset(na.omit(reviews.data[,c("u_status","u_rating")], 
                 subset = reviews.data$u_status == c("completed","taking now","dropped")))
  print(summary(exp2))
  exp2$u_rating <- as.numeric(unlist(exp2$u_rating))
  exp2$u_status <- ordered(as.factor(unlist(na.omit(exp2$u_status))), levels =c("completed","taking now","dropped"))
  boxplot(u_rating ~ u_status, data = na.omitexp2, 
          ylab = "Rating of the Course", 
          xlab = "Student Status")
  # One-way ANOVA
  model.exp2.aov <- aov(exp2$u_rating ~ exp2$u_status , data = exp2)
  print(summary(model.exp2.aov))
  print(model.exp2.aov$fitted.values)
  EtaSq(model.exp2.aov, 2)
  # Fitting a linear model with the average estimate of completed
  # The "proportion of variance explained" measure R2R2 for multiple 
  # regression has an ANOVA equivalent, (eta squared)
  model.exp2.lm <- lm(exp2$u_rating ~ exp2$u_status , data = exp2)
  print(summary(model.exp2.lm)) # Very poor
  #---------------------------------------------  
  # Mutual Information
  require('infotheo')
  #---------------------------------------------
  # Statistical Association
  # MI of anonymity
  # MI(anonym = 1, stat = polarity) = 0.004016574
  mutinformation(X = reviews.data[,c(13)], Y = reviews.data[,c(17)]) 
  # MI(anonym = 1, stat = drop) = 0.0006712691
  mutinformation(X = reviews.data[,c(13)], Y = reviews.data[,c(14)]) 
  # 0.0166727
  print(mutinformation(exp2$u_rating, exp2$u_status))
  # MI completion rate ~ polarity (0.006403662)
  mutinformation(X = reviews.data$polarity, Y = reviews.data$u_status)
  # (0.04918192)
  mutinformation(X = reviews.data$c_provider, Y = reviews.data$u_status)
  # (0.5180972)
  mutinformation(X = reviews.data$c_provider, Y = reviews.data$u_rating)
  
  reviews.data$u_status <- ifelse(reviews.data$u_status == "dropped", -1, 
                                  ifelse(reviews.data$u_status == "completed", 1, 0))
#--------------------------------------------------------------------------------------------------
#                               PART III: TEXT NORMALZATION
#--------------------------------------------------------------------------------------------------
# III.1- Load Text---------------------------  
  reviews.text <- as.data.frame(reviews.raw[,c(10)]); colnames(reviews.text) <- c("text.raw")
  reviews.text$text.raw <- as.character(unlist(reviews.text$text.raw))
# Cleaning out URL's
  url.pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  reviews.text$text.raw <- gsub(pattern = url.pattern, replacement = " ", x = reviews.text$text.raw)
# Import some features from reviews.cat ------------------------------------------------------
  reviews.text$polarity <- data.matrix(as.numeric(reviews.data$polarity)) 
  colnames(reviews.text$polarity) <- "ground.thruth"
  print(summary(na.omit(reviews.text$polarity)))
  # Dropped Out or not [Focus on drop-out | Watch out class imbalance!]
  reviews.text$stat_dropp <- data.matrix(reviews.data$u_status_drp)
  print(summary(as.factor(reviews.text$stat_dropp)))
  # Ratings (rescaled) [1-5]
  reviews.text$rating <- data.matrix(rating.rescaled)    
  reviews.text$rating[is.na(reviews.text$rating)] <- mean(reviews.text$rating)
  print(summary(reviews.text$rating))
#------------------------------------------------------------------------------------------------------  
#   Quantitative Text Analysis
#------------------------------------------------------------------------------------------------------  
# III.2 Quantitative Text Analytics ---------------------------  
  require('quanteda')
  # Word Count
  reviews.text$wc <- NA
  reviews.text$wc <- sapply(reviews.text$text.raw, function(x) length(unlist(strsplit(as.character(x), "\\W+")))); reviews.text$wc <- data.matrix(reviews.text$wc)
  # Sentence Count
  reviews.text$sentc <- NA
  reviews.text$sentc <- sapply(reviews.text$text.raw, function(x) length(gregexpr('[[:alnum:] ][.!?]', as.character(x))[[1]] > 0)); reviews.text$sentc <- data.matrix(reviews.text$sentc)
  
  reviews.text$pos.words.ratio <- reviews.text$pos.words/reviews.text$sentc; reviews.text$pos.words.ratio <- as.numeric(reviews.text$pos.words.ratio)
  reviews.text$neg.words.ratio <- reviews.text$neg.words/reviews.text$sentc; reviews.text$neg.words.ratio <- as.numeric(reviews.text$neg.words.ratio)
  # Word/Sentence Ratio
  reviews.text$wps <- NA
  reviews.text$wps <- reviews.text$wc/reviews.text$sentc; reviews.text$wps <- data.matrix(reviews.text$wps)
  # Punctutaion Count (!)
  reviews.text$punct.excl <- NA
  reviews.text$punct.excl <- str_count(as.character(reviews.text$text.raw), "!"); reviews.text$punct.excl <- data.matrix(as.numeric(reviews.text$punct.excl))
  # Punctuation Count  
  reviews.text$punct <- NA
  reviews.text$punct <- sapply(reviews.text$text.raw, function(x) length(gregexpr("[[:punct:]]", as.character(x))[[1]] >= 0)); reviews.text$punct <- data.matrix(as.numeric(reviews.text$punct))
  reviews.text$text.raw <- gsub("[[:punct:]]"," ", reviews.text$text.raw)
  # CapsLock Count
  reviews.text$case <- NA 
  reviews.text$case <- sapply(reviews.text$text.raw, function(x) length(gregexpr("\\b[A-Z]{2,}\\b", as.character(x))[[1]] > 0)); reviews.text$case <- as.matrix(as.numeric(reviews.text$case))
  reviews.text$text.raw <- tolower(reviews.text$text.raw)
  # Negation Count
  require('stringr')
  # Opinions shifters - Negations  
  # See Liu2012 # <not, never, none, nobody, nowhere, neither and cannot> 
  reviews.text$neg <- str_count(as.character(reviews.text$text.raw), "(not|n't|none|nobody|nowhere|neither|cannot|no|never)") 
  reviews.text$neg <- data.matrix(as.numeric(reviews.text$neg))
  # Negations/Sentence Ratio  
  reviews.text$neg.ratio <- reviews.text$neg/reviews.text$sentc; reviews.text$neg.ratio <- data.matrix(reviews.text$neg.ratio)
  # But Clauses - Sentence level (see exception "not only but also")
  # reviews.text$but_cl <- str_count(as.character(reviews.text$text.raw), "(but)")
  # reviews.text$but_cl <- data.matrix(as.numeric(reviews.text$but_cl))
# Plotting
  # hist(reviews.text$sentc, 
  #      freq = F, 
  #      main = " ", xlab = "Number of sentences",
  #      col=c("cadetblue","cadetblue4"),
  #      border = "white")
  # neg.words.ratio
  plot.new(density(na.omit(reviews.text$neg.words.ratio), 
               from = -1.5, to = 15, 
               bw = 0.5), col="brown3", main=" ", xlab="N = 31068 ")
  abline(v = mean(na.omit(reviews.text$neg.words.ratio)), col = "brown3", lwd = 1)
  # pos.words.ratio
  lines(density(na.omit(reviews.text$pos.words.ratio), 
                from = -1, to = 15, bw = 0.5), col="cadetblue")
  abline(v = mean(na.omit(reviews.text$pos.words.ratio)), col = "cadetblue", lwd = 1)
  # Sentence C
  lines(density(na.omit(reviews.text$sentc), 
                from = -1, to = 15, bw = 0.65), col="black")
  #abline(v = mean(na.omit(reviews.text$sentc)), col = "darkgrey", lwd = 2)
  # Word C
  lines(density(na.omit(reviews.text$wc), 
                from = -1, to = 15, bw = 0.6), col="grey")
  #abline(v = mean(reviews.text$wc), col = "grey", lwd = 2)
  # Word/Sentence Ratio
  lines(density(na.omit(reviews.text$wps), 
                from = -1, to = 20, bw = 0.6), col="darkgreen")
  #abline(v = mean(reviews.text$wps), col = "darkgreen", lwd = 1)
#------------------------------------------------------    
#  Quantitative Text Analysis
#------------------------------------------------------  
  # qta <- corstars(reviews.text[,c(2:14)])
  # print(qta)
  # reviews.text.bup <- reviews.text
  # # Sort out outliers #Q3=67
  # reviews.text <- subset(reviews.text.bup, reviews.text.bup$wps <= 67) 
#------------------------------------------------------ 
#                   POLARITY annotation  
#------------------------------------------------------ 
# Method 1: Opinion Lexicon  
# Count Positive and Negative Words (B.Liu 2012)/(Fang 2015)
# http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
#-------------------------------------------------------------------  
# III.3 Identify Reviews containing Sentiment Sentences ------------------ 
# Identify Reviews with Sentiment Words
  p.file <- file("positive-words.txt", "r")
  pos.lexicon <- readLines(p.file)
  reviews.text$pos.words <- vapply(reviews.text$text.raw, function(x) sum(stri_count_fixed(x, pos.lexicon)), 1L)
  reviews.text$pos.words <- data.matrix(reviews.text$pos.words)
  colnames(reviews.text$pos.words) <- "lexical.pol.pos.count"
  n.file <- file("negative-words.txt", "r")
  neg.lexicon <- readLines(n.file)
  reviews.text$neg.words <- vapply(reviews.text$text.raw, function(x) sum(stri_count_fixed(x, neg.lexicon)), 1L)
  reviews.text$neg.words <- data.matrix(reviews.text$neg.words)
  colnames(reviews.text$neg.words) <- "lexical.pol.neg.count"
  # Count total Opinions Words
  reviews.text$tot.opinion.words <- data.matrix(reviews.text$pos.words + reviews.text$neg.words)
  
# Polarity Classification  
# Sentiment Score = (- NEG + POS) used by Liu 2012 and Fang 2015
# Review lexical Score
  reviews.text$lexical.polarity <- data.matrix(reviews.text$pos.words - reviews.text$neg.words)
  reviews.text$lexical.polarity[is.na(reviews.text$lexical.polarity)] <- median(reviews.text$lexical.polarity)
  colnames(reviews.text$lexical.polarity) <- "lexical.polarity.tp"
  #reviews.text$lexical.polarity.ps <- data.matrix(reviews.text$lexical.polarity/reviews.text$sentc)
  #reviews.text$lexical.ratio.polarity <- reviews.text$pos.words.ratio - reviews.text$neg.words.ratio
  # Classifier 1
  reviews.text$lex.pol <- ifelse(reviews.text$lexical.polarity > 0, 1,
                                  ifelse(reviews.text$lexical.polarity < 0, -1,0))
#-------------------------------------------------------------------  
# Method 2: Sentiment R Package - Bayes Method
# III.4 Polarity Annotation ---------------------------      
#-------------------------------------------------------------------  
# NOT SELECTED - as Opinion Lexicon performs much better
#------------------------------------------------------------------  
  # Wiebel QALP lexicon 
  # method = bayes
#   require('sentiment')
#   sent.bayes <- classify_polarity(opinion.lexicon$text.raw, algorithm= "bayes")
# #  sent.voting <- classify_polarity(opinion.lexicon$text.raw)
#   # Classifier 2 Bayes
#   opinion.lexicon$sent_bayes_score <- data.matrix(sent.bayes[,3])
#   opinion.lexicon$sent_bayes_score <- as.numeric(opinion.lexicon$sent_bayes_score)
#   opinion.lexicon$sent_bayes_char <- sent.bayes[,4]
#   opinion.lexicon$sent_bayes <- ifelse(opinion.lexicon$sent_bayes_char=="positive", 1, 
#                                        ifelse(opinion.lexicon$sent_bayes_char=="negative", -1, 0))
#   opinion.lexicon$sent_bayes <- data.matrix(opinion.lexicon$sent_bayes)
#   opinion.lexicon$sent_bayes_char <- NULL
# 
# # Plotting the different polairty measurements
#   rev.pol <- opinion.lexicon[,c(2,4,7,8,9,10)]
#   #pol.cor <- round(cor(na.omit(rev.pol)))
#   pol.cor <- corstars(rev.pol)
#   print(polarity.cor)
  # p.cor <- corr.p(pol.cor, nrow(reviews.text))
  # colnames(pol.cor) <- c("rating", "ground.truth", "lexical.pol.tp","stat.pol.tf")
  # rownames(pol.cor) <- c("rating", "ground.truth", "lexical.pol.tp","stat.pol.tf")
#-------------------------------------------------------------------------------------------
#                       POS Tagging
#-------------------------------------------------------------------------------------------  
# POS Tagging ---------------------------  
require("openNLP","NLP")
library("parallel")
tagPOS <-  function(x, ...){
    s <- as.String(x)
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- NLP::Annotation(1L, "sentence", 1L, nchar(s))
    a2 <- NLP::annotate(s, word_token_annotator, a2)
    a3 <- NLP::annotate(s, Maxent_POS_Tag_Annotator(), a2) #sentence
    a3w <- a3[a3$type == "word"] #diff between sentence and word
    POStags <- unlist(lapply(a3w$features, '[[', "POS"))
    # POStagged <- paste(sprintf("%s/%s", s[a3w], POStags == "NN"), collapse = " ")
    POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
    #    list(POStagged=POStagged, POStags=POStags)
    list(POStagged=POStagged)
}
# Extracting Opinion Words/Phrases  
extractPOS <- function(x, thisPOSregex) {
    x <- as.String(x)
    wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
    POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
    POSwords <- subset(POSAnnotation, type == "word")
    tags <- sapply(POSwords$features, '[[', "POS")
    thisPOSindex <- grep(thisPOSregex, tags)
    tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
    untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
    untokenizedAndTagged}
# Parallel processing
  # text.var <- as.data.frame(reviews.text$text.raw)
  # cl <- makeCluster(mc <- getOption("cl.cores", detectCores()/2))
  # clusterEvalQ(cl, {
  #   library(openNLP)
  #   library(NLP)
  #   PTA <- Maxent_POS_Tag_Annotator()})
  # m <- parLapply(cl, text.var, tagPOS)
  # print(m)
  # stopCluster(cl)
#-------------------------------------------------------------------------------------------
#                       CORPORA
#-------------------------------------------------------------------------------------------  
require('tm','openNLP','NLP')
require('RTextTools')
# III.5 Feature vector (DTM) ---------------------------
  # ALL
  corpus.tmp <- as.data.frame(reviews.text$text.raw)
  corpus.tmp <- Corpus(DataframeSource(corpus.tmp))
  # Corpus processing
  reviews.corpus <- tm_map(corpus.tmp, content_transformer(tolower))
  reviews.corpus <- tm_map(reviews.corpus, removeNumbers)
  reviews.corpus <- tm_map(reviews.corpus, removePunctuation)
  reviews.corpus <- tm_map(reviews.corpus, function(x) removeWords(x, stopwords("english")))
  #reviews.corpus <- tm_map(reviews.corpus, stemDocument, language = "english")
  # Corpus convertion to DTM -------------------
  reviews.dtm <- DocumentTermMatrix(reviews.corpus, 
                                    control = list(
                                    removePunctuation = TRUE,
                                    removeWords = TRUE,
                                    stopwords(kind = "english"),
                                    stripWhitespace = TRUE,
#                                    stemming = TRUE,
                                    removeSparseTerms = TRUE))    
  reviews.dtm.xs <- removeSparseTerms(reviews.dtm, sparse = 0.999) # sparsity = 94%, max = 10, docs = 32244, terms = 233, Non-/sparse entries =  420788/7092064 (5.9)
  print(head(inspect(reviews.dtm.xs)))
  print(dim(reviews.dtm.xs))
  #------------------------------
  # Measure Term Frequency [TF(w)]
  dtm.xs <- as.matrix(reviews.dtm.xs)
  word.freq <- as.data.frame(colSums(dtm.xs)); colnames(word.freq) <- "freq"; 
  word.freq$word <- rownames(word.freq)
  # Measure the relative frequency [TF/N]
  word.freq$prob <- word.freq$freq/nrow(word.freq) # Relative
  # ITIDF ---------------------------  
  # itidf2 <-DocumentTermMatrix(dtm.xs,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))  
  tf <- dtm.xs
  idf <- log(nrow(dtm.xs)/colSums(dtm.xs))
  word.freq$idf <- idf
  tfidf <- dtm.xs
  for(word in names(idf)){
    tfidf[,word] <- tf[,word] * idf[word]
  }
  word.freq$tfidf <- colSums(tfidf)/nrow(tfidf)
  word.freq <- word.freq[with(word.freq, order(tfidf, decreasing = TRUE)),]
  rownames(word.freq) <- word.freq$word
  # Experiments -------------------------------------------
  require(tm)
  # N_words = 1931 (no stpwords)
  freq.terms <- findFreqTerms(reviews.dtm.xs, 1500)
  #Finding associated words to relevant words also used by Adam2013
  assoc.instr <- findAssocs(reviews.dtm.xs, "instructor", 0.05)
  assoc.mat <- findAssocs(reviews.dtm.xs, "materials", 0.05)
  assoc.assig <- findAssocs(reviews.dtm.xs, "assignments", 0.05)
#-------------------------------------------------------------------------------  
# POS annotation
#-------------------------------------------------------------------------------  
# POS Annotation ---------------------------  
  POS <- tagPOS(word.freq$word)
  POS_all <- as.data.frame(stri_split_regex(POS, " "))
  word.freq$POStmp <- POS_all[1:nrow(POS_all),]
  word.freq$POS <- lapply(strsplit(as.character(word.freq$POStmp), "/" ), "[", 2) 
  word.freq$POS <- as.data.frame(unlist(word.freq$POS)); colnames(word.freq$POS) <- "POS"
  # POS statistics
    POS.ratio <- as.data.frame(as.data.frame(summary(word.freq$POS))[,3]); colnames(POS.ratio) <- "POS.freq"
    print(POS.ratio)
  # Clean
  word.freq$POStmp <- NULL
  remove(POS,POS_all)
  # Lookup most common words according with their POS
  #-------------------------------------------------------------------------
  # POS
  #-------------------------------------------------------------------------
  NN <- subset(word.freq[,c("tfidf","word","POS")], grepl("NN", word.freq$POS[,1], fixed = FALSE)); NN <- as.data.frame(NN)
  NN <- NN[with(NN, order(tfidf, decreasing = TRUE)),]
  
  JJ <- subset(word.freq[,c("tfidf","word","POS")], grepl("JJ", word.freq$POS[,1], fixed = FALSE)); JJ <- as.data.frame(JJ)
  JJ <- JJ[with(JJ, order(tfidf, decreasing = TRUE)), ]
  RB <- subset(word.freq[,c("tfidf","word","POS")], grepl("RB", word.freq$POS[,1], fixed = FALSE)); RB <- as.data.frame(RB)
  RB <- RB[with(RB, order(tfidf, decreasing = TRUE)), ]
  VB <- subset(word.freq[,c("tfidf","word","POS")], grepl("VB", word.freq$POS[,1], fixed = FALSE)); RB <- as.data.frame(RB)
  VB <- VB[with(VB, order(tfidf, decreasing = TRUE)), ]
  
# III.6 Opinions Words ---------------------------    
  opinion.lexicon.pos <- rbind(JJ,RB); opinion.lexicon.pos <- rbind(opinion.lexicon.pos,VB)
  opinion.lexicon.pos <- as.data.frame(opinion.lexicon.pos); op.lex <- opinion.lexicon.pos[,2]
  emotion.lexicon <- as.character(op.lex);  emo.lex <- as.character(strsplit(emotion.lexicon, " "))
  
  reviews.text$text.raw <- tolower(reviews.text$text.raw)
  # Extract Opinion Words 
  for(i in (1:nrow(reviews.text))){
    reviews.text$BoW[i] <- as.character(strsplit(reviews.text$text.raw[i], " "))
    reviews.text$emo.words[i] <- list(as.vector(as.character(unlist(as.vector(na.omit(as.character(str_match(reviews.text$BoW[i], emo.lex))))))))
    }
  # create opinion DTM
  require('RTextTools','tm') 
  emo.words <- reviews.text[,c("emo.words","lexical.polarity","lex.pol")]
  emo.corpus.tmp <- as.vector(emo.words$emo.words)
  # VCorpus = Virtual Corpus
  emo.corpus <- VCorpus(VectorSource(emo.corpus.tmp))
  # Corpus convertion to DTM -------------------
  num.cores <- getOption("mc.cores")
  options(mc.cores=1)
  emo.dtm <- DocumentTermMatrix(emo.corpus, 
                                    control = list(
                                      removePunctuation = TRUE,
                                      removeWords = TRUE,
                                      stopwords(kind = "english"),
                                      stripWhitespace = TRUE
                                      #stemming = TRUE,
                                      #removeSparseTerms = TRUE))    
                                    ))
  options(mc.cores=num.cores)
#  emo.dtm.xs <- removeSparseTerms(emo.dtm, sparse = 0.999) # sparsity = 94%, max = 10, docs = 32244, terms = 233, Non-/sparse entries =  420788/7092064 (5.9)
  print(head(inspect(emo.dtm)))
  print(dim(emo.dtm))
# Terms Clustering ---------------------------    
    require(graphics)
    nn <- as.data.frame(NN[1:55,])
    nn.m <- as.matrix(nn$tfidf); rownames(nn.m) <- rownames(nn)
    d <- dist(nn.m, method="euclidian")   
    fit <- hclust(d=d, method="ward.D")   
    print(fit)
    plot(fit, hang = -1)
    
    plot.new()
    plot(fit, hang = -1, xlab = "Terms Clustering")
    groups <- cutree(fit, k=10)   # "k=" defines the number of clusters you are using   
    rect.hclust(fit, k=10, border="red")
#-------------------------------------------------------------------------
# STATUS
#-------------------------------------------------------------------------  
  require('RTextTools','tm')
    #Check how many "dropped" are affected
    ratings.dropped <- as.data.frame(reviews.text$text.raw[reviews.text$stat_dropp==1])
    corpus.dropped <- Corpus(DataframeSource(ratings.dropped))  
    num.cores <- getOption("mc.cores")
    options(mc.cores=1)
    drop.dtm <- DocumentTermMatrix(corpus.dropped, 
                                          control = list(
                                            removePunctuation = TRUE,
                                            removeWords = TRUE,
                                            stopwords(kind = "english"),
                                            stripWhitespace = TRUE))
    
  word.freq.drop <- as.data.frame(colSums(as.matrix(drop.dtm))); colnames(word.freq.drop) <- "freq"
  word.freq.drop$word <- rownames(word.freq.drop)
  word.freq.drop$prob <- word.freq.drop$freq/nrow(word.freq.drop)
  drop.mat <- as.matrix(drop.dtm) 
  tf <- drop.mat; 
  idf <- log(nrow(drop.mat)/colSums(drop.mat))
  word.freq.drop$idf <- idf; 
  tfidf <- drop.mat
  for(word in names(idf)){
    tfidf[,word] <- tf[,word] * idf[word]
  }
  word.freq.drop$tfidf <- colSums(tfidf)/nrow(tfidf)
  word.freq.drop <- word.freq.drop[with(word.freq.drop, order(tfidf, decreasing = TRUE)),]
  rownames(word.freq.drop) <- word.freq.drop$word  
  
  POS <- tagPOS(word.freq.drop$word)
  POS_all <- as.data.frame(stri_split_regex(POS, " "))
  word.freq.drop$POStmp <- POS_all[1:nrow(POS_all),]
  word.freq.drop$POS <- lapply(strsplit(as.character(word.freq.drop$POStmp), "/" ), "[", 2) 
  word.freq.drop$POS <- as.data.frame(unlist(word.freq.drop$POS)); colnames(word.freq.drop$POS) <- "POS"
  word.freq.drop$POStmp <- NULL
  remove(POS,POS_all)
  
  NN.drop <- subset(word.freq.drop[,c("tfidf","word","POS")], grepl("NN", word.freq.drop$POS[,1], fixed = FALSE)); NN.drop <- as.data.frame(NN.drop)
  NN.drop <- NN.drop[with(NN.drop, order(tfidf, decreasing = TRUE)),]
  
  JJ.drop <- subset(word.freq[,c("tfidf","word","POS")], grepl("JJ", word.freq$POS[,1], fixed = FALSE)); JJ <- as.data.frame(JJ)
  JJ.drop <- JJ[with(JJ, order(tfidf, decreasing = TRUE)), ]
  # Disengagemnt Clustering ---------------------------    
  require(graphics)
  nn.d <- as.data.frame(NN.drop[1:30,])
  nn.dm <- as.matrix(nn.d$tfidf); rownames(nn.dm) <- rownames(nn.d)
  d <- dist(nn.dm, method="euclidian")   
  fit <- hclust(d=d, method="ward.D")   
  print(fit)
  plot(fit, hang = -1)
  
  plot.new()
  plot(fit, hang = -1, xlab = "Terms Clustering")
  groups <- cutree(fit, k=8)   # "k=" defines the number of clusters you are using   
  rect.hclust(fit, k=8, border="red")
  
#------------------------------
# EXPERIMENTS: NEGATIVE-CORPUS  
#------------------------------
  require(openNLP)
  corpus.tmp.neg <- as.data.frame(subset(reviews.text$text.raw, reviews.text$lex.pol == -1))
  reviews.corpus.neg <- Corpus(DataframeSource(corpus.tmp.neg))
  num.cores <- getOption("mc.cores")
  options(mc.cores=1)
  reviews.neg.dtm <- DocumentTermMatrix(reviews.corpus.neg, 
                                        control = list(
                                          removePunctuation = TRUE,
                                          removeWords = TRUE,
                                          stopwords(kind = "english"),
                                          stripWhitespace = TRUE))
  word.freq.neg <- as.data.frame(colSums(as.matrix(reviews.neg.dtm))); colnames(word.freq.neg) <- "freq"
  word.freq.neg$word <- rownames(word.freq.neg)
  word.freq.neg$prob <- word.freq.neg$freq/nrow(word.freq.neg)
  neg.mat <- as.matrix(reviews.neg.dtm) 
  tf <- neg.mat; 
  idf <- log(nrow(neg.mat)/colSums(neg.mat))
  word.freq.neg$idf <- idf; 
  tfidf <- neg.mat
  for(word in names(idf)){
    tfidf[,word] <- tf[,word] * idf[word]
  }
  word.freq.neg$tfidf <- colSums(tfidf)/nrow(tfidf)
  word.freq.neg <- word.freq.neg[with(word.freq.neg, order(tfidf, decreasing = TRUE)),]
  rownames(word.freq.neg) <- word.freq.neg$word  
  
  POS <- tagPOS(word.freq.neg$word)
  POS_all <- as.data.frame(stri_split_regex(POS, " "))
  word.freq.neg$POStmp <- POS_all[1:nrow(POS_all),]
  word.freq.neg$POS <- lapply(strsplit(as.character(word.freq.neg$POStmp), "/" ), "[", 2) 
  word.freq.neg$POS <- as.data.frame(unlist(word.freq.neg$POS)); colnames(word.freq.neg$POS) <- "POS"
  word.freq.neg$POStmp <- NULL
  remove(POS,POS_all)
  
  NN.neg <- subset(word.freq.neg[,c("tfidf","word","POS")], grepl("NN", word.freq.neg$POS[,1], fixed = FALSE)); NN.neg <- as.data.frame(NN.neg)
  NN.neg <- NN.neg[with(NN.neg, order(tfidf, decreasing = TRUE)),]
  
  # Disengagemnt Clustering ---------------------------    
  require(graphics)
  nn.n <- as.data.frame(NN.neg[1:30,])
  nn.nm <- as.matrix(nn.d$tfidf); rownames(nn.nm) <- rownames(nn.d)
  d <- dist(nn.dm, method="euclidian")   
  fit <- hclust(d=d, method="ward.D")   
  print(fit)
  plot(fit, hang = -1)
  
  plot.new()
  plot(fit, hang = -1, xlab = "Terms Clustering")
  groups <- cutree(fit, k=8)   # "k=" defines the number of clusters you are using   
  rect.hclust(fit, k=8, border="blue")
  
  word.freqs.neg <- findFreqTerms(reviews.neg.dtm, lowfreq = 300, highfreq = 600)
  print(word.freqs.neg)
  word.assoc.neg <- findAssocs(reviews.neg.dtm, word.freq.neg$word, corlimit = 0.95)
#-----------------------------------------  
# Cluster
#-----------------------------------------  
  library(cluster)   
  require(graphics)
  nn <- as.data.frame(JJ[1:30,])
  nn.m <- as.matrix(nn$tfidf); rownames(nn.m) <- rownames(nn)
  d <- dist(nn.m, method="euclidian")   
  fit <- hclust(d=d, method="ward.D")   
  print(fit)
  plot(fit, hang = -1)
  
  plot.new()
  plot(fit, hang = -1, xlab = "Terms Clustering")
  groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
  rect.hclust(fit, k=6, border="red")
  
#------------------------------------------------------------------------------------------------------  
#   Modelling & Evaluation
#------------------------------------------------------------------------------------------------------  
# Modelling and Evaluation ---------------------------  
  require('e1071')  
  require('caret')  
  # Review-level-classification
  # Also possible to 60 train -  20 test - 20 cross-validation
  # Training with library(e1071)
  # III.7 Polarity Classification ------------------------------
  require('RTextTools')
  emo.mt <- as.matrix(emo.dtm)
  # Naive Bayes
  # training data (80%), testing (20%)
  classifier.nB <- naiveBayes(emo.mt[1:25654,], as.factor(reviews.text$lex.pol[1:25654]))
  predicted <- predict(classifier.nB, emo.mt[25655:nrow(emo.mt),]) 
  predicted
  table(reviews.text$polarity[25655:nrow(reviews.text)], predicted)
  recall_accuracy(reviews.text$polarity[25655:nrow(reviews.text)], predicted)
  
  container <- create_container(emo.dtm, as.numeric(as.factor(na.omit(reviews.text$lex.pol))),
                                trainSize=1:25654, testSize=25655:nrow(reviews.text),
                                virgin=TRUE)
  models <- train_models(container, algorithms=c("SVM"))#"RF"
  
  # III.8 Evaluation of Classifiers
  # Evaluation of polarity annotation ----------------------------
  # Subset of positive and negative
  eval.set <- rev.pol[,c(1,3,4,6,5)]
  # Convert to binary classification
  eval.set <- subset(eval.set, eval.set$polarity != 0 & 
                       eval.set$lex.pol != 0 &
                       eval.set$polarity_bayes != 0)
  # Confusion matrices
  conf.matrix.liu <- count(eval.set, vars = c("polarity","lex.pol"))
  conf.matrix.bayes <- count(eval.set, vars = c("polarity","polarity_bayes"))
  # Evaluate
  require('ROCR')
  y <- eval.set$polarity_bayes
  predictions.liu <- eval.set$lexical.polarity
  pred.liu <- prediction(na.omit(predictions.liu), y)
  perf.roc.liu <- performance(pred.liu, "tpr", "fpr")
  perf.test.liu <- performance(pred.liu, "cost")
  perf.prec.liu <- performance(pred.liu, "prec", "rec")
  # Lift
  plot(perf.lift.liu, col="darkgrey")
  abline(a=0, b= 1)
  
  y <- eval.set$lex.pol
  predictions.bayes <- as.matrix(eval.set$polarity_bayes_score)
  pred.bayes <- prediction(na.omit(predictions.bayes), y)
  perf.roc.bayes <- performance(pred.bayes, "tpr", "fpr")
  perf.prec.bayes <- performance(pred.bayes, "prec", "rec")
  # ROC
  plot(perf.roc.liu, col="cadetblue"); abline(a=0, b= 1)
  plot(perf.roc.bayes, col="brown3"); abline(a=0, b= 1)
  # Precision / recall
  plot(perf.prec.liu, col="cadetblue")
  plot(perf.prec.bayes, col="brown3")
  # Experiment: Limiting FPR
  pROC = function(pred, fpr.stop){
    perf <- performance(pred, "tpr", "fpr")
    for (iperf in seq_along(perf@x.values)){
      ind = which(perf@x.values[[iperf]] <= fpr.stop)
      perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
      perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]}
    return(perf)}
  proc.perf = pROC(pred.liu, fpr.stop=0.3)
  plot(proc.perf)
  abline(a=0, b= 1)
  #------------------------------------------------------ 
  #                   SENTENCE annotation  
  #------------------------------------------------------ 
  # Text Normalization Text [Sentence] ---------------------------  
  # require(syuzhet) 
  # review.sentences
  # 113870 sentences - ALL Sentences
  # review.sentences <- get_sentences(as.String(reviews.text$text.raw))
  # Emotion Sentences ()
  # emotion.sentences <- classify_polarity(review.sentences)
  # rev.sentences.df <- as.data.frame(review.sentences)
  # rev.sentences.df$polarity <- classify_polarity(rev.sentences.df)
  # reviews.sentence.corpus <- Corpus(VectorSource(review.sentences))
  # #reviews.corpus <- tm_map(reviews.corpus, removeSparseTerms)
  #------------------------------
  # Corpus convertion to DTM
  # sentence.dtm <- DocumentTermMatrix(reviews.sentence.corpus, 
  #                                   control = list(
  #                                     removePunctuation = TRUE,
  #                                     removeWords = TRUE,
  #                                     stopwords(kind = "english"),
  #                                     stripWhitespace = TRUE,
  #                                     stemming = TRUE,
  #                                     removeSparseTerms = TRUE))    
  # sentence.dtm.xs <- removeSparseTerms(sentence.dtm, sparse = 0.999)
  
  
