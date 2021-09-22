rm(list=ls())

library(textclean)
library(katadasaR)
library(tokenizers)
library(wordcloud)
library(dplyr)
library(rtweet)
library(stopwords)
library(tokenizers)
library(data.table)


token <- create_token(
  app = "rtweet library study",
  consumer_key = "FsIOFAPVd4GpiApy5pWTvQTyZ",
  consumer_secret = "h534T8QqhMwkk6880OsR77ZArUUjxulkT6NxNeoNpQTPGlZEA9",
  access_token = "1436210707519868943-bS6R2W3AZVoyLbIpWfZE5vJuuHTN17",
  access_secret = "sVdg8kyFVsKaTbtc86NpVi6bAvPSKrr2EEDkUiGTeIt9M")

smartfrenTweets <- search_tweets(
  "smartfren", n=2000, include_rts = FALSE, retryonratelimit = F
)
telkomselTweets <- search_tweets(
  "telkomsel", n=2000, include_rts = FALSE, retryonratelimit = F
)
indosatTweets <- search_tweets(
  "@IndosatOoredoo", n=2000, include_rts = FALSE, retryonratelimit = F
)
indosatTweets2 <- search_tweets(
  "indosat", n=2000, include_rts = FALSE, retryonratelimit = F
)
indosatTweets3 <- search_tweets(
  "ooredoo", n=2000, include_rts = FALSE, retryonratelimit = F
)
xlTweets <- search_tweets(
  "@myxl", n=2000, include_rts = FALSE, retryonratelimit = F
)
xlTweets2 <- search_tweets(
  "kartu xl", n=2000, include_rts = FALSE, retryonratelimit = F
)
xlTweets3 <- search_tweets(
  "operator xl", n=2000, include_rts = FALSE, retryonratelimit = F
)
triTweets <- search_tweets(
  "@triindonesia", n=2000, include_rts = FALSE, retryonratelimit = F
)
triTweets2 <- search_tweets(
  "kartu tri", n=2000, include_rts = FALSE, retryonratelimit = F
)
triTweets3 <- search_tweets(
  "kartu 3", n=2000, include_rts = FALSE, retryonratelimit = F
)
triTweets4 <- search_tweets(
  "operator tri", n=2000, include_rts = FALSE, retryonratelimit = F
)



# Binding the dataframes
indosatTweets <- rbind(indosatTweets, indosatTweets2, indosatTweets3)
xlTweets <- rbind(xlTweets, xlTweets2, xlTweets3)
triTweets <- rbind(triTweets, triTweets2, triTweets3, triTweets4)

# smartfrenTweets = read.csv("smartfren_tweets_raw.csv")
# telkomselTweets = read.csv("telkomsel_tweets_raw.csv")
# indosatTweets = read.csv("indosat_tweets_raw.csv")
# xlTweets = read.csv("xl_tweets_raw.csv")
# triTweets = read.csv("tri_tweets_raw.csv")

smartfrenTweets <- smartfrenTweets$text %>% 
  as.character()
telkomselTweets <- telkomselTweets$text %>% 
  as.character()
indosatTweets <- indosatTweets$text %>% 
  as.character()
xlTweets <- xlTweets$text %>% 
  as.character()
triTweets <- triTweets$text %>% 
  as.character()

######################## Pre-processing ############################################

# Replacing space between lines with space between words
smartfrenTweets <- gsub( "\n"," ",smartfrenTweets)
telkomselTweets <- gsub( "\n"," ",telkomselTweets)
indosatTweets <- gsub( "\n"," ",indosatTweets)
xlTweets <- gsub( "\n"," ",xlTweets)
triTweets <- gsub( "\n"," ",triTweets)

# Replace HTML and URLs
smartfrenTweets <- smartfrenTweets %>% 
  replace_html() %>% # replace html with blank 
  replace_url()   # replace URLs with blank
telkomselTweets <- telkomselTweets %>% 
  replace_html() %>% # replace html with blank 
  replace_url()   # replace URLs with blank
indosatTweets <- indosatTweets %>% 
  replace_html() %>% # replace html with blank 
  replace_url()   # replace URLs with blank
xlTweets <- xlTweets %>% 
  replace_html() %>% # replace html with blank 
  replace_url()   # replace URLs with blank
triTweets <- triTweets %>% 
  replace_html() %>% # replace html with blank 
  replace_url()   # replace URLs with blank

# Replace Emoticons and Emojis
smartfrenTweets <- smartfrenTweets %>% 
  replace_emoji(.) %>% 
  replace_html(.)
telkomselTweets <- telkomselTweets %>% 
  replace_emoji(.) %>% 
  replace_html(.)
indosatTweets <- indosatTweets %>% 
  replace_emoji(.) %>% 
  replace_html(.)
xlTweets <- xlTweets %>% 
  replace_emoji(.) %>% 
  replace_html(.)
triTweets <- triTweets %>% 
  replace_emoji(.) %>% 
  replace_html(.)

# Replace mentions and hashtags
smartfrenTweets <- smartfrenTweets %>% 
  replace_tag(smartfrenTweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(smartfrenTweets, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags

telkomselTweets <- telkomselTweets %>% 
  replace_tag(telkomselTweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(telkomselTweets, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags

indosatTweets <- indosatTweets %>% 
  replace_tag(indosatTweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(indosatTweets, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags

xlTweets <- xlTweets %>% 
  replace_tag(xlTweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(xlTweets, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags

triTweets <- triTweets %>% 
  replace_tag(triTweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(triTweets, pattern = "#([A-Za-z0-9_]+)",replacement="")      # remove hashtags

# Replace slang words
spell.lex <- read.csv("colloquial-indonesian-lexicon.csv")

smartfrenTweets <- replace_internet_slang(smartfrenTweets, slang = paste0("\\b",
                                                        spell.lex$slang, "\\b"),
                                 replacement = spell.lex$formal, ignore.case = TRUE)
telkomselTweets <- replace_internet_slang(telkomselTweets, slang = paste0("\\b",
                                                                          spell.lex$slang, "\\b"),
                                          replacement = spell.lex$formal, ignore.case = TRUE)
indosatTweets <- replace_internet_slang(indosatTweets, slang = paste0("\\b",
                                                                          spell.lex$slang, "\\b"),
                                          replacement = spell.lex$formal, ignore.case = TRUE)
xlTweets <- replace_internet_slang(xlTweets, slang = paste0("\\b",
                                                                      spell.lex$slang, "\\b"),
                                        replacement = spell.lex$formal, ignore.case = TRUE)
triTweets <- replace_internet_slang(triTweets, slang = paste0("\\b",
                                                            spell.lex$slang, "\\b"),
                                   replacement = spell.lex$formal, ignore.case = TRUE)

# Strip all of the non-relevant symbols and case
smartfrenTweets <- strip(smartfrenTweets)
telkomselTweets <- strip(telkomselTweets)
indosatTweets <- strip(indosatTweets)
xlTweets <- strip(xlTweets)
triTweets <- strip(triTweets)

# To see if the data has no duplicate
smartfrenTweets <- smartfrenTweets %>% 
  as.data.frame() %>% 
  distinct()
telkomselTweets <- telkomselTweets %>% 
  as.data.frame() %>% 
  distinct()
indosatTweets <- indosatTweets %>% 
  as.data.frame() %>% 
  distinct()
xlTweets <- xlTweets %>% 
  as.data.frame() %>% 
  distinct()
triTweets <- triTweets %>% 
  as.data.frame() %>% 
  distinct()

# Stemming the words into the base or root of its words (indonesian base words)
smartfrenTweets <- as.character(smartfrenTweets$.)
telkomselTweets <- as.character(telkomselTweets$.)
indosatTweets <- as.character(indosatTweets$.)
xlTweets <- as.character(xlTweets$.)
triTweets <- as.character(triTweets$.)

stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}

smartfrenTweets <- lapply(tokenize_words(smartfrenTweets[]), stemming)
telkomselTweets <- lapply(tokenize_words(telkomselTweets[]), stemming)
indosatTweets <- lapply(tokenize_words(indosatTweets[]), stemming)
xlTweets <- lapply(tokenize_words(xlTweets[]), stemming)
triTweets <- lapply(tokenize_words(triTweets[]), stemming)

# Tokenization/ breaking the sentences into words
smartfrenTweets <- tokenize_words(smartfrenTweets)
telkomselTweets <- tokenize_words(telkomselTweets)
indosatTweets <- tokenize_words(indosatTweets)
xlTweets <- tokenize_words(xlTweets)
triTweets <- tokenize_words(triTweets)

# Removing the stopwords, tokenize again, and transform to character
myStopwords <- readLines("stopword_list_id_2.txt")

smartfrenTweets <- as.character(smartfrenTweets)
telkomselTweets <- as.character(telkomselTweets)
indosatTweets <- as.character(indosatTweets)
xlTweets <- as.character(xlTweets)
triTweets <- as.character(triTweets)

smartfrenTweets <- tokenize_words(smartfrenTweets, stopwords = myStopwords)
telkomselTweets <- tokenize_words(telkomselTweets, stopwords = myStopwords)
indosatTweets <- tokenize_words(indosatTweets, stopwords = myStopwords)
xlTweets <- tokenize_words(xlTweets, stopwords = myStopwords)
triTweets <- tokenize_words(triTweets, stopwords = myStopwords)

smartfrenTweets <- as.character(smartfrenTweets)
telkomselTweets <- as.character(telkomselTweets)
indosatTweets <- as.character(indosatTweets)
xlTweets <- as.character(xlTweets)
triTweets <- as.character(triTweets)



##############################################
# Sentiment using Lexicon

# Importing all of the sentiment words (positive and negative words) in both Indonesian and English
# 1st Indonesian lexicon
opinion.lexicon.pos = scan("positive.txt", what = "character", comment.char = ";")
opinion.lexicon.neg = scan("negative.txt", what = "character", comment.char = ";")

# 2nd Indonesian lexicon
opinion.lexicon.pos2 = scan("positive_words_id.txt", what = "character", comment.char = ";")
opinion.lexicon.neg2 = scan("negative_words_id.txt", what = "character", comment.char = ";")

# English Lexicon
opinion.lexicon.pos.en = scan("positive-words-en.txt", what = "character", comment.char = ";")
opinion.lexicon.neg.en = scan("negative-words-en.txt", what = "character", comment.char = ";")

# Combine all of the lexicons (English and Indonesia)
pos.words = c(opinion.lexicon.pos, opinion.lexicon.pos2, opinion.lexicon.pos.en)
neg.words = c(opinion.lexicon.neg, opinion.lexicon.neg2, opinion.lexicon.neg.en)


# Words matching formula
getSentimentScore = function(sentences, pos.words, neg.words, .progress = "none")
  
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    #remove digit, punctuation, dan special/control character:
    
    sentence = gsub("[[:cntrl:]]", "", gsub("[[:punct:]]", "", gsub("\\d+", "", sentence)))
    
    #convert all text to lowercase:
    
    sentence = tolower(sentence)
    
    #separate all words with using space (space delimiter):
    
    words = unlist(str_split(sentence, "\\s+"))
    
    #Do the boolean match from each words using pos &amp;amp;amp; neg opinion-lexicon:
    
    pos.matches = !is.na(match(words, pos.words))
    neg.matches = !is.na(match(words, neg.words))
    
    #score sentimen = total positive sentiment - total negative:
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, pos.words, neg.words, .progress=.progress)
  
  #return data frame contains the sentence and the sentiment:
  return(data.frame(text = sentences, score = scores))
}

# Sentiment result from words matching
smartfrenResult = getSentimentScore(smartfrenTweets, pos.words, neg.words)
View(smartfrenResult)

telkomselResult = getSentimentScore(telkomselTweets, pos.words, neg.words)
View(telkomselResult)

indosatResult = getSentimentScore(indosatTweets, pos.words, neg.words)
View(indosatResult)

xlResult = getSentimentScore(xlTweets, pos.words, neg.words)
View(xlResult)

triResult = getSentimentScore(triTweets, pos.words, neg.words)
View(triResult)

# Adding polarities column
smartfrenResult$Polarity <-
  ifelse(smartfrenResult$score > 0, "Positive",
         ifelse(smartfrenResult$score < 0, "Negative",
                ifelse(smartfrenResult$score == 0, "Neutral", NA)
         )
  )

telkomselResult$Polarity <-
  ifelse(telkomselResult$score > 0, "Positive",
         ifelse(telkomselResult$score < 0, "Negative",
                ifelse(telkomselResult$score == 0, "Neutral", NA)
         )
  )

indosatResult$Polarity <-
  ifelse(indosatResult$score > 0, "Positive",
         ifelse(indosatResult$score < 0, "Negative",
                ifelse(indosatResult$score == 0, "Neutral", NA)
         )
  )

xlResult$Polarity <-
  ifelse(xlResult$score > 0, "Positive",
         ifelse(xlResult$score < 0, "Negative",
                ifelse(xlResult$score == 0, "Neutral", NA)
         )
  )

triResult$Polarity <-
  ifelse(triResult$score > 0, "Positive",
         ifelse(triResult$score < 0, "Negative",
                ifelse(triResult$score == 0, "Neutral", NA)
         )
  )

# Visualize the sentiment result
hist(smartfrenResult$score)
hist(telkomselResult$score)
hist(indosatResult$score)
hist(xlResult$score)
hist(triResult$score)

barplot(prop.table(table(smartfrenResult$Polarity)))
barplot(prop.table(table(telkomselResult$Polarity)))
barplot(prop.table(table(indosatResult$Polarity)))
barplot(prop.table(table(xlResult$Polarity)))
barplot(prop.table(table(triResult$Polarity)))

# Save the result to csv files
fwrite(smartfrenResult, "D:\\Smartfren Internship\\Sentiment Analysis Twitter Project\\smartfren_tweets_result_with_polarity.csv")
fwrite(telkomselResult, "D:\\Smartfren Internship\\Sentiment Analysis Twitter Project\\telkomsel_tweets_result_with_polarity.csv")
fwrite(indosatResult, "D:\\Smartfren Internship\\Sentiment Analysis Twitter Project\\indosat_tweets_result_with_polarity.csv")
fwrite(xlResult, "D:\\Smartfren Internship\\Sentiment Analysis Twitter Project\\xl_tweets_result_with_polarity.csv")
fwrite(triResult, "D:\\Smartfren Internship\\Sentiment Analysis Twitter Project\\tri_tweets_result_with_polarity.csv")
