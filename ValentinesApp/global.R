#### Global file for Valentine's ShinyApp.

##################

library(text2vec)
#This is for large corpuses too much for the system to handle.


library(qdap)
library(tidytext)
library(dplyr)
library(lubridate)
library(radarchart)
library(textstem)


#Needed on server.
library(wordcloud)
library(ggplot2)
library(directlabels)




###########################
library(data.table)

#Loading in needed data.
vdata <- fread('CleanTweets.csv')

#Taking sample of total data for shiny app purposes.
vdata <- sample_n(vdata, 200000)


#Limiting columns consuming memory.
vdata <- vdata[, c("created_at", "text", "time", "date")]

##########################



#### Creating stopword list for this project.
stop_words <- c(tm::removePunctuation(tm::stopwords("english")), "valentines", "day", "valentine")
add_stopwords <- c('get', 'just', 'will', 'amp', 'got', 'know', 'make', 'now', 'really', 'getting', 'us',
                   'u', 'let', 'via', 'come', 'th', 'can', 'ever', 'else', 'also', 'x', 'cause',
                   's', 'coming', 'days', 'even', 'tomorrow', 'es', 'ay', 'love', 'happy', 'like', 'going', 'want', 'one',
                   'go', 'good', 'see', 'year', 'gift', 'time', 'oh', 'yet', 'st', 'way', 'back', 'around', 'much', 'still',
                   'makes', 'say', 'hey', 'lot', 'ya', 'thursday')


#Combining stopword lists into one list.
total_stopwords <- union(stop_words, add_stopwords)



##############################################


########## Pre-Processing for app. ################
#This cuts down on time needed to navigate app by not performing these tasks in server file.

#Creating hour column.
vdata$hour = format(as.POSIXct(vdata$time,format="%H:%M:%S"),"%H")

#Tokenizing tweets.
tweet_tokens <- vdata$text %>%
  lemmatize_strings %>%
  word_tokenizer

tweet_tokens <- itoken(tweet_tokens)

#Creating initial vocaburaly for tweets.
terms <- create_vocabulary(tweet_tokens, ngram = c(ngram_min = 1L, ngram_max = 1L), stopwords = total_stopwords, sep = " ")

vectorizer <- vocab_vectorizer(terms)



#Creating DTM.
#      DTM showcases the frequency of each word in each document.
tweet_dtm <- create_dtm(tweet_tokens , vectorizer, type = "dgCMatrix")
tweet_dtm <- tidy(tweet_dtm)





#Creating vocabulary abd DTM for sentiment.
sentiterms <- create_vocabulary(tweet_tokens, ngram = c(ngram_min = 1L, ngram_max = 1L), stopwords = stop_words)
sentivect <- vocab_vectorizer(sentiterms)


senti_dtm <- create_dtm(tweet_tokens , sentivect, type = "dgCMatrix")
senti_dtm <- tidy(senti_dtm)





###############################################





# Calculating TF-IDF.
tweet_tfidf <- tweet_dtm
colnames(tweet_tfidf) <- c('document', 'term', 'frequency')

tweet_tfidf <- tweet_tfidf %>%
  group_by(document) %>%
  mutate(doc_length = sum(frequency))

tweet_tfidf$TF <- tweet_tfidf$frequency/tweet_tfidf$doc_length
tweet_tfidf <- inner_join(x = tweet_tfidf, y = terms[, c("term", "doc_count")], by = "term")

# Calculating IDF.
tweet_tfidf$IDF <- log(length(unique(tweet_tfidf[['document']]))/tweet_tfidf$doc_count)

#Creating tf-idf column.
tweet_tfidf$TF_IDF <- tweet_tfidf$TF * tweet_tfidf$IDF


#Creating final TF_IDF sum for each term.
tweet_tfidf <- tweet_tfidf %>%
  group_by(term) %>%
  summarize(weighted_count = sum(TF_IDF))




################################################




#Sentiment analysis.

# Designating lexicon that will be used.
lex <- lexicon::hash_sentiment_nrc

#Finding sentiment score for each document.
tweet_sentiments <- inner_join(senti_dtm, lex, by = c("column" = "x"))
tweet_sentiments$total_score <- tweet_sentiments$value * tweet_sentiments$y


#Summarizing sentiment for documents.
tweet_sentiments <- tweet_sentiments %>%
  group_by(row) %>%
  summarize(tweet_sentiment = sum(total_score))



#Adding sentiment score to tweet dataframe.
vsent <- merge.data.frame(vdata, tweet_sentiments, by.x = 0, by.y = 'row')

#Creating positive, negative column.
vsent$category <- ifelse(vsent$tweet_sentiment > 0, 'positive',
                         ifelse(vsent$tweet_sentiment < 0, 'negative', "neutral"))



# Daily grouped counts.
dailygroup <- vsent %>%
  group_by(date, category) %>%
  summarize(group_total = n())




#Creating dataframe for sentiment trend analysis.

#Getting hourly average sentiment by category.
categorysent <- vsent %>%
  group_by(date, hour, category) %>%
  summarize(avg_sentiment = mean(tweet_sentiment), tweet_count = n())

## Creating total daily tweet count.
categorysent <- categorysent %>%
  group_by(date) %>%
  mutate(total_tweets = sum(tweet_count))

#Creating category percentage.
categorysent$cat_pct <- categorysent$tweet_count/categorysent$total_tweets


#Creating sentiment category colors.
senticolors = c('positive' = 'darkred', 'negative' = 'slategrey', 'neutral' = 'violetred')




####################################



#### Using positive and negative tweets as categories.
#Commonality/Comparison Clouds.

positive <- vsent %>%
  filter(category == 'positive')

positive <- left_join(positive, tweet_dtm, by = c("Row.names" = "row"))

positive <- positive %>%
  group_by(column) %>%
  summarize(frequency = sum(value))



negative <- vsent %>%
  filter(category == 'negative')

negative <- left_join(negative, tweet_dtm, by = c("Row.names" = "row"))

negative <- negative %>%
  group_by(column) %>%
  summarize(frequency = sum(value))

colnames(positive) <- c("word", "Positive Tweets")
colnames(negative) <- c("word", "Negative Tweets")


pos_neg <- merge(positive, negative, all = TRUE)
pos_neg[is.na(pos_neg)] <- 0

row.names(pos_neg) <- pos_neg$word
pos_neg <- pos_neg[-1]




########################



#Emotion Analysis.
emotion_lex <- get_sentiments("nrc")

tweet_emotions <- inner_join(senti_dtm, emotion_lex, by = c('column' = 'word'))

#Removing positive and negative since we are trying to analyze emotions and not sentiments.
tweet_emotions <- tweet_emotions[!(tweet_emotions$sentiment %in% c("positive", "negative")),]

#Summarizing emotions by tweet.
tweet_emotions <- tweet_emotions %>%
  group_by(row, sentiment) %>%
  summarize(emotion_count = sum(value))

tweet_emotions <- merge(x = tweet_emotions, y = vsent[ , c("Row.names", "date")],
                         by.x = 'row', by.y = 'Row.names', all.x=FALSE)


#Looking at daily emotions.
daily_emotion <- tweet_emotions%>%
  group_by(date, sentiment) %>%
  summarize(emotion_count = n())

daily_emotion <- daily_emotion %>%
  group_by(date) %>%
  mutate(daily_total = sum(emotion_count))

daily_emotion$emo_pct <- daily_emotion$emotion_count/daily_emotion$daily_total*100

daily_emotion$y <- as.POSIXct(daily_emotion$date)


#Overall emotions.
overall_emotions <- tweet_emotions %>%
  group_by(sentiment) %>%
  summarize(emotion_count = sum(emotion_count))


#Emotion colors for trend line graph.
#Creating a color table to be used for team trend lines in plots.
emotioncolors = c('anger' = '#660000', 'anticipation' = '#FF0000', 'disgust' = '#CC3333', 'fear' = 'gray27',
           'joy' = '#990000', 'sadness' = 'darkred', 'surprise' = '#FF6666', 'trust' = 'tomato3')



#Generating random number for README tab.
tweetnum <- sample(nrow(vsent), 1)